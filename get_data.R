library(rcrossref)
library(roadoi)
library(tidyverse)
library(gender)
library(data.table) # to read hmgu_publications

# Read data ---------------------------------------------------------------

## Read HMGU data

hmgu_publications <- fread(
  "./Data/hmgu_publications.csv",
  fill = TRUE)

# get all the dois

dois <- hmgu_publications$DOI
dois <- subset(dois, !(is.na(dois) | dois == ""))

## get crossref data
# save as RDS, so it only needs to run once

# dois_list <- split(dois, ceiling(seq_along(dois)/100)) # split in 100er chunk
#
# crossref_df <- data.frame()
# for(i in 1:length(dois_list)) {
#   print(paste("start round ", i))
#   start_time <- Sys.time()
#   
#   dois_list[[i]] %>% 
#       cr_works %>% 
#       pluck("data") -> crossref_df_i
#   crossref_df <- rbindlist(list(crossref_df, crossref_df_i), fill = TRUE)
#   
#   end_time <- Sys.time()
#   print(paste("finished round ", i, ", duration: ", end_time - start_time))
# }

crossref_df <- readRDS("./Data/crossref_df.RDS")

 
## get unpaywall data
# save as RDS, so it only needs to run once

# unpaywall_function <- function(dois) {
#   return(
#     tryCatch(
#       oadoi_fetch(
#         dois, 
#         email = "lea.schulz-vanheyden@helmholtz-muenchen.de"),
#       error = function(e) NULL))
# }
# 
# dois %>%
#   lapply(unpaywall_function) %>% 
#   rbindlist() -> unpaywall_df

unpaywall_df <- readRDS("./Data/unpaywall_df.RDS")

## clean up hmgu data

# delete entries with no dois (and rename the variable)

hmgu_df <- subset(hmgu_publications,
                  hmgu_publications$DOI %in% dois) %>% 
            filter(DOI != "undefined") %>% 
            mutate(doi = DOI)

# problems: hmgu some letters capital, for crossref and unpaywall everything
# lowercase, sometimes more than one doi (keep only first), sometimes empty
# space, one time a typing mistake

hmgu_df$doi <- str_to_lower(hmgu_df$doi)
hmgu_df$doi <- str_replace(hmgu_df$doi, "; [:graph:]+$", "")
hmgu_df$doi <- str_replace(hmgu_df$doi, "[:space:]+(?=$)", "")
hmgu_df[doi == "t10.1017/s0029665120000336"]$doi <- "10.1017/s0029665120000336"

# to check the problems:
# setdiff(crossref_df$doi, hmgu_df$doi)
# crossref_df[doi == "10.1017/s0029665120000336"]
# hmgu_df[ISSN == "0029-6651"]

## join the datasets

data <- full_join(hmgu_df, unpaywall_df, by = "doi")

# manually remove duplicate rows
duplicates <- data[duplicated(data$doi) | duplicated(data$doi, fromLast = TRUE),]
# View(duplicates)
data <- filter(data, !doi %in% c("10.15252/embj.2020105696",
                                 "10.15252/emmm.201809466",
                                 "10.1007/s00066-016-1087-4",
                                 "10.2337/dbi17-0029")) %>% 
  rbind(duplicates[c(1,5,9,13)])

data <- full_join(data, crossref_df, by = "doi") %>%
        distinct()





# Get additional variables from given -------------------------------------

## first author and surname of first author (for gender)

# input:  df: data frame
# output: name of the first author
author_list <- data$author
surnames <- lapply(author_list, function(df) {
  name <- subset(df,
                 df$sequence == "first")$given # get first author
  if (!any(df$sequence == "first")) name <- df$given # no first
  if (length(name) > 1) name <- name[1] # if many, give first
  return(name)
}) 
# warnings can be ignored 

surnames[sapply(surnames, is.null)] <- NA
data$surname <- unlist(surnames)
# get middle name if there
data$middle_name <- str_extract(data$surname,
                                "(?<=\\s)[:alpha:]+")
# get first name
data$first_name <- str_extract(data$surname,
                               "^[:alpha:]+(?=([^[:alpha:]]|$))")

# input:  df: data frame
# output: name of the first author
first_authors <- lapply(author_list, function(df) {
  surname <- subset(df,
                       df$sequence == "first")$given # get first author
  if (!any(df$sequence == "first")) surname <- df$given # no first
  if (length(surname) > 1) surname <- surname[1] # if many, give first
  family_name <- subset(df,
                        df$sequence == "first")$family # get first author
  if (!any(df$sequence == "first")) family_name <- df$family # no first
  if (length(family_name) > 1) family_name <- family_name[1] # if many, give first
  name <- paste(surname, family_name, " ")
  return(name)
})
# warnings can be ignored 

first_authors[sapply(first_authors, is.null)] <- NA
data$first_author <- unlist(first_authors)
# delete the empty space at end
data$first_author <- str_replace(data$first_author,
                                 "\\s{2}", "")

## get gender
# if not possible from 1. surname take 2.

# save as RDS, because it takes time to run

# data$gender <- "unknown"
# for(i in 1:nrow(data)){
#   gender <- gender(data[i,]$first_name)$gender
#   if(!is.character(gender)) {
#     gender <- gender(data[i,]$middle_name)$gender # gives us 42 more
#   }
#   if(!is.character(gender)) gender <- NA
#   data[i,]$gender <- gender
# }

gender <- readRDS("./Data/gender.RDS")

data <- right_join(data, gender, by = "doi")

## get h-index

# scraping takes place in file scraping_semantic.R, it uses the dois list and
# gives a h-index list

saveRDS(dois, file = "./Data/dois.RDS")

# after scraping load and join the completed list to data

h_index_list <- read.csv2("./Data/h_index.txt",
                     col.names = c("doi", "h_index"), encoding = "utf-8")
h_index_list %>%
  right_join(data, by = "doi") -> data

## length of article from page numbers

# we have 2 variables that have the pages, calculate length for both and if one
# is missing take the other

page_start1 <- as.numeric(str_extract(data$page, "(?<=^[:alpha:]?)\\d+(?=-)"))
page_end1 <- as.numeric(str_extract(data$page, "(?<=-[:alpha:]?)\\d+$"))
length1 <- page_end1 - page_start1 + 1

page_start2 <- as.numeric(str_extract(data$Pages, "(?<=^[:alpha:]?)\\d+(?=-)"))
page_end2 <- as.numeric(str_extract(data$Pages, "(?<=-[:alpha:]?)\\d+$"))
length2 <- page_end2 - page_start2 + 1

length <- cbind(length1, length2)

length %>% 
  data.frame() %>% 
  mutate("length" = ifelse(is.na(length1) & is.na(length2), NA, # both NA
                    ifelse(is.na(length1), length2,  # only length2 has a value
                    ifelse(is.na(length2), length1,  # only length1 has a value
                    ifelse(length1 == length2, length1, # both are the same value
                           NA # if not the same set to NA
                           ))))) -> length

# add to data

data %>% 
  cbind("length" = length$length) -> data

# delete length for Journal ERJ Open Res. (because it is date instead of pages)
data[data$Journal == "ERJ Open Res.", "length"] <- NA
# delete false negative lengths
data[data$length <= 0 & !is.na(data$length), "length"] <- NA


## date of oa from best oa location
oa_date <- data %>% 
  unnest(cols = best_oa_location, names_repair = "universal") %>%
  select(oa_date, doi) %>% 
  ungroup()

data <- data %>% 
  left_join(oa_date, by = "doi")

## get journal classification, completed manually (see get_journal_subject.R)

complete_journals <- read.csv("./Data/complete_journals.csv",
                              sep = ",", header = TRUE, encoding = "utf-8")

# join together with data
complete_journals %>% 
  right_join(data, by = "journal_name") -> data


# Save data ---------------------------------------------------------------

saveRDS(data, "./Data/data_complete.RDS")
