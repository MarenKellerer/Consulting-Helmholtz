# load packages
library(tidyverse)
library(stringr) # to replace missing days

# load data

data <- readRDS("./Data/data_complete.RDS")

## keep only journal articles

table(data$`Publication type`, useNA = "always")

data <- subset(data, data$`Publication type` == "Artikel: Journalartikel")

## keep only english articles

table(data$language, useNA = "always")

data <- subset(data, data$language == "en" | is.na(data$language)) #keep the NAs

# from NAs manually delete the non english papers

not_english <- c("10.3238/arztebl.2019.0521", "10.3238/arztebl.2019.0736b",
                 "10.25646/5982", "10.15134/2018m0001", "10.5675/hywa_2018,6_1",
                 "10.14627/537647050", "10.18420/in2017_193", 
                 "10.3243/kwe2017.05.001", "10.1055/s-0037-1605651",
                 "10.1055/s-0037-1605817", "10.3238/arztebl.2017.0815")

data <- data[!data$doi %in% not_english, ]


# if day is missing for published.print or published.online set to 15th
# what if month also missing? then the date is set to NA

# input:  date_vec: vector of the published date
#         replacement_day: numeric value to replace the date; default is 15
# ouput:  data frame with complete date 
complete_date <- function(date_vec, replacement_day = 15) {
  
  # split into year, month and day
  year <- str_extract(date_vec, "^\\d{4}")
  month <- str_extract(date_vec, "(?<=\\d{4}-)\\d{2}")
  day <- str_extract(date_vec, "(?<=-\\d{2}-)\\d{2}")

  # set all day NAs to replacement_day
  day[is.na(day)] <- replacement_day
  
  # add back together
  date_complete <- str_c(year, month, day, sep = "-") # if no month -> no year
  
  return(date_complete)
}

data$published.online <- complete_date(data$published.online, 15)
data$published.print <- complete_date(data$published.print, 15)

## delete rows with data on oa missing

data <- subset(data, !is.na(is_oa))

### make right types

data <- data %>% 
  mutate(gender = as.factor(gender), 
         first_author = as.factor(first_author), 
         journal_name = as.factor(journal_name),
         journal_subject = as.factor(journal_subject),
         `Publication type` = as.factor(`Publication type`),
         `Document type` = as.factor(`Document type`),
         references.count = as.integer(references.count),
         is.referenced.by.count = as.integer(is.referenced.by.count),
         oa_status = as.factor(oa_status),
         published.online = as.Date(published.online, format = "%Y-%m-%d"),
         published.print = as.Date(published.print, format = "%Y-%m-%d"),
# add published_earliest
         published_earliest = pmin(published.print, published.online, na.rm = T))

### Manually delete wrong journals

journal_names_delete <- c("Drinking", "Poster Presentations", "ePoster",
                          "Oral presentations", "Invited Speaker Abstracts",
                          "Poster Presentations - Proffered Abstracts")

data <- data[!data$journal_name %in% journal_names_delete,]

### keep only variables of interest

data <- data %>% 
  transmute(doi = doi,
           impact_factor = `Impact Factor`,
           references_count = references.count,
           citation_count = is.referenced.by.count,
           length = length,
           gender = gender,
           h_index = h_index,
           published_earliest = published_earliest,
           is_oa = is_oa,
           oa_status = oa_status,
           journal_name = journal_name,
           journal_subject = journal_subject)  %>% 
  # sort by doi
  arrange(doi) %>% 
  # data is grouped
  ungroup()

# set right order of oa_status levels
data$oa_status <-  factor(data$oa_status,c("closed", "bronze", "hybrid",
                                           "gold", "green"))

# make multidiscpl reference cat for journal_subject
data <- within(data, journal_subject <- relevel(journal_subject,
                                                ref = "multidisciplinary"))

### calculate time since publication
# if there was no day we set it to 15, so weeks could be wrong by max 2

time_cit_coll <- as.Date("20.01.2021", format = "%d.%m.%Y") # day we got data

data$time_since_pub <- as.numeric(time_cit_coll - data$published_earliest)

# delete papers that have published earliest after 2020

data <- subset(data, data$published_earliest < as.Date("01.01.2021", 
                                                       format = "%d.%m.%Y"))

###### manual cleaning

data[data$doi == "10.1002/eji.201970107", "references_count"] <- 945

### save

saveRDS(data, "./Data/data_clean.RDS")

