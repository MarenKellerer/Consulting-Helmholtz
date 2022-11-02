library(tidyverse)
library(readr)
library(plyr)


# read classifications given by scopus

scopus_data <- read_delim("./Data/Scopus_data.CSV", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

# read data to get relevant journals

data <- readRDS("./Data/data_clean.RDS")


# Get Classification ------------------------------------------------------

# get only columns we are interested in and only rough classification (first 2
# digits), use unique to get rid of double mentions

scopus_data <- scopus_data %>% 
  transmute(journal_name = Title, # rename to be able to join later
            ASJC = scopus_data$`Scopus ASJC Code (Sub-subject Area)`) %>% 
  mutate(ASJC = as.numeric(substr(ASJC, 1, 2))) %>% 
  unique()

# classification into 4 big areas (https://bit.ly/2S7DSbN) and multidisciplinary

physical_sciences <- c(15, 16, 17, 19, 21, 23, 25, 26, 31)
health_sciences <- c(27, 29, 34, 35, 36)
social_sciences <- c(12, 14, 18, 20, 32, 33)
life_sciences <- c(11, 13, 24, 28, 30)

# Assigning the categories to the journals
scopus_data <- scopus_data %>% 
  mutate(journal_subject = ifelse(ASJC %in% physical_sciences, "physical",
                           ifelse(ASJC %in% health_sciences, "health",
                           ifelse(ASJC %in% social_sciences, "social",
                           ifelse(ASJC %in% life_sciences, "life",
                           "multidisciplinary"))))) %>% 
  select(-ASJC) # keep only big categories

# Keep only journals we need ----------------------------------------------

# journals we later use
journals_data <- data$journal_name %>% 
  unique() %>% 
  as.character()

# scopus data of journals we need
scopus_used <- subset(scopus_data, journal_name %in% journals_data)

# Assign only one category ------------------------------------------------

# Counting the occurrence of the smaller subcategories belonging to the main
# category. If they do not occur equally often: choose the one that is mentioned
# more often

scopus_used <- ddply(scopus_used,.(journal_name, journal_subject), nrow) %>% 
  group_by(journal_name) %>% top_n(1, V1) %>% 
  select(-V1)

# duplicates: TRUE if there is more than one category (and not one more often)

scopus_used <- scopus_used %>%  mutate(duplicates_1 = 
                    duplicated(scopus_used$journal_name, fromLast = FALSE)) %>% 
  mutate(duplicates_2 = 
                    duplicated(scopus_used$journal_name, fromLast = TRUE)) %>% 
  mutate(duplicates =  as.logical(duplicates_1 + duplicates_2)) %>% 
  select(c(-duplicates_1, -duplicates_2))

# if journal has more than one subject, but it's multidisciplinary, take other

remove_mult <- subset(scopus_used, 
                        scopus_used$duplicates == TRUE & 
                        scopus_used$journal_subject == "multidisciplinary")
scopus_used <- anti_join(scopus_used, remove_mult, by = "journal_name") 

# find the ones that still have more than one subject
scopus_used <- scopus_used %>%  mutate(duplicates_1 = 
                  duplicated(scopus_used$journal_name, fromLast = FALSE)) %>% 
  mutate(duplicates_2 = 
           duplicated(scopus_used$journal_name, fromLast = TRUE)) %>% 
  mutate(duplicates =  as.logical(duplicates_1 + duplicates_2)) %>% 
  select(c(-duplicates_1, -duplicates_2))

# make df with journals with clear subject
scopus_compl <- subset(scopus_used, duplicates == FALSE) %>% 
  select(-duplicates)

# Manually add category for journals without ------------------------------

# create excel sheet to manually assign missing journals subject (or if all the
# subjects occur the same decide what the subject will be)

# journals that need to be assigned
journals_na <- setdiff(journals_data, scopus_compl$journal_name) %>% 
  as.data.frame()

write_csv(journals_na,"./Data/journals_na.csv")


# import excel of journals with manual categories
journals_na_filled <- read_csv("./Data/journals_na_filled.csv")

# combine journals_na_filled with scopus_compl and save as excel data
complete_journals <- rbind(journals_na_filled, scopus_compl)
write_csv(complete_journals,"./Data/complete_journals.csv")
