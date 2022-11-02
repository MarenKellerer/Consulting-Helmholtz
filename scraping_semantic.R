library(tidyverse)
library(stringr)
library(httr)
library(rvest)
library(jsonlite)


# functions ---------------------------------------------------------------

get_paper_url <- function(doi) { # get right url from doi
  url <- paste0("https://api.semanticscholar.org/v1/paper/", doi)
}

get_author_url <- function(url) { # get url for first author
  
  tryCatch({page <- GET(url)}, finally = return("3333"))

    
    # if request was unsuccessful return 1111
    status <- page$status_code
    if (status != 200) return("1111")
    
    data <- fromJSON(rawToChar(page$content))
    author_id <- data$authors[1, ]$authorId
    author_url <- data$authors[1, ]$url
    
    # if url is missing return 2222
    if (is.na(author_url)) return("2222")
    
    return(author_url)
}

get_h_index <- function(author_url){ # get h-index from author page
  
    page <- read_html(author_url)
    h_index <- html_elements(page, ".author-detail-card__stats-row__value") %>% 
                  html_text() %>% 
                  nth(2) %>% 
                  as.numeric()
    
    return(h_index)
}

# scrape ------------------------------------------------------------------

dois <- readRDS("./Data/dois.RDS") # get dois

for (doi in dois) {

  url <- get_paper_url(doi)
  
  author_url <- get_author_url(url)
  
  # check if first author has an url
  if (author_url == "1111") {
    h_index <- 1111
  } else if (author_url == "2222") {
    h_index <- 2222
  } else if (author_url == "3333") {
    h_index <- 3333
  } else {
    h_index <- get_h_index(author_url)
  }
  
  output <- paste(doi, h_index, sep = ";")
  data.table::fwrite(list(output),
                     file = "./Data/h_index.txt",
                     sep = "\n", append = T)
  # sleep
  Sys.sleep(sample(1, 1:5))

}
