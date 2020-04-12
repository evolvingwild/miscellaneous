## -------------------------------- ##
##   Scrape Deckbox.org Inventory   ##
## -------------------------------- ##

## Dependencies
library(RCurl); library(xml2); library(rvest)
library(dplyr)


## Required objects
pages_ <- 18                    ## number of pages of your inventory
deckbox_id <- your_deckbox_id   ## replace this with your deckbox.org id (found in the url)


## Scrape data using for loop
for(i in 1:pages_) { 
  
  print(i)
  
  ## Scrape data
  check <- rvest::html_table(
    rvest::html_nodes(
      xml2::read_html(paste0("https://deckbox.org/sets/", deckbox_id, "?p=", i)), 
      "#set_cards_table_details"
      )
    )
  
  ## Column names
  process_data <- check[[1]][-1, -c(1, 3)]
  colnames(process_data) <- c("name", "avg", "type")
  
  ## Short cleaning
  process_data <- process_data %>% 
    mutate(
      avg = gsub("\\$", "", avg) %>% as.numeric(.), 
      avg = ifelse(is.na(avg), 0, avg)
      )
  
  ## Bind
  if (i == 1) { 
    deckbox_cards <- process_data
    } else {
      deckbox_cards <- bind_rows(deckbox_cards, process_data)
      }
  
  Sys.sleep(0.5)
  
  }

rm(i, check, process_data)






