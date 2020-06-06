## --------------------------- ##
##   Scrape GroupMe Messages   ##
## --------------------------- ##

## Dependencies
library(jsonlite)
library(tidyverse)

## Required objects (your api token and group ID):
api_token <- your_api_token  ## Your GroupMe API Token can be found here: https://dev.groupme.com/
group_id <-  your_group_id   ## The specific group_id can be found by accessing the /messages endpoint and finding the group ID


## Determine total runs needed (accessing messages is limited to 100 at a time)
mess_count <- jsonlite::fromJSON(
  paste0(
    "https://api.groupme.com/v3/groups/", 
    group_id, 
    "/messages?token=", 
    api_token
    )
  )
runs <- ceiling(mess_count$response$count / 100); print(runs)


## System sleep
sleep_ <- 1


## --------------------------------- ##
##   Scrape GroupMe Using For Loop   ##
## --------------------------------- ##

for (i in 1:runs) {
  
  ## Determine before_id to pass to url
  if (i == 1) { 
    oldest_id <- NULL
    } else {
      oldest_id <- messages_data$response$messages$id[nrow(messages_data$response$messages)]
      }
  
  
  ## Create url to scrape
  loop_url <- paste0(
    "https://api.groupme.com/v3/groups/", 
    group_id, 
    "/messages?token=", 
    api_token, 
    "&before_id=", 
    oldest_id, 
    "&limit=100"
    )
  
  
  ## Scrape & parse JSON
  messages_data <- jsonlite::fromJSON(loop_url)
  
  
  ## Deal with missing `event` nested data frame (results in an integer(0) object)
  if (identical(which(names(messages_data$response$messages) == "event"), integer(0))) { 
    loop_data_new <- messages_data$response$messages
    } else { 
      loop_data_new <- messages_data$response$messages[, -which(names(messages_data$response$messages) == "event")]
      }
  
  
  ## Bind
  if (i == 1) {
    loop_data_joined <- loop_data_new
    } else { 
      loop_data_joined <- bind_rows(loop_data_joined, loop_data_new)
      }
  
  
  if (i == 1) cat("Processing", runs, "GET Requests...", "\n", "-----------------------------", "\n")
  
  cat("run:", i, "\n")
  Sys.sleep(sleep_)
  
  }



## --------------------------- ##
##   Modify Basic User Stats   ##
## --------------------------- ##

## Transfer scraped data to separate data frame
loop_data_final <- loop_data_joined %>% 
  arrange(created_at)


## Add total like count for each message
loop_data_final$likes_received <- unlist(
  lapply(
    loop_data_final$favorited_by, 
    function(x) length(x)
    )
  )

loop_data_final$favorited_by <- unlist(
  lapply(
    loop_data_final$favorited_by, 
    function(x) paste0(x, collapse = ", ")
    )
  )

## Add message attributes column
loop_data_final$message_type <- unlist(
  lapply(
    loop_data_final$attachments, 
    function(x) ifelse(length(x) > 0, x$type, NA)
    )
  )

## Extract url if message contained an image
loop_data_final$image_url <- unlist(
  lapply(
    loop_data_final$attachments, 
    function(x) ifelse(length(x) > 0 & ("image" %in% x$type | "linked_image" %in% x$type), x$url, NA)
    )
  )

## Extract users mentioned in message
loop_data_final$users_mentioned <- unlist(
  lapply(
    loop_data_final$attachments, 
    function(x) ifelse(length(x) > 0 & "mentions" %in% x$type, paste0(unlist(x$user_ids), collapse = ", "), NA)
    )
  )

## Add additional information that might be useful / cleanup
loop_data_final <- loop_data_final %>% 
  mutate(
    ## Handle dates
    date_sent = as.POSIXct(created_at, origin = "1970-01-01"), 
    date = lubridate::date(date_sent), 
    time = strftime(date_sent, format = "%H:%M:%S"), 
    ## Determine message type
    message_type = case_when(
      message_type == "location" ~ "location", 
      message_type == "video" ~ "video", 
      message_type == "postprocessing" ~ "unknown", 
      user_id == "system" | user_id == "calendar" ~ "system", 
      message_type == "mentions" | !is.na(users_mentioned) ~ "mentions", 
      grepl(".gif", image_url) ~ "gif", 
      grepl(".png|.jpeg|", image_url) ~ "image", 
      is.na(message_type) & is.na(image_url) ~ "text", 
      grepl("Created new poll", text) ~ "poll", 
      message_type == "emoji" ~ "text", 
      grepl("created event", text) ~ "event", 
      TRUE ~ NA_character_
      ), 
    favorited_by = ifelse(favorited_by == "", NA, favorited_by), 
    website_linked = ifelse(grepl("https://|http://", text), stringr::str_extract(text, "(http:\\/\\/|https:\\/\\/)+[a-zA-Z0-9-\\.]+\\/+"), NA), 
    ## Convert columns for SQL compatibility
    created_at = as.character(created_at)
    ) %>% 
  select(
    group_id, platform, message_id = id, created_at, date, time, 
    message_type, user_id, sender_id, name, text, likes_received, liked_by = favorited_by, users_mentioned, image_url, website_linked, 
    sender_type, source_guid, system, 
    attachments, 
    system, avatar_url
    ) %>% 
  arrange(created_at) %>% 
  data.frame()


## Data removing non-sql compliant columns (for reference)
groupme_messages <- loop_data_final %>% 
  select(-attachments)





## ---------------------- ##
##   Create Summed Data   ##
## ---------------------- ##

## Sum total messages sent from users to the group
user_sum_messages <- loop_data_final %>% 
  filter(!is.na(user_id)) %>% 
  mutate(messages_sent = 1) %>% 
  group_by(user_id) %>% 
  mutate(user_names = paste0(unique(name), collapse = " / ")) %>% 
  group_by(user_id, user_names) %>% 
  summarise(messages_sent = sum(messages_sent)) %>% 
  data.frame()


## Sum total likes a user has given to messages in the group
user_sum_likes_given <- data.frame(
  user_id = unlist(loop_data_final$favorited_by), 
  stringsAsFactors = FALSE
  ) %>% 
  mutate(likes_given = 1) %>% 
  group_by(user_id) %>% 
  summarise(likes_given = sum(likes_given)) %>% 
  data.frame()


## Sum total likes a user has received for messages they sent in the group
user_sum_likes_received <- loop_data_final %>% 
  group_by(user_id) %>% 
  summarise(likes_received = sum(likes_received)) %>% 
  data.frame()


## Join all above dataframes
user_messages_compiled <- user_sum_messages %>% 
  left_join(
    user_sum_likes_given, 
    by = "user_id"
    ) %>% 
  left_join(
    user_sum_likes_received, 
    by = "user_id"
    ) %>% 
  mutate(likes_per_message = round(likes_received / messages_sent, 2)) %>% 
  arrange(desc(messages_sent)) %>% 
  filter(messages_sent > 1000)  ## quick message cutoff 



## Save data as .rds
# saveRDS(loop_data_final, "data/groupme_scrape_2020-06-03.rds")




