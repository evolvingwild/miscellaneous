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
runs <- ceiling(mess_count$response$count / 100)


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
    function(x) ifelse(length(x) > 0 & "image" %in% x$type, x$url, NA)
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
    date_sent = as.POSIXct(created_at, origin = "1970-01-01"), 
    message_type = case_when(
      message_type == "mentions" | !is.na(users_mentioned) ~ "mentions", 
      grepl(".gif", image_url) ~ "gif", 
      grepl(".png|.jpeg|", image_url) ~ "image", 
      is.na(message_type) & is.na(image_url) ~ "text", 
      grepl("Created new poll", text) ~ "poll", 
      message_type == "emoji" ~ "text", 
      TRUE ~ NA_character_
      ), 
    favorited_by = ifelse(likes_received == 0, NA, favorited_by)
    ) %>% 
  select(
    group_id, platform, id, created_at, date_sent, 
    message_type, user_id, sender_id, name, text, likes_received, favorited_by, users_mentioned, image_url, 
    sender_type, source_guid, system, attachments, system, avatar_url
    ) %>% 
  data.frame()




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


## Sum total links a user has submitted to the group
user_sum_links_posted <- loop_data_final %>% 
  filter(grepl("https://|http://", text)) %>% 
  mutate(url_check = stringr::str_extract(text, "(http:\\/\\/|https:\\/\\/)+[a-zA-Z\\.]*\\.+[a-z]+\\/+")) %>% 
  filter(!is.na(url_check)) %>% 
  mutate(link_count = 1) %>% 
  group_by(user_id) %>% 
  summarise(link_count = sum(link_count)) %>% 
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
  left_join(
    user_sum_links_posted, 
    by = "user_id"
    ) %>% 
  mutate(
    likes_per_message = round(likes_received / messages_sent, 2), 
    link_count = ifelse(is.na(link_count), 0, link_count), 
    link_perc = round(100 * link_count / messages_sent, 2)
    ) %>% 
  select(-c(link_count, link_perc)) %>%  ## removing for display purposes
  filter(messages_sent > 10)             ## quick message cutoff 







