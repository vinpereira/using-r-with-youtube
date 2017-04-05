# It is required to install these libraries if it is the first time using them
# to install use 'install.packages("library_name")'
library(tuber)
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(tidyr)

# Get access to the Youtube API
yt_oauth(app_id = "put your app ID from Youtube API", app_secret = "put your app secret from Youtube API")

# Get all videos from a channel
# You can use this code 'yt_search(term = "Channel name")' to find the channel ID
get_videos_from_channel <- function(dates) {
  yt_search(term = "",
            type = "video",
            channel_id = "put the channel ID",
            published_after = dates$start,
            published_before = dates$end)
}

# Set the date range (I used January 2012 to December 2017)
dates <- tibble(start = seq(ymd("2012-01-01"), ymd("2017-01-01"), by = "years"), end = seq(ymd("2012-12-31"), ymd("2017-12-31"), by = "years")) %>%
         mutate(start = paste(start, "T0:00:00Z", sep = ""),end = paste(end, "T0:00:00Z", sep = ""))

# Get all channel videos in the date range
videos <- by_row(.d = dates, ..f = get_videos_from_channel, .to = "videos_info")

# Get the stats for each video
get_videos_stats <- function(df_row) {
  get_stats(video_id = df_row$video_id)
}

# Mount a collection of data from all videos
videos_data <- bind_rows(videos$videos_info) %>%
               select(title, publishedAt, video_id) %>%
               by_row(..f = get_videos_stats, .to = "videos_stats")

# EACH new graph will replace the old one
# Plot a graph (using videos_data) Publication Date X Views
videos_data %>%
  mutate(views = map(videos_stats, .f = 'viewCount')) %>%
  unnest(views) %>%
  mutate(views = as.numeric(views),
  publishedAt = as_date(publishedAt)) %>%
  ggplot(aes(x = publishedAt, y = views)) +
  geom_line(aes(y = 1000000, colour = "1 Million")) +
  geom_line(aes(y = 10000000, colour = '10 Millions')) +
  geom_line(aes(y = 20000000, colour = '20 Millions')) +
  geom_line() +
  labs(x = "Publication Date", y = "Views") +
  theme_bw()

# Plot a graph (using videos_data) Publication Date X Likes/Dislikes
videos_data %>%
  mutate(likes = map(videos_stats, .f = 'likeCount'),
  dislikes = map(videos_stats, .f = 'dislikeCount')) %>%
  unnest(likes, dislikes) %>%
  mutate(likes = as.numeric(likes),
  dislikes = as.numeric(dislikes),
  publishedAt = as_date(publishedAt),
  prop = likes/dislikes) %>%
  ggplot(aes(x = publishedAt)) +
  geom_line(aes(y = prop)) +
  labs(x = "Publication Date", y = "Likes/Dislikes") +
  theme_bw()
