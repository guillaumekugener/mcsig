library(tidyverse)
library(ggrepel)
library(magrittr)
library(reshape2)
library(rjson)
library(lubridate)
library(DT)

groupme_dir <- '~/Downloads/52315419 3/'
messages <- fromJSON(paste(readLines(file.path(groupme_dir, 'message.json')), collapse=""))

# Make a table that is: Date, Sender, Likes, Text
final_table_all_messages <- NULL
for (m in messages) {
  
  if (is.null(m$text)) {
    m$text <- 'attachment'
  }
  
  final_table_all_messages %<>% rbind(
    data.frame(
      name = m$name, 
      created = m$created_at, 
      likes = length(m$favorited_by), 
      text = m$text,
      stringsAsFactors = F
    ))
}

final_table_all_messages %<>% mutate(d=with_tz(as_datetime(created), 'America/Los_Angeles'))

final_table_all_messages %>% datatable()

# Overview of number of posts by day
overview_by_day <- final_table_all_messages %>%
  mutate(day=round_date(d, 'day')) %>%
  group_by(day) %>%
  dplyr::summarise(count=n(), tot_likes=sum(likes))

ggplot(overview_by_day, aes(day, count)) +
  geom_line() +
  geom_text(data = overview_by_day %>% filter(count > 200), aes(label=day), nudge_x = 10, nudge_y = 10)

final_table_all_messages %>%
  mutate(day=round_date(d, 'day')) %>% 
  filter(day == round_date(ymd('20190830', tz = 'America/Los_Angeles'))) %>%
  datatable()

# Count the total of posts and likes per person
likes_total <- final_table_all_messages %>%
  group_by(name) %>%
  dplyr::summarise(number_of_posts=n(), total_likes=sum(likes)) %>%
  mutate(ratio=total_likes/number_of_posts)

ggplot(likes_total, aes(number_of_posts, total_likes)) +
  geom_point() +
  geom_text_repel(data=likes_total %>% filter(number_of_posts > 100), aes(label=name))

