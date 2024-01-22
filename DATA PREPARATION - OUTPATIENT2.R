
library(janitor)
library(tidyverse)

los2014_op_events_processed <- los2014_op_events %>%
  clean_names() %>%
  mutate(follow_up_start = as.Date(follow_up_start),
         as.Date(follow_up_start),
         as.Date(follow_up_start),
         as.Date(follow_up_start))

#check for any deaths after follow up start
#check and remove appointments before follow_up_start
los2014_op_events_processed <- los2014_op_events_processed %>% 
  mutate(date_comp = interval(FOLLOW_UP_START, APPTDATE) / days(1))

nrow(los2014_op_events_processed[los2014_op_events_processed$date_comp < 1 & !is.na(los2014_op_events_processed$date_comp), ])

los2014_op_events_processed <- los2014_op_events_processed %>% 
  filter(!date_comp < 1)

#check and remove appointments after follow_up_end
los2014_op_events_processed <- los2014_op_events_processed %>% 
  mutate(date_comp = interval(APPTDATE, FOLLOW_UP_END) / days(1))

nrow(los2014_op_events_processed[los2014_op_events_processed$date_comp < 1 & !is.na(los2014_op_events_processed$date_comp), ])

los2014_op_events_processed <- los2014_op_events_processed %>% 
  filter(!date_comp < 1)

look up bigmemory package
