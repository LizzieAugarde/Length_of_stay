library(NDRSAfunctions)
library(tidyverse)
library(janitor)

casref01 <- createConnection()

los2014_op_events_2014_query <- "
SELECT a.tumourid, 
       a.patientid, 
       a.follow_up_start, 
       a.follow_up_end, 
       a.deathdatebest, 
       c.attendkeyanon,
       c.datayear,
       c.apptdate,
       c.attended,
       c.stafftyp,
       c.tretspef
FROM analysiselizabethaugarde.los2014_cohort a 
LEFT JOIN heslive.hes_linkage_av_op@casref01 b ON a.patientid = b.patientid 
LEFT JOIN heslive.hesop@casref01 c ON b.attendkeyanon = c.attendkeyanon AND b.datayear = c.datayear
WHERE (c.datayear > 2013 AND c.datayear < 2015)
AND a.patientid IS NOT NULL
"

# Fetch data
los2014_op_events_2014 <- dbGetQueryOracle(casref01, los2014_op_events_2014_query, rowlimit = NA)

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