###################### Length of stay 2023######################

#Script to process linked A&E data/registry data from HES (A&E events 
#in first 5 years post diagnosis from AT_PATHWAY linked on TUMOURID).
#Basic aggregation of all A&E data for all diagnoses, for assessing 
#rough impact of 2020 on the data

#Created December 2023 by Lizzie Augarde 
#Change log:
############################################################### 

#prep
library(tidyverse)
library(janitor)
library(NDRSAfunctions)

casref01 <- createConnection()

#pulling from SQL
los2014_ae_events_raw <- dbGetQueryOracle(casref01, "select los2014_cohort.tumourid, 
los2014_cohort.patientid, 
los2014_cohort.follow_up_start, 
los2014_cohort.follow_up_end, 
los2014_cohort.deathdatebest, 
event_date,
round(months_between(event_date, follow_up_start), 1) as months_post_diag
from los2014_cohort inner join analysisncr.at_pathway@cas2309
on los2014_cohort.tumourid = analysisncr.at_pathway.tumourid and analysisncr.at_pathway.event_type = 5 --identifying A&E attendance events
and analysisncr.at_pathway.event_date between follow_up_start and follow_up_end", rowlimit = NA)

#clean  
los2014_ae_events_raw <- los2014_ae_events_raw %>%
  clean_names() %>% mutate(diag_year = 2014)

los2015_ae_events_raw <- los2015_ae_events_raw %>%
  clean_names() %>% mutate(diag_year = 2015)

los2016_ae_events_raw <- los2016_ae_events_raw %>%
  clean_names() %>% mutate(diag_year = 2016)

#by month post_diagnosis
los_ae_events_by_month <- rbind(los2014_ae_events_raw, los2015_ae_events_raw, los2016_ae_events_raw) %>%
  group_by(diag_year, months_post_diag) %>%
  summarise(attend_count = n())

ggplot(los_ae_events_by_month, aes(x = months_post_diag, y = attend_count)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~diag_year, nrow = 1)

#by years post-diagnosis
los_ae_events_by_year <- rbind(los2014_ae_events_raw, los2015_ae_events_raw, los2016_ae_events_raw) %>%
  mutate(years_post_diag = case_when(months_post_diag <= 12 ~ paste0("Less than 1 year\n(", diag_year, "-", diag_year+1, ")"),
                                     months_post_diag > 12 & months_post_diag <= 24 ~ paste0("1-2 years\n(", diag_year+1, "-", diag_year+2, ")"),
                                     months_post_diag > 24 & months_post_diag <= 36 ~ paste0("2-3 years\n(", diag_year+2, "-", diag_year+3, ")"),
                                     months_post_diag > 36 & months_post_diag <= 48 ~ paste0("3-4 years\n(", diag_year+3, "-", diag_year+4, ")"),
                                     months_post_diag > 48 ~ paste0("4-5 years\n(", diag_year+4, "-", diag_year+5, ")"))) %>%
  group_by(diag_year, years_post_diag) %>%
  summarise(attend_count = n()) %>%
  mutate(num_for_factor = as.numeric(gsub("\\D", "", years_post_diag))) %>%
  mutate(years_post_diag = fct_reorder(years_post_diag, num_for_factor)) %>%
  ungroup()

#by years post-diagnosis 
ggplot(los_ae_events_by_year, aes(x = years_post_diag, y = attend_count)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~diag_year, nrow = 1, scales = "free_x")         
