###################### Length of stay 2023######################

#Script to process linked A&E data/registry data from HES (A&E events 
#in first 5 years post diagnosis from AT_PATHWAY linked on TUMOURID)

#Created November 2023 by Lizzie Augarde 
#Change log:
#06/12/2023 adapted to read in from csv and developed cleaning and processing steps
############################################################### 

#prep
library(tidyverse)
library(janitor)
library(NDRSAfunctions)

casref01 <- createConnection()

#extract
ae_query <- "select a.tumourid, 
                    a.patientid, 
                    a.follow_up_start, 
                    a.follow_up_end, 
                    a.deathdatebest, 
                    c.aekeyanon,
                    c.datayear,
                    c.arrivaldate_raw
      from analysiselizabethaugarde.los2014_cohort a left join 
          heslive.hes_linkage_av_ae@casref01 b on a.patientid = b.patientid left join
          heslive.hesae@casref01 c on b.aekeyanon = c.aekeyanon and b.datayear = c.datayear
      where c.datayear in ('1314', '1415', '1516', '1617', '1718', '1819', '1920', '2021')
      and a.patientid is not null"

los2014_ae_events_raw <- dbGetQueryOracle(casref01, ae_query, rowlimit = NA)

#convert arrival date variable to date 
los2014_ae_events_raw <- los2014_ae_events_raw %>% 
  mutate(ARRIVALDATE = as.Date(ARRIVALDATE_RAW, format = "%d%m%Y"))

#checks 
nrow(los2014_ae_events_raw[duplicated(los2014_ae_events_raw), ]) #duplicate rows 
n_distinct(los2014_ae_events_raw$PATIENTID) #number unique patient IDs is <= cohort number

los2014_ae_events_check <- los2014_ae_events_raw %>% 
  mutate(death_diag_comp = interval(FOLLOW_UP_START, DEATHDATEBEST) / days(1)) %>%
  mutate(diag_att_comp = interval(FOLLOW_UP_START, ARRIVALDATE) / days(1)) %>% 
  mutate(fuend_att_comp = interval(ARRIVALDATE, FOLLOW_UP_END) / days(1)) %>%
  mutate(PATIENTID = as.character(PATIENTID)) %>%
  mutate(cohort_check = ifelse(PATIENTID %in% los2014_cohort$PATIENTID, "TRUE", "FALSE"))

nrow(los2014_ae_events_check[los2014_ae_events_check$death_diag_comp < 1 & !is.na(los2014_ae_events_check$death_diag_comp), ]) #no deaths before diagnoses
nrow(los2014_ae_events_check[los2014_ae_events_check$diag_epi_comp < 1 & !is.na(los2014_ae_events_check$diag_epi_comp), ]) #attendance date before diag date - what to do about those diagnosed during a spell? 
nrow(los2014_ae_events_check[los2014_ae_events_check$fuend_epi_comp < 1 & !is.na(los2014_ae_events_check$fuend_epi_comp), ]) #some died on same day as attendance - what to do about these? 
nrow(los2014_ae_events_check[los2014_ae_events_check$cohort_check == "FALSE"]) #all patients in events are in cohort table
NEED TO FIGURE THESE BITS OUT BEFORE DOIGN ANYTHING ELSE 

#adjusting dates
los2014_ae_events <- los2014_ae_events_raw %>%NEED TO FILTER OUT ATTENDANCES BEYOND THE 5 YEAR MARK 
  clean_names() %>%
  unique() %>% #removing duplicate events
  #how long after diagnosis each attendance occurs  
  mutate(att_days_post_diag = difftime(as.Date(arrivaldate), as.Date(follow_up_start), units = "days")) %>%
  mutate(att_months_post_diag = case_when(att_days_post_diag < 93  ~ "Within 3 months",
                                          att_days_post_diag > 92 & att_days_post_diag < 183 ~ "3-6 months",
                                          att_days_post_diag > 182 & att_days_post_diag < 366  ~ "6-12 months",
                                          att_days_post_diag > 365  ~ "At least 12 months",)) %>%
  mutate(att_years_post_diag = case_when(att_days_post_diag < 366 ~ "Within 1 year",
                                         att_days_post_diag > 365 & att_days_post_diag < 731 ~ "1-2 years",
                                         att_days_post_diag > 730 & att_days_post_diag < 1096  ~ "2-3 years",
                                         att_days_post_diag > 1095 & att_days_post_diag < 1461  ~ "3-4 years",
                                         att_days_post_diag > 1460 ~ "4-5 years",)) %>% THIS KEEPS IN STUFF BEYOND 5 YEARS, FIX THIS
  unique() #removing duplicate events





  #identifying how long after diagnosis each patient died
  mutate(death_years_post_diag = case_when(difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days") < 366 ~ "Within 1 year",
                                           difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days") < 731 ~ "1-2 years",
                                           difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days") < 1096 ~ "2-3 years",
                                           difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days") < 1461 ~ "3-4 years",
                                           difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days") > 1460  ~ "4-5 years",
                                           TRUE ~ "Alive at 5 years")) %>%
  #identifying whether a patient is alive at each year post diagnosis
  mutate(alive_years_post_diag = case_when(death_years_post_diag == "Within 1 year" ~ "Death within 1 year",
                                           death_years_post_diag == "1-2 years" ~ "At least 1 year",
                                           death_years_post_diag == "2-3 years" ~ "At least 2 years",
                                           death_years_post_diag == "3-4 years" ~ "At least 3 years",
                                           death_years_post_diag == "4-5 years" ~ "At least 4 years",
                                           TRUE ~ "At least 5 years"))


  
  
  
  
  
