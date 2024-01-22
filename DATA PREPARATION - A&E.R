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
  mutate(ARRIVALDATE = as.POSIXct(ARRIVALDATE_RAW, format = "%d%m%Y"))

#checks 
nrow(los2014_ae_events_raw[duplicated(los2014_ae_events_raw), ]) #duplicate rows 
n_distinct(los2014_ae_events_raw$PATIENTID) #number unique patient IDs is <= cohort number

los2014_ae_events <- los2014_ae_events_raw %>% 
  mutate(death_diag_comp = interval(FOLLOW_UP_START, DEATHDATEBEST) / days(1)) %>%
  mutate(diag_att_comp = interval(FOLLOW_UP_START, ARRIVALDATE) / days(1)) %>% 
  mutate(fuend_att_comp = interval(ARRIVALDATE, FOLLOW_UP_END) / days(1)) %>%
  mutate(PATIENTID = as.character(PATIENTID)) %>% 
  mutate(cohort_check = PATIENTID %in% los2014_cohort$PATIENTID) %>%
  filter(diag_att_comp >= 0) %>% #keeping only attendances occurring on or after diag date
  filter(fuend_att_comp >= 0) #keeping only attendances occurring on or before follow up end date

length(which(los2014_ae_events$death_diag_comp < 1 & !is.na(los2014_ae_events$death_diag_comp))) #should not include anyone with death date before or on attendance date
length(which(los2014_ae_events$diag_att_comp < 0 & !is.na(los2014_ae_events$diag_att_comp))) 
length(which(los2014_ae_events$fuend_att_comp < 0 & !is.na(los2014_ae_events$fuend_att_comp))) 
length(which(los2014_ae_events$cohort_check  == "FALSE")) #all patients in events are in cohort table

#adjusting dates
los2014_ae_events <- los2014_ae_events %>% 
  clean_names() %>%
  unique() %>% #removing duplicate events
  select(-c(death_diag_comp, diag_att_comp, fuend_att_comp, cohort_check)) %>% #removing check variables
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
                                         att_days_post_diag > 1460 ~ "4-5 years",)) %>% 
  #identifying how long after diagnosis each patient survives
  mutate(surv_days_post_diag = difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days")) %>%
  mutate(surv_months_post_diag = case_when(surv_days_post_diag < 93  ~ "Less than 3 months",
                                           surv_days_post_diag > 92 & surv_days_post_diag < 183 ~ "3-6 months",
                                           surv_days_post_diag > 182 & surv_days_post_diag < 366  ~ "6-12 months",
                                           surv_days_post_diag > 365  ~ "At least 12 months",)) %>%
  mutate(surv_years_post_diag = case_when(surv_days_post_diag < 366 ~ "Within 1 year",
                                          surv_days_post_diag > 365 & surv_days_post_diag < 731 ~ "1-2 years",
                                          surv_days_post_diag > 730 & surv_days_post_diag < 1096  ~ "2-3 years",
                                          surv_days_post_diag > 1095 & surv_days_post_diag < 1461  ~ "3-4 years",
                                          surv_days_post_diag > 1460 ~ "4-5 years",)) %>% 


  
  
  
  
  
