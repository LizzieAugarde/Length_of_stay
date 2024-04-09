###################### Length of stay 2023 ######################

#Script to process linked A&E data/registry data from HES (A&E events 
#in first 5 years post diagnosis from AT_PATHWAY linked on TUMOURID)

#Created November 2023 by Lizzie Augarde 
#Change log:
#06/12/2023 adapted to read in from csv and developed cleaning and processing steps
#22/01/2024 filtered to attendances within the 5 year post diagnosis window. Added
#variables to identify who survives to different time periods. Aggregating to number
#of attendances per patient by time period
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
  mutate(att_3months = ifelse(att_days_post_diag < 93, 1, 0),
         att_6months = ifelse(att_days_post_diag < 183, 1, 0),
         att_9months = ifelse(att_days_post_diag < 274, 1, 0),
         att_12months = ifelse(att_days_post_diag < 366, 1, 0),
         att_1.5years = ifelse(att_days_post_diag < 550, 1, 0),
         att_2years = ifelse(att_days_post_diag < 731, 1, 0),
         att_2.5years = ifelse(att_days_post_diag < 914, 1, 0),
         att_3years = ifelse(att_days_post_diag < 1096, 1, 0),
         att_3.5years = ifelse(att_days_post_diag < 1279, 1, 0),
         att_4years = ifelse(att_days_post_diag < 1461, 1, 0),
         att_4.5years = ifelse(att_days_post_diag < 1643, 1, 0),
         att_5years = ifelse(att_days_post_diag < 1826, 1, 0)) %>%
  
  #identifying how long after diagnosis each patient survives
  mutate(surv_days_post_diag = difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days")) %>%
  mutate(alive_3months = ifelse(surv_days_post_diag >= 92 | is.na(surv_days_post_diag), "Yes", "No"), #marking those who die within 3 months of diagnosis with "Yes"
         alive_6months = ifelse(surv_days_post_diag >= 182 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_9months = ifelse(surv_days_post_diag >= 273 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_12months = ifelse(surv_days_post_diag >= 365 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_1.5years = ifelse(surv_days_post_diag >= 549 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_2years = ifelse(surv_days_post_diag >= 730 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_2.5years = ifelse(surv_days_post_diag >= 913 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_3years = ifelse(surv_days_post_diag >= 1095 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_3.5years = ifelse(surv_days_post_diag >= 1278 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_4years = ifelse(surv_days_post_diag >= 1460 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_4.5years = ifelse(surv_days_post_diag >= 1642 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_5years = ifelse(surv_days_post_diag >= 1825 | is.na(surv_days_post_diag), "Yes", "No"))

#write out record level A&E attendance data
write.csv(los2014_ae_events, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Attendance record-level cleaned A&E data 20240122.csv")

#survival variables only data frame
los2014_ae_patients_survival <- los2014_ae_events %>%
  select(patientid, alive_3months, alive_6months, alive_9months, alive_12months, alive_1.5years,
         alive_2years, alive_2.5years, alive_3years, alive_3.5years, alive_4years, alive_4.5years, alive_5years) %>%
  unique()


##### TOTAL ATTENDANCES PER PATIENT PER TIME PERIOD ##### 
los2014_ae_patient_agg <- los2014_ae_events %>%
  mutate(attend_count = 1) %>%
  group_by(patientid) %>%
  
  #cumulative total attendances
  summarize(cum_att_3months = sum(att_3months), cum_att_6months = sum(att_6months), cum_att_9months = sum(att_9months), cum_att_12months = sum(att_12months), 
            cum_att_1.5years = sum(att_1.5years), cum_att_2years = sum(att_2years), 
            cum_att_2.5years = sum(att_2.5years), cum_att_3years = sum(att_3years),
            cum_att_3.5years = sum(att_3.5years), cum_att_4years = sum(att_4years),
            cum_att_4.5years = sum(att_4.5years), cum_att_5years = sum(att_5years),
            
            #period-specific total attendances (i.e. att 6 months is attendances which occur between 3 and 6 months only)
            ps_att_3months = cum_att_3months, ps_att_6months = cum_att_6months-cum_att_3months, 
            ps_att_9months = cum_att_9months-cum_att_6months, ps_att_12months = cum_att_12months-cum_att_9months, 
            ps_att_1.5years = cum_att_1.5years-cum_att_12months, ps_att_2years = cum_att_2years-cum_att_1.5years, 
            ps_att_2.5years = cum_att_2.5years-cum_att_2years, ps_att_3years = cum_att_3years-cum_att_2.5years, 
            ps_att_3.5years = cum_att_3.5years-cum_att_3years, ps_att_4years = cum_att_4years-cum_att_3.5years, 
            ps_att_4.5years = cum_att_4.5years-cum_att_4years, ps_att_5years = cum_att_5years-cum_att_4.5years) %>%
  
  left_join(select(los2014_ae_patients_survival, patientid, alive_3months, alive_6months, alive_9months, alive_12months, alive_1.5years,
                   alive_2years, alive_2.5years, alive_3years, alive_3.5years, alive_4years, alive_4.5years, alive_5years), by = "patientid")


##### ADDING VARIABLES NEEDED FOR AGE BREAKDOWNS #####   
los2014_ae_patient_agg <- los2014_ae_patient_agg %>%
  mutate(patientid = as.character(patientid)) %>%
  left_join(., los2014_cohort_agevars, by = "patientid") 

#write out patient level aggregated A&E data
write.csv(los2014_ae_patient_agg, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Patient-level aggregated A&E data 20240409.csv")


  