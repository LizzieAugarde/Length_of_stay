###################### Length of stay 2023 ######################

#Script to process HES outpatient data for 2014 diagnoses into LOS data frame

#Created November 2023 by Lizzie Augarde 
#Change log:
#21/02/2024 revised based on LOS calculations in APC and A&E data, 
#waiting on OP treatment specialties list from Mac COCE
############################################################### 

##### PREP AND RAW DATA ##### 
library(NDRSAfunctions)
library(tidyverse)
library(janitor)

casref01 <- createConnection()

#extract 
los2014_op_events_query <- "select 
    a.tumourid, 
    a.patientid, 
    a.site_icd10_o2_3char,
    a.follow_up_start, 
    a.follow_up_end, 
    a.deathdatebest, 
    c.attendkeyanon,
    c.datayear,
    c.apptdate,
    c.attended,
    c.stafftyp,
    c.tretspef
from analysiselizabethaugarde.los2014_cohort a
left join heslive.hes_linkage_av_op@casref01 b on a.patientid = b.patientid
left join heslive.hesop@casref01 c on b.attendkeyanon = c.attendkeyanon and b.datayear = c.datayear
where c.datayear in ('1314', '1415', '1516', '1617', '1718', '1819', '1920')
  and c.tretspef in ('370', '800', '318', '101', '100', '130', '103', '812', '303', '330', '160', '340',   
                     '320', '110', '120', '104', '315', '650', '140', '502', '301', '410', '300', '503', '324')
  and a.patientid is not null"

los2014_op_events <- dbGetQueryOracle(casref01, los2014_op_events_query, rowlimit = NA)


##### CHECKS ##### 
nrow(los2014_op_events[duplicated(los2014_op_events), ]) #duplicate rows 
n_distinct(los2014_op_events$PATIENTID) #number unique patient IDs is <= cohort number
n_distinct(los2014_op_events$ATTENDKEYANON) #number of unique IDs should be same as number of rows

#duplicated ATTENDKEYANON - 8 ATTENDKEYANONs appear twice, same appts associated with 2 patients, excluding all these ATTENDKEYANONs
los2014_op_events %>% group_by(ATTENDKEYANON) %>% filter(n() >1) #view duplicates

los2014_op_events <- los2014_op_events %>% #removing duplicated ATTENDKEYANONS
  group_by(ATTENDKEYANON) %>%
  filter(n() == 1) %>%
  ungroup()

n_distinct(los2014_op_events$ATTENDKEYANON) #number of unique IDs should be same as number of rows

#time intervals
los2014_op_events <- los2014_op_events %>% 
  mutate(death_diag_comp = interval(FOLLOW_UP_START, DEATHDATEBEST) / days(1)) %>%
  mutate(diag_appt_comp = interval(FOLLOW_UP_START, APPTDATE) / days(1)) %>% 
  mutate(fuend_appt_comp = interval(APPTDATE, FOLLOW_UP_END) / days(1)) %>% 
  mutate(cohort_check = ifelse(PATIENTID %in% los2014_cohort$PATIENTID, "TRUE", "FALSE")) %>%
  filter(diag_appt_comp >= 0) %>% #keeping only appointments on or after diag date
  filter(fuend_appt_comp >= 0) #keeping only appointments starting on or before follow up end date

length(which(los2014_op_events$death_diag_comp < 1 & !is.na(los2014_op_events$death_diag_comp))) #should not include anyone with death date before or on adm start date
length(which(los2014_op_events$diag_appt_comp < 0 & !is.na(los2014_op_events$diag_appt_comp))) 
length(which(los2014_op_events$cohort_check  == "FALSE")) #all patients in events are in cohort table


##### CALCULATING LENGTH OF STAY ##### 
los2014_op_events <- los2014_op_events %>%
  clean_names() %>%
  
  #counting each appointment as 1 day LOS
  select(-c(death_diag_comp, diag_appt_comp, fuend_appt_comp, cohort_check)) %>% #removing check variables
  
  #how long after diagnosis each appointment occurs  
  mutate(appt_days_post_diag = difftime(as.Date(apptdate), as.Date(follow_up_start), units = "days")) %>%
  mutate(appt_3months = ifelse(appt_days_post_diag < 93, 1, 0),
         appt_6months = ifelse(appt_days_post_diag < 183, 1, 0),
         appt_9months = ifelse(appt_days_post_diag < 274, 1, 0),
         appt_12months = ifelse(appt_days_post_diag < 366, 1, 0),
         appt_1.5years = ifelse(appt_days_post_diag < 550, 1, 0),
         appt_2years = ifelse(appt_days_post_diag < 731, 1, 0),
         appt_2.5years = ifelse(appt_days_post_diag < 914, 1, 0),
         appt_3years = ifelse(appt_days_post_diag < 1096, 1, 0),
         appt_3.5years = ifelse(appt_days_post_diag < 1279, 1, 0),
         appt_4years = ifelse(appt_days_post_diag < 1461, 1, 0),
         appt_4.5years = ifelse(appt_days_post_diag < 1643, 1, 0),
         appt_5years = ifelse(appt_days_post_diag < 1826, 1, 0)) %>%
  
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

#write out record level OP appointment data
write.csv(los2014_op_events, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Appointment record-level cleaned OP data 20240404.csv")

#survival variables only data frame
los2014_op_patients_survival <- los2014_op_events %>%
  select(patientid, alive_3months, alive_6months, alive_9months, alive_12months, alive_1.5years,
         alive_2years, alive_2.5years, alive_3years, alive_3.5years, alive_4years, alive_4.5years, alive_5years) %>%
  unique()


##### TOTAL LENGTH OF STAY PER PATIENT PER TIME PERIOD ##### 
los2014_op_patient_agg <- los2014_op_events %>%
  group_by(patientid) %>%
  summarize(total_appt_3months = sum(appt_3months), total_appt_6months = sum(appt_6months), total_appt_9months = sum(appt_9months), 
            total_appt_12months = sum(appt_12months), total_appt_1.5years = sum(appt_1.5years), total_appt_2years = sum(appt_2years), 
            total_appt_2.5years = sum(appt_2.5years), total_appt_3years = sum(appt_3years), total_appt_3.5years = sum(appt_3.5years), 
            total_appt_4years = sum(appt_4years), total_appt_4.5years = sum(appt_4.5years), total_appt_5years = sum(appt_5years)) %>%
  
  #adding survival for each patient
  left_join(select(los2014_op_patients_survival, patientid, alive_3months, alive_6months, alive_9months, alive_12months, alive_1.5years, alive_2years, 
                   alive_2.5years, alive_3years, alive_3.5years, alive_4years, alive_4.5years, alive_5years), by = "patientid")


##### ADDING VARIABLES NEEDED FOR AGE BREAKDOWNS #####   
los2014_op_patient_agg <- los2014_op_patient_agg %>%
  mutate(patientid = as.character(patientid)) %>%
  
  #adding in DOB and calculating age at diag
  left_join(select(los2014_cohort, patientid, diagnosisdatebest, birthdatebest), by = "patientid") %>%
  mutate(diag_age_days = difftime(as.Date(diagnosisdatebest), as.Date(birthdatebest), units = "days")) %>%
  
  #ageing on for 1, 2 and 5 years post-diagnosis 
  mutate(age_1yr_postdiag = as.numeric(diag_age_days + 365),
         age_2yrs_postdiag = as.numeric(diag_age_days + 730),
         age_5yrs_postdiag = as.numeric(diag_age_days + 1825)) %>%
  mutate(age_1yr_postdiag = floor(age_1yr_postdiag/365),
         age_2yrs_postdiag = floor(age_2yrs_postdiag/365),
         age_5yrs_postdiag = floor(age_5yrs_postdiag/365)) %>%
  
  #converting to age groups
  mutate(age_1yr_postdiag = case_when(age_1yr_postdiag < 10 ~ "0-9",
                                      age_1yr_postdiag < 20 & age_1yr_postdiag > 9 ~ "10-19",
                                      age_1yr_postdiag < 30 & age_1yr_postdiag > 19 ~ "20-29",
                                      age_1yr_postdiag < 40 & age_1yr_postdiag > 29 ~ "30-39",
                                      age_1yr_postdiag < 50 & age_1yr_postdiag > 39 ~ "40-49",
                                      age_1yr_postdiag < 60 & age_1yr_postdiag > 49 ~ "50-59",
                                      age_1yr_postdiag < 70 & age_1yr_postdiag > 59 ~ "60-69",
                                      age_1yr_postdiag < 80 & age_1yr_postdiag > 69 ~ "70-79",
                                      age_1yr_postdiag > 79 ~ "80+")) %>%
  mutate(age_2yrs_postdiag = case_when(age_2yrs_postdiag < 10 ~ "0-9",
                                       age_2yrs_postdiag < 20 & age_2yrs_postdiag > 9 ~ "10-19",
                                       age_2yrs_postdiag < 30 & age_2yrs_postdiag > 19 ~ "20-29",
                                       age_2yrs_postdiag < 40 & age_2yrs_postdiag > 29 ~ "30-39",
                                       age_2yrs_postdiag < 50 & age_2yrs_postdiag > 39 ~ "40-49",
                                       age_2yrs_postdiag < 60 & age_2yrs_postdiag > 49 ~ "50-59",
                                       age_2yrs_postdiag < 70 & age_2yrs_postdiag > 59 ~ "60-69",
                                       age_2yrs_postdiag < 80 & age_2yrs_postdiag > 69 ~ "70-79",
                                       age_2yrs_postdiag > 79 ~ "80+")) %>%
  mutate(age_5yrs_postdiag = case_when(age_5yrs_postdiag < 10 ~ "0-9",
                                       age_5yrs_postdiag < 20 & age_5yrs_postdiag > 9 ~ "10-19",
                                       age_5yrs_postdiag < 30 & age_5yrs_postdiag > 19 ~ "20-29",
                                       age_5yrs_postdiag < 40 & age_5yrs_postdiag > 29 ~ "30-39",
                                       age_5yrs_postdiag < 50 & age_5yrs_postdiag > 39 ~ "40-49",
                                       age_5yrs_postdiag < 60 & age_5yrs_postdiag > 49 ~ "50-59",
                                       age_5yrs_postdiag < 70 & age_5yrs_postdiag > 59 ~ "60-69",
                                       age_5yrs_postdiag < 80 & age_5yrs_postdiag > 69 ~ "70-79",
                                       age_5yrs_postdiag > 79 ~ "80+")) %>%
  select(-c(diagnosisdatebest, birthdatebest, diag_age_days))

#write out patient level aggregated OP data
write.csv(los2014_op_patient_agg, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Patient-level aggregated OP data 20240404.csv")

