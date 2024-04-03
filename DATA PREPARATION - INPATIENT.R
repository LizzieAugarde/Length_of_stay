###################### Length of stay 2023######################

#Script to process HES APC data for 2014 diagnoses into LOS data frame

#Created November 2023 by Lizzie Augarde 
#Change log:
#09/02/2024 added LOS calculations and tidied script
#08/03/2024 added in patient age and ageing on for 1, 2 and 5 years post-diagnosis, 
#also keeping all episodes cancer-related and not cancer-related 
############################################################### 

##### PREP AND RAW DATA ##### 
library(NDRSAfunctions)
library(tidyverse)
library(janitor)

casref01 <- createConnection()

#extract - extract from SQL takes ages, better to read from csvs in data folder (below)
apc_query <- "select a.tumourid, 
                     a.patientid, 
                     a.follow_up_start, 
                     a.follow_up_end, 
                     a.deathdatebest, 
                     c.epikeyanon,
                     c.datayear,
                     c.admidate, 
                     c.disdate, 
                     c.epiend_raw,
                     c.admimeth,
                     max(case when d.pos = 1 then d.diag_4 end) as diag_1,
                     max(case when d.pos = 2 then d.diag_4 end) as diag_2,
                     max(case when d.pos = 3 then d.diag_4 end) as diag_3
      from analysiselizabethaugarde.los2014_cohort a left join 
          heslive.hes_linkage_av_apc@casref01 b on a.patientid = b.patientid left join 
          heslive.hesapc@casref01 c on b.epikeyanon = c.epikeyanon and b.datayear = c.datayear left join 
          heslive.hesapc_diag@casref01 d on c.epikeyanon = d.epikeyanon and c.datayear = d.datayear 
      where c.datayear = '1920'
      group by a.tumourid,
               a.patientid,
               a.follow_up_start,
               a.follow_up_end,
               a.deathdatebest,
               c.epikeyanon,
               c.datayear,
               c.admidate,
               c.disdate,
               c.epiend_raw,
               c.admimeth"

los2014_apc_events <- dbGetQueryOracle(casref01, apc_query, rowlimit = NA)

#read from csvs 
los2014_apc_events_1314_1415 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1314_1415_20240207.csv")
los2014_apc_events_1516_1617 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1516_1617_20240207.csv")
los2014_apc_events_1718_1819 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1718_1819_20240207.csv")
los2014_apc_events_1920 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1920_20240207.csv")

los2014_apc_events <- rbind(los2014_apc_events_1314_1415, los2014_apc_events_1516_1617, los2014_apc_events_1718_1819, los2014_apc_events_1920)

#adjusting discharge dates for records with weird dates/missing discharge date
los2014_apc_events <- los2014_apc_events %>% 
  mutate(EPIEND_RAW = dmy(EPIEND_RAW)) %>%
  mutate(DISDATE = ifelse(DISDATE %in% c("1801-01-01", "1800-01-01"), as.character(EPIEND_RAW), 
                          ifelse(is.na(DISDATE), as.character(EPIEND_RAW), DISDATE)))


##### CHECKS ##### 
nrow(los2014_apc_events[duplicated(los2014_apc_events), ]) #duplicate rows 
n_distinct(los2014_apc_events$PATIENTID) #number unique patient IDs is <= cohort number

los2014_apc_events <- los2014_apc_events %>% 
  mutate(death_diag_comp = interval(FOLLOW_UP_START, DEATHDATEBEST) / days(1)) %>%
  mutate(diag_epi_comp = interval(FOLLOW_UP_START, ADMIDATE) / days(1)) %>% 
  mutate(fuend_epi_comp = interval(ADMIDATE, FOLLOW_UP_END) / days(1)) %>%
  #mutate(cohort_check = ifelse(PATIENTID %in% los2014_cohort$PATIENTID, "TRUE", "FALSE")) %>%
  filter(diag_epi_comp >= 0) %>% #keeping only episodes starting on or after diag date
  filter(fuend_epi_comp >= 0) #keeping only episodes starting on or before follow up end date

length(which(los2014_apc_events$death_diag_comp < 1 & !is.na(los2014_apc_events$death_diag_comp))) #should not include anyone with death date before or on adm start date
length(which(los2014_apc_events$diag_epi_comp < 0 & !is.na(los2014_apc_events$diag_epi_comp))) 
length(which(los2014_apc_events$fuend_epi_comp < 0 & !is.na(los2014_apc_events$fuend_epi_comp))) 
length(which(los2014_apc_events$cohort_check  == "FALSE")) #all patients in events are in cohort table


##### IDENTIFYING CANCER-RELATED ADMISSIONS ##### 
#defining episodes as cancer-related based on C code in first 3 diagnosis code positions
los2014_apc_events <- los2014_apc_events %>%
  clean_names() %>%
  unique() %>% #removing duplicate events
  select(-c(death_diag_comp, diag_epi_comp, fuend_epi_comp)) %>% #removing check variables
  mutate(diag1_cancer = case_when(str_detect(diag_1, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag2_cancer = case_when(str_detect(diag_2, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag3_cancer = case_when(str_detect(diag_3, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(episode_cancer_related = case_when(diag1_cancer == "Y" | diag2_cancer == "Y" | diag3_cancer == "Y" ~ "Y", TRUE ~ "N")) 

#count number of episodes overall 
length(unique(los2014_apc_events$epikeyanon)) #1955032 episodes

#keeping only cancer-related admissions and marking the position of the cancer ICD-10 code
diagcols <- c("diag1_cancer", "diag2_cancer", "diag3_cancer")

los2014_apc_events <- los2014_apc_events %>%
  #filter(episode_cancer_related == "Y") %>% #removing for now - want to look at both 'cancer-related' and all APC care
  mutate(cancer_code_pos = apply(select(., all_of(diagcols)) == "Y", 1, function(x) toString(which(x)))) %>%
  mutate(cancer_code_pos = ifelse(cancer_code_pos == "", "NA", cancer_code_pos))

#count number of episodes and patients
los2014_apc_events %>%
  group_by(episode_cancer_related) %>%
  summarize(count = n_distinct(epikeyanon)) #1318287 episodes cancer-related, 636745 episodes not cancer-related 

los2014_apc_events %>%
  group_by(episode_cancer_related) %>%
  summarize(count = n_distinct(patientid)) #171442 patients with cancer-related episode, 138238 patients with episode not cancer-related 


##### CALCULATING LENGTH OF STAY ##### 
los2014_apc_events <- los2014_apc_events %>%
  mutate(disdate = if_else(disdate > follow_up_end, follow_up_end, disdate)) %>% #caps discharge dates to date of death or FU end if they exceed 
  
  #calculating length of each episode in days - adding one to count the start date as 1 day and prevent any 0 length episodes
  mutate(epi_length = difftime(as.Date(disdate), as.Date(admidate), units = "days") + 1) %>%
  mutate(epi_length = as.numeric(epi_length)) %>%
  
  #set up dates for time periods for each patient 
  mutate(fus_plus3months  = as.Date(follow_up_start) + 92,
         fus_plus6months  = as.Date(follow_up_start) + 182,
         fus_plus9months  = as.Date(follow_up_start) + 273,
         fus_plus12months = as.Date(follow_up_start) + 365,
         fus_plus1.5years = as.Date(follow_up_start) + 549,
         fus_plus2years   = as.Date(follow_up_start) + 730,
         fus_plus2.5years = as.Date(follow_up_start) + 913,
         fus_plus3years   = as.Date(follow_up_start) + 1095,
         fus_plus3.5years = as.Date(follow_up_start) + 1278,
         fus_plus4years   = as.Date(follow_up_start) + 1460,
         fus_plus4.5years = as.Date(follow_up_start) + 1642,
         fus_plus5years   = as.Date(follow_up_start) + 1825) %>%
  
  #set up time difference variables - time between admission and each follow up date 
  mutate(diff_3months  = difftime(as.Date(fus_plus3months), as.Date(admidate), units = "days"),
         diff_6months  = difftime(as.Date(fus_plus6months), as.Date(admidate), units = "days"),
         diff_9months  = difftime(as.Date(fus_plus9months), as.Date(admidate), units = "days"),
         diff_12months = difftime(as.Date(fus_plus12months), as.Date(admidate), units = "days"),
         diff_1.5years = difftime(as.Date(fus_plus1.5years), as.Date(admidate), units = "days"),
         diff_2years   = difftime(as.Date(fus_plus2years), as.Date(admidate), units = "days"),
         diff_2.5years = difftime(as.Date(fus_plus2.5years), as.Date(admidate), units = "days"),
         diff_3years   = difftime(as.Date(fus_plus3years), as.Date(admidate), units = "days"),
         diff_3.5years = difftime(as.Date(fus_plus3.5years), as.Date(admidate), units = "days"),
         diff_4years   = difftime(as.Date(fus_plus4years), as.Date(admidate), units = "days"),
         diff_4.5years = difftime(as.Date(fus_plus4.5years), as.Date(admidate), units = "days"),
         diff_5years   = difftime(as.Date(fus_plus5years), as.Date(admidate), units = "days")) %>%
  
  #calculating length of each episode within each time period 
  mutate(los_3months  = ifelse(diff_3months < 0, 0, ifelse(epi_length > diff_3months, diff_3months, epi_length)),
         los_6months  = ifelse(diff_6months < 0, 0, ifelse(epi_length > diff_6months, diff_6months, epi_length)),
         los_9months  = ifelse(diff_9months < 0, 0, ifelse(epi_length > diff_9months, diff_9months, epi_length)),
         los_12months = ifelse(diff_12months < 0, 0, ifelse(epi_length > diff_12months, diff_12months, epi_length)),
         los_1.5years  = ifelse(diff_1.5years < 0, 0, ifelse(epi_length > diff_1.5years, diff_1.5years, epi_length)),
         los_2years   = ifelse(diff_2years < 0, 0, ifelse(epi_length > diff_2years, diff_2years, epi_length)),
         los_2.5years = ifelse(diff_2.5years < 0, 0, ifelse(epi_length > diff_2.5years, diff_2.5years, epi_length)),
         los_3years   = ifelse(diff_3years < 0, 0, ifelse(epi_length > diff_3years, diff_3years, epi_length)),
         los_3.5years = ifelse(diff_3.5years < 0, 0, ifelse(epi_length > diff_3.5years, diff_3.5years, epi_length)),
         los_4years   = ifelse(diff_4years < 0, 0, ifelse(epi_length > diff_4years, diff_4years, epi_length)),
         los_4.5years = ifelse(diff_4.5years < 0, 0, ifelse(epi_length > diff_4.5years, diff_4.5years, epi_length)),
         los_5years   = ifelse(diff_5years < 0, 0, ifelse(epi_length > diff_5years, diff_5years, epi_length)))


##### ADDING SURVIVAL METRICS ##### 
los2014_apc_events <- los2014_apc_events %>%
  mutate(surv_days_post_diag = difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days")) %>%
  mutate(alive_3months = ifelse(surv_days_post_diag >= 92 | is.na(surv_days_post_diag), "Yes", "No"), #marking those who die within 3 months of diagnosis with "No"
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

#check for any stays longer than survival length (should have all been excluded earlier in script)
length(which(los2014_apc_events$epi_length-1 > los2014_apc_events$surv_days_post_diag)) 

#survival variables only data frame
los2014_apc_patients_survival <- los2014_apc_events %>%
  select(patientid, alive_3months, alive_6months, alive_9months, alive_12months, alive_1.5years, alive_2years, alive_2.5years, alive_3years, 
         alive_3.5years, alive_4years, alive_4.5years, alive_5years) %>%
  unique()


##### WRITE OUT CLEANED RECORD-LEVEL APC DATA ##### 
write.csv(los2014_apc_events, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Episode record-level cleaned APC data all episodes 20240308.csv")


##### TOTAL LENGTH OF STAY PER PATIENT PER TIME PERIOD ##### 
los2014_apc_patient_agg <- los2014_apc_events %>%
  group_by(patientid, episode_cancer_related) %>%
  summarize(total_los_3months = sum(los_3months), total_los_6months = sum(los_6months), total_los_9months = sum(los_9months), 
            total_los_12months = sum(los_12months), total_los_1.5years = sum(los_1.5years), total_los_2years = sum(los_2years), 
            total_los_2.5years = sum(los_2.5years), total_los_3years = sum(los_3years), total_los_3.5years = sum(los_3.5years), 
            total_los_4years = sum(los_4years), total_los_4.5years = sum(los_4.5years), total_los_5years = sum(los_5years)) %>%
  
  #adding survival for each patient
  left_join(select(los2014_apc_patients_survival, patientid, alive_3months, alive_6months, alive_9months, alive_12months, alive_1.5years, alive_2years, 
                   alive_2.5years, alive_3years, alive_3.5years, alive_4years, alive_4.5years, alive_5years), by = "patientid")


##### ADDING VARIABLES NEEDED FOR AGE BREAKDOWNS #####   
los2014_apc_patient_agg <- los2014_apc_patient_agg %>%
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


##### WRITE OUT CLEANED PATIENT-LEVEL AGGREGATED APC DATA ##### 
write.csv(los2014_apc_patient_agg, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Patient-level aggregated APC data 20240308.csv")


  