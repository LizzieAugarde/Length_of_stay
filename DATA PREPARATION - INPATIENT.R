###################### Length of stay 2023######################

#Script to process HES data for 2014, 2015 and 2016 diagnoses to 
#assess the impact of 2020/2021 on length of stay

#Created November 2023 by Lizzie Augarde 
#Change log:
############################################################### 

#prep
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
                     c.admimeth,
                     max(case when d.pos = 1 then d.diag_4 end) as diag_1,
                     max(case when d.pos = 2 then d.diag_4 end) as diag_2,
                     max(case when d.pos = 3 then d.diag_4 end) as diag_3
      from analysiselizabethaugarde.los2014_cohort a left join 
          heslive.hes_linkage_av_apc@casref01 b on a.patientid = b.patientid left join 
          heslive.hesapc@casref01 c on b.epikeyanon = c.epikeyanon and b.datayear = c.datayear left join 
          heslive.hesapc_diag@casref01 d on c.epikeyanon = d.epikeyanon and c.datayear = d.datayear 
      where c.datayear in ('1516', '1617')
      group by a.tumourid,
               a.patientid,
               a.follow_up_start,
               a.follow_up_end,
               a.deathdatebest,
               c.epikeyanon,
               c.datayear,
               c.admidate,
               c.disdate,
               c.admimeth"

los2014_apc_events <- dbGetQueryOracle(casref01, apc_query, rowlimit = NA)

#read from csvs 
los2014_apc_events_1314_1415 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1314_1415_20240124.csv")
los2014_apc_events_1516_1617 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1516_1617_20240124.csv")
los2014_apc_events_1718_1819 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1718_1819_20240124.csv")
los2014_apc_events_1920_2021 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1920_2021_20240124.csv")

los2014_apc_events <- rbind(los2014_apc_events_1314_1415, los2014_apc_events_1516_1617, los2014_apc_events_1718_1819, los2014_apc_events_1920_2021)

#checks 
nrow(los2014_apc_events[duplicated(los2014_apc_events), ]) #duplicate rows 
n_distinct(los2014_apc_events$PATIENTID) #number unique patient IDs is <= cohort number

los2014_apc_events <- los2014_apc_events %>% 
  mutate(death_diag_comp = interval(FOLLOW_UP_START, DEATHDATEBEST) / days(1)) %>%
  mutate(diag_epi_comp = interval(FOLLOW_UP_START, ADMIDATE) / days(1)) %>% 
  mutate(fuend_epi_comp = interval(ADMIDATE, FOLLOW_UP_END) / days(1)) %>%
  mutate(cohort_check = ifelse(PATIENTID %in% los2014_cohort$PATIENTID, "TRUE", "FALSE")) %>%
  filter(diag_epi_comp >= 0) %>% #keeping only episodes starting on or after diag date
  filter(fuend_epi_comp >= 0) #keeping only episodes starting on or before follow up end date

length(which(los2014_apc_events$death_diag_comp < 1 & !is.na(los2014_apc_events$death_diag_comp))) #should not include anyone with death date before or on adm start date
length(which(los2014_apc_events$diag_epi_comp < 0 & !is.na(los2014_apc_events$diag_epi_comp))) 
length(which(los2014_apc_events$fuend_epi_comp < 0 & !is.na(los2014_apc_events$fuend_epi_comp))) 
length(which(los2014_apc_events$cohort_check  == "FALSE")) #all patients in events are in cohort table

#defining episodes as cancer-related
los2014_apc_events <- los2014_apc_events %>%
  clean_names() %>%
  unique() %>% #removing duplicate events
  select(-c(death_diag_comp, diag_epi_comp, fuend_epi_comp, cohort_check)) %>% #removing check variables
  mutate(diag1_cancer = case_when(str_detect(diag_1, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag2_cancer = case_when(str_detect(diag_2, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag3_cancer = case_when(str_detect(diag_3, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(episode_cancer_related = case_when(diag1_cancer == "Y" | diag2_cancer == "Y" | diag3_cancer == "Y" ~ "Y", TRUE ~ "N"))

diagcols <- c("diag1_cancer", "diag2_cancer", "diag3_cancer")

los2014_apc_events <- los2014_apc_events %>%
  mutate(cancer_code_pos = apply(select(., diagcols) == "Y", 1, function(x) toString(which(x)))) %>%
  mutate(cancer_code_pos = ifelse(cancer_code_pos == "", "NA", cancer_code_pos))

#adjusting dates
#SOMETHING WEIRD HAPPENING WITH DISDATE - CHECK AGAINST RAW DATA IN SQL 
los2014_apc_events <- los2014_apc_events %>%
  #####mutate(adj_disdate = if_else(disdate > follow_up_end, follow_up_end, disdate)) %>% #caps discharge dates to date of death or FU end if they exceed 
  #how long after diagnosis each admission occurs 
  mutate(epi_days_post_diag = difftime(as.Date(admidate), as.Date(follow_up_start), units = "days")) %>%
  mutate(epi_3months = ifelse(epi_days_post_diag < 93, 1, 0),
         epi_6months = ifelse(epi_days_post_diag < 183, 1, 0),
         epi_9months = ifelse(epi_days_post_diag < 274, 1, 0),
         epi_12months = ifelse(epi_days_post_diag < 366, 1, 0),
         epi_1.5years = ifelse(epi_days_post_diag < 550, 1, 0),
         epi_2years = ifelse(epi_days_post_diag < 731, 1, 0),
         epi_2.5years = ifelse(epi_days_post_diag < 914, 1, 0),
         epi_3years = ifelse(epi_days_post_diag < 1096, 1, 0),
         epi_3.5years = ifelse(epi_days_post_diag < 1279, 1, 0),
         epi_4years = ifelse(epi_days_post_diag < 1461, 1, 0),
         epi_4.5years = ifelse(epi_days_post_diag < 1643, 1, 0),
         epi_5years = ifelse(epi_days_post_diag < 1826, 1, 0)) %>%
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
         alive_5years = ifelse(surv_days_post_diag >= 1825 | is.na(surv_days_post_diag), "Yes", "No")) %>%
  #calculating length of each episode in days 
  mutate(epi_length = difftime(as.Date(admidate), as.Date(disdate), units = "days"))

#write out record level APC episode data
write.csv(los2014_ae_events, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Episode record-level cleaned APC data 20240126.csv")

#survival variables only data frame
los2014_apc_patients_survival <- los2014_apc_events %>%
  select(patientid, alive_3months, alive_6months, alive_12months, 
         alive_2years, alive_3years, alive_4years, alive_5years) %>%
  unique()

#aggregating number of episodes by time period for each patient
los2014_apc_patient_agg_months <- los2014_ae_events %>%
  mutate(attend_count = 1) %>%
  group_by(patientid) %>%
  summarize(att_3months = sum(att_3months), att_6months = sum(att_6months), att_9months = sum(att_9months), att_12months = sum(att_12months), 
            att_1.5years = sum(att_1.5years), att_2years = sum(att_2years), 
            att_2.5years = sum(att_2.5years), att_3years = sum(att_3years),
            att_3.5years = sum(att_3.5years), att_4years = sum(att_4years),
            att_4.5years = sum(att_4.5years), att_5years = sum(att_5years)) %>%
  left_join(select(los2014_ae_patients_survival, patientid, alive_3months, alive_6months, alive_12months, 
                   alive_2years, alive_3years, alive_4years, alive_5years), by = "patientid")

#write out patient level aggregated A&E data
write.csv(los2014_ae_patient_agg_months, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Patient-level aggregated A&E data 20240122.csv")





do this bit in a separate cohort data frame? 
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
  