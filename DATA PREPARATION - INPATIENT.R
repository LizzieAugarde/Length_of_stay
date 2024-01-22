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

#extract
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
          heslive.hes_linkage_av_apc@casref01 b on a.patientid = b.patientid left join --link to HES linkage fields
          heslive.hesapc@casref01 c on b.epikeyanon = c.epikeyanon and b.datayear = c.datayear left join --link to HES inpatient records
          heslive.hesapc_diag@casref01 d on c.epikeyanon = d.epikeyanon and c.datayear = d.datayear --link to HES inpatient diagnostic codes
      where (c.datayear > 2013 and c.datayear < 2022)
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

#checks 
nrow(los2014_apc_events[duplicated(los2014_apc_events), ]) #duplicate rows 
n_distinct(los2014_apc_events$PATIENTID) #number unique patient IDs is <= cohort number

los2014_apc_events_check <- los2014_apc_events %>% 
  mutate(death_diag_comp = interval(FOLLOW_UP_START, DEATHDATEBEST) / days(1)) %>%
  mutate(diag_epi_comp = interval(FOLLOW_UP_START, ADMIDATE) / days(1)) %>% 
  mutate(fuend_epi_comp = interval(ADMIDATE, FOLLOW_UP_END) / days(1)) %>%
  mutate(cohort_check = ifelse(PATIENTID %in% los2014_cohort$PATIENTID, "TRUE", "FALSE"))

nrow(los2014_apc_events_check[los2014_apc_events_check$death_diag_comp < 1 & !is.na(los2014_apc_events_check$death_diag_comp), ]) #no deaths before diagnoses
nrow(los2014_apc_events_check[los2014_apc_events_check$diag_epi_comp < 1 & !is.na(los2014_apc_events_check$diag_epi_comp), ]) #admission date before diag date - what to do about those diagnosed during a spell? 
nrow(los2014_apc_events_check[los2014_apc_events_check$fuend_epi_comp < 1 & !is.na(los2014_apc_events_check$fuend_epi_comp), ]) #some died on same day as admittance - what to do about these? 
nrow(los2014_apc_events_check[los2014_apc_events_check$cohort_check == "FALSE"]) #all patients in events are in cohort table

#defining episodes as cancer-related
los2014_apc_events <- los2014_apc_events %>%
  clean_names() %>%
  unique() %>% #removing duplicate events
  mutate(diag1_cancer = case_when(str_detect(diag_1, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag2_cancer = case_when(str_detect(diag_2, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag3_cancer = case_when(str_detect(diag_3, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(episode_cancer_related = case_when(diag1_cancer == "Y" | diag2_cancer == "Y" | diag3_cancer == "Y" ~ "Y", TRUE ~ "N"))

diagcols <- c("diag1_cancer", "diag2_cancer", "diag3_cancer")

los2014_apc_events <- los2014_apc_events %>%
  mutate(cancer_code_pos = apply(select(., diagcols) == "Y", 1, function(x) toString(which(x)))) %>%
  mutate(cancer_code_pos = ifelse(cancer_code_pos == "", "NA", cancer_code_pos))

#adjusting dates
los2014_apc_events <- los2014_apc_events %>%
  mutate(adj_disdate = if_else(disdate > follow_up_end, follow_up_end, disdate)) %>% #caps discharge dates to date of death or FU end if they exceed 
  #how long after diagnosis each admission occurs  
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
  