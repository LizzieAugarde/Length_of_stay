###################### Length of stay 2023 ######################

#Data prep - cohort
#Script to extract cohort of 2014 diagnoses (SQL query separate),
#creating denominator data of patients alive at the end of each time period, 
#and creating age variables

#Created June 2024 by Lizzie Augarde 
############################################################### 

##### PREP AND READ IN RAW DATA ########
library(NDRSAfunctions)
library(tidyverse)
library(janitor)

casref01 <- createConnection()

#query to create the cohort is in QA folder, takes approx 2 minutes. Extract takes approx 2 minutes
cohort <- dbGetQueryOracle(casref01, "SELECT * FROM LOS2014_COHORT", rowlimit = NA) 


##### CREATING DENOMINATOR DATA FRAME - WHETHER A PATIENT IS ALIVE AT THE END OF EACH TIME PERIOD ##### 
cohort_survival <- cohort %>%
  clean_names() %>%
  mutate(surv_days_post_diag = difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days")) %>% #days alive after diagnosis 
  
  #variables indicating whether patient is alive at the end of each time period 
  mutate(alive_12months = ifelse(surv_days_post_diag >= 365 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_2years = ifelse(surv_days_post_diag >= 730 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_3years = ifelse(surv_days_post_diag >= 1095 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_4years = ifelse(surv_days_post_diag >= 1460 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_5years = ifelse(surv_days_post_diag >= 1825 | is.na(surv_days_post_diag), "Yes", "No")) %>%
  select(patientid, alive_12months, alive_2years, alive_3years, alive_4years, alive_5years) 


##### CREATING CLEANED COHORT DATASET ########
cohort_clean <- cohort %>% 
  clean_names() %>%
  mutate(patientid = as.character(patientid)) %>%
  mutate(diag_age_days = difftime(as.Date(diagnosisdatebest), as.Date(birthdatebest), units = "days")) 
  
#ageing on - working out each patient's age at each time point post-diagnosis
cohort_clean <- cohort_clean |>
  mutate(age_12months_postdiag = as.numeric(diag_age_days + 365),
         age_2years_postdiag = as.numeric(diag_age_days + 730),
         age_3years_postdiag = as.numeric(diag_age_days + 1095),
         age_4years_postdiag = as.numeric(diag_age_days + 1460),
         age_5years_postdiag = as.numeric(diag_age_days + 1825)) %>%
  mutate(age_12months_postdiag = floor(age_12months_postdiag/365),
         age_2years_postdiag = floor(age_2years_postdiag/365),
         age_3years_postdiag = floor(age_3years_postdiag/365),
         age_4years_postdiag = floor(age_4years_postdiag/365),
         age_5years_postdiag = floor(age_5years_postdiag/365))

#age groups
categorise_age <- function(agevar) {
  case_when(
    agevar < 10 ~ "0-9", agevar < 20 ~ "10-19", agevar < 30 ~ "20-29", 
    agevar < 40 ~ "30-39", agevar < 50 ~ "40-49", agevar < 60 ~ "50-59", 
    agevar < 70 ~ "60-69", agevar < 80 ~ "70-79", agevar >= 80 ~ "80+")
}
  
cohort_clean <- cohort_clean |>
  mutate(
    age_12months_postdiag = categorize_age(age_12months_postdiag),
    age_2years_postdiag = categorize_age(age_2years_postdiag),
    age_3years_postdiag = categorize_age(age_3years_postdiag),
    age_4years_postdiag = categorize_age(age_4years_postdiag),
    age_5years_postdiag = categorize_age(age_5years_postdiag)) |>

#ethnicity 
  mutate(ethnicity = case_when(ethnicity %in% c("A", "B", "C", "0") ~ "White",
                               ethnicity %in% c("D", "E", "F", "G") ~ "Mixed",
                               ethnicity %in% c("H", "J", "K", "L") ~ "Asian",
                               ethnicity %in% c("M", "N", "P") ~ "Black",
                               ethnicity == "R" ~ "Chinese", 
                               ethnicity == "S" ~ "Other",
                               ethnicity %in% c("Z", "X") ~ "Not known", TRUE ~ "Not known"))

  #removing unnecessary variables 
  select(-c(diagnosisdatebest, birthdatebest, diag_age_days))
####need to add in staging

##### DENOMINATOR DATA FRAMES BY TIME PERIOD AND AGE GROUP FOR GRAPHS #######
time_intervals <- c("12months", "2years", "3years", "4years", "5years")

cohort_survival_age <- left_join(cohort_survival, cohort_agevars, by = "patientid")

#function to create a data frame of patients who are alive at the end of a time period, by age group and another variable 
create_survival_cohort <- function(data, variable, alive_variable, period) {
  data %>%
    select(patientid, !!variable, !!alive_variable) %>%
    filter(!!alive_variable == "Yes") %>%
    group_by(!!variable) %>%
    summarise(number_alive_at_period_end = n()) %>%
    rename("age_group" := !!variable) %>%
    mutate(period = period)
}

#function to run the create_survival_cohort function across all the time periods in the time_intervals object
survival_cohorts <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  alive_variable <- sym(paste0("alive_", interval))
  period <- interval
  
  create_survival_cohort(cohort_survival_age, age_variable, alive_variable, period)
}) 

names(survival_cohorts) <- paste0("survival_cohort_", time_intervals)
list2env(survival_cohorts, envir = .GlobalEnv) #converts the survival cohorts from a list to individual objects
rm(survival_cohorts) #removes the list object
survival_cohort_objects <- ls(pattern = "^survival_cohort")
survival_cohort_list <- mget(survival_cohort_objects) 
combined_survival_cohort <- do.call(rbind, survival_cohort_list) #combines to a single data frame ie the number alive at each time interval, by age group




