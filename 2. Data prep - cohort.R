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
cohort_survival <- cohort |>
  clean_names() |>
  mutate(surv_days_post_diag = difftime(as.Date(deathdatebest), as.Date(follow_up_start), units = "days")) |> #days alive after diagnosis 
  
  #variables indicating whether patient is alive at the end of each time period 
  mutate(alive_12months = ifelse(surv_days_post_diag >= 365 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_2years = ifelse(surv_days_post_diag >= 730 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_3years = ifelse(surv_days_post_diag >= 1095 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_4years = ifelse(surv_days_post_diag >= 1460 | is.na(surv_days_post_diag), "Yes", "No"),
         alive_5years = ifelse(surv_days_post_diag >= 1825 | is.na(surv_days_post_diag), "Yes", "No")) |>
  select(patientid, alive_12months, alive_2years, alive_3years, alive_4years, alive_5years) 


##### CREATING CLEANED COHORT DATASET ########
cohort_clean <- cohort |> 
  clean_names() |>
  mutate(patientid = as.character(patientid)) |>
  mutate(diag_age_days = difftime(as.Date(diagnosisdatebest), as.Date(birthdatebest), units = "days")) 
  
#ageing on - working out each patient's age at each time point post-diagnosis
cohort_clean <- cohort_clean |>
  mutate(age_12months_postdiag = as.numeric(diag_age_days + 365),
         age_2years_postdiag = as.numeric(diag_age_days + 730),
         age_3years_postdiag = as.numeric(diag_age_days + 1095),
         age_4years_postdiag = as.numeric(diag_age_days + 1460),
         age_5years_postdiag = as.numeric(diag_age_days + 1825)) |>
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
    age_12months_postdiag = categorise_age(age_12months_postdiag),
    age_2years_postdiag = categorise_age(age_2years_postdiag),
    age_3years_postdiag = categorise_age(age_3years_postdiag),
    age_4years_postdiag = categorise_age(age_4years_postdiag),
    age_5years_postdiag = categorise_age(age_5years_postdiag)) |>


  #ethnicity 
  mutate(ethnicity = case_when(ethnicity %in% c("A", "B", "C", "0") ~ "White",
                               ethnicity %in% c("D", "E", "F", "G") ~ "Mixed",
                               ethnicity %in% c("H", "J", "K", "L") ~ "Asian",
                               ethnicity %in% c("M", "N", "P") ~ "Black",
                               ethnicity == "R" ~ "Chinese", 
                               ethnicity == "S" ~ "Other",
                               ethnicity %in% c("Z", "X") ~ "Not known", TRUE ~ "Not known"))


#stage at diagnosis
source("stage_functions_tidy.R") #stage functions provided by Chloe Bright 

#trans patient fix provided by Chloe Bright
cohort_clean <- cohort_clean |> mutate(
  stage_pi_detail = if_else(
    condition = (
      ((gender == 2 & site_icd10r4_o2_3char_from2013 %in% c("C60", "C61", "C62", "C63")) |
         (gender == 1 & site_icd10r4_o2_3char_from2013 %in% c("C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58"))
      ) &
        !(stage_best == "?" | is.na(stage_best))
    ),
    true = "Y",
    false = stage_pi_detail
  )
)

cohort_clean <- stage_table(cohort_clean) 


#removing unnecessary variables 
cohort_clean <- cohort_clean |>
  select(-c(stage_best_system, stage_best, stage_pi, stage_pi_detail, 
            site_icd10r4_o2_3char_from2013, SUMMARY_STAGE, EARLY_ADVANCED_STAGE, 
            diagnosisdatebest, birthdatebest, diag_age_days, rank))


##### DENOMINATOR DATA FRAMES BY TIME PERIOD, AGE GROUP AND EACH CHARACTERISTIC #######
time_intervals <- c("12months", "2years", "3years", "4years", "5years")

cohort_clean <- left_join(cohort_clean, cohort_survival, by = "patientid")

#function to create a data frame of patients who are alive at the end of a time period, by age group
create_survival_cohort <- function(data, age_variable, alive_variable, period) {
  data |>
    select(patientid, !!age_variable, !!alive_variable) |>
    filter(!!alive_variable == "Yes") |>
    group_by(!!age_variable) |>
    summarise(number_alive_at_period_end = n()) |>
    rename("age_group" := !!age_variable) |>
    mutate(period = period)
}

#function to run the create_survival_cohort function across all the time periods in the time_intervals object
#after first filtering to each value of a specified characteristic variable 
#so we end up with a data frame of the number of patients alive at the end of each time period, for each value of the characteristic
generate_survival_cohorts <- function(data, time_intervals, char_variable) {
  
  #get unique values of the characteristic of interest
  unique_values <- unique(data[[char_variable]])
  
  #empty list to store results 
  all_cohorts <- list()
  
  for (char_value in unique_values) {
    filtered_data <- data |> 
      filter(!!rlang::sym(char_variable) == char_value)
    
    #generate the survival cohort for each time interval
    cohorts_for_characteristic <- lapply(time_intervals, function(interval) {
      age_variable <- sym(paste0("age_", interval, "_postdiag"))
      alive_variable <- sym(paste0("alive_", interval))
      period <- interval
      
      create_survival_cohort(filtered_data, age_variable, alive_variable, period)
    })
    
    #combine the cohorts for this value of the characteristic
    combined_cohort <- bind_rows(cohorts_for_characteristic, .id = "time_interval")
    combined_cohort <- combined_cohort |>
      mutate(characteristic = char_value)
    
    #store the combined cohort in the list
    all_cohorts[[char_value]] <- combined_cohort
  }
  
  combined_all_cohorts <- bind_rows(all_cohorts, .id = "char_value")
  
  #name the output with the characteristic variable name 
  output_name <- paste0("combined_survival_cohort_", char_variable)
  assign(output_name, combined_all_cohorts, envir = .GlobalEnv)
  
  return(combined_all_cohorts)

}

#specify a characteristic to generate age-specific survival cohorts for
char_variable <- "ndrs_main"

#run survival cohorts by age for a specified characteristic variable
combined_survival_cohort <- generate_survival_cohorts(cohort_clean, time_intervals, char_variable)




