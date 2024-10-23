###################### Length of stay 2023 ######################

#Script to generate annual age-specific admissions rates in the general population from published HES data
#Inpatient https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity
#Outpatient https://digital.nhs.uk/data-and-information/publications/statistical/hospital-outpatient-activity
#A&E	https://digital.nhs.uk/data-and-information/publications/statistical/hospital-accident--emergency-activity/2017-18
#Population data from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales

#Rates are calculated on financial year HES data and calendar year population data, not ideal but ok for now 

#June 2024 by Lizzie Augarde 
############################################################### 


##### PREP AND READ IN RAW DATA #####
library(PHEindicatormethods)
library(xlsx)

#population size by age group in 2013 to 2019
pop <- read.xlsx("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/General population size and HES data 20240308.xlsx", sheetName = "Population", colIndex = 1:8)

#number of admissions/appointments/attendances by each group in financial years 2013/14 to 2019/20
gen_pop_apc <- read.xlsx("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/General population size and HES data 20240308.xlsx", sheetName = "Inpatient admissions")
gen_pop_op <- read.xlsx("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/General population size and HES data 20240308.xlsx", sheetName = "Outpatient appointments")
gen_pop_ae <- read.xlsx("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/General population size and HES data 20240308.xlsx", sheetName = "A&E attendances")

#population size - denominator
pop <- pop %>%
  mutate(mean_pop = rowMeans(select(., starts_with("X2")))) %>% #annual mean population by age group across the time period
  select(age_group, mean_pop) %>%
  filter(!is.na(age_group))

#inpatient admissions - numerator
gen_pop_apc <- gen_pop_apc %>%
  mutate(mean_adms = rowMeans(select(., starts_with("X2")))) %>% #annual mean admissions by age group across the time period 
  select(age_group, mean_adms) %>%
  filter(!is.na(age_group))

#outpatient appointments - numerator
gen_pop_op <- gen_pop_op %>%
  mutate(mean_appts = rowMeans(select(., starts_with("X2")))) %>% #annual mean appointments by age group across the time period 
  select(age_group, mean_appts) %>%
  filter(!is.na(age_group))

#A&E attendances - numerator
gen_pop_ae <- gen_pop_ae %>%
  mutate(mean_atts = rowMeans(select(., starts_with("X2")))) %>% #annual mean attendances by age group across the time period 
  select(age_group, mean_atts) %>%
  filter(!is.na(age_group))


###### ANNUAL AVERAGE AGE-SPECIFIC RATES #######
gen_pop_apc <- left_join(gen_pop_apc, pop, by = c("age_group"))  %>%
  phe_rate(., mean_adms, mean_pop, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value")

gen_pop_op <- left_join(gen_pop_op, pop, by = c("age_group"))  %>%
  phe_rate(., mean_appts, mean_pop, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value")

gen_pop_ae <- left_join(gen_pop_ae, pop, by = c("age_group")) %>%
  phe_rate(., mean_atts, mean_pop, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value")
  
#creating repeating data frames to allow for overlaying on the graphs
gen_pop_apc <- gen_pop_apc[rep(seq_len(nrow(gen_pop_apc)), each = 5),]
gen_pop_op <- gen_pop_op[rep(seq_len(nrow(gen_pop_op)), each = 5),]
gen_pop_ae <- gen_pop_ae[rep(seq_len(nrow(gen_pop_ae)), each = 5),]

#creating a period variable to match the cancer population data frame
gen_pop_ae$period <- rep(c("1 year", "2 years", "3 years", "4 years", "5 years"), times = 9)
gen_pop_op$period <- rep(c("1 year", "2 years", "3 years", "4 years", "5 years"), times = 9)
gen_pop_apc$period <- rep(c("1 year", "2 years", "3 years", "4 years", "5 years"), times = 9)
