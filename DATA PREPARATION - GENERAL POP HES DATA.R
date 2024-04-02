###################### Length of stay 2023 ######################

#Script to generate LOS rates in the general population from HES data
#HES data from:
#- A&E	https://digital.nhs.uk/data-and-information/publications/statistical/hospital-accident--emergency-activity/2017-18
#- Inpatient 
#- Outpatient

#Created April 2024 by Lizzie Augarde 
#Change log:
############################################################### 

require(PHEindicatormethods)
library(PHEindicatormethods)
library(xlsx)

pop <- read.xlsx("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/General population size and HES data 20240308.xlsx", sheetName = "Population", colIndex = 1:8)
gen_pop_ae <- read.xlsx("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/General population size and HES data 20240308.xlsx", sheetName = "A&E attendances")
gen_pop_op<- read.xlsx("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/General population size and HES data 20240308.xlsx", sheetName = "Outpatient appointments")

pop <- pop %>%
  mutate(mean_pop = rowMeans(select(., starts_with("X2")))) %>%
  select(age_group, mean_pop) %>%
  filter(!is.na(age_group))

gen_pop_ae <- gen_pop_ae %>%
  mutate(mean_atts = rowMeans(select(., starts_with("X2")))) %>%
  select(age_group, mean_atts) %>%
  filter(!is.na(age_group))

gen_pop_op <- gen_pop_op %>%
  mutate(mean_appts = rowMeans(select(., starts_with("X2")))) %>%
  select(age_group, mean_appts) %>%
  filter(!is.na(age_group))

gen_pop_ae <- left_join(gen_pop_ae, pop, by = c("age_group")) %>%
  phe_rate(., mean_atts, mean_pop, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value")
  
gen_pop_op <- left_join(gen_pop_op, pop, by = c("age_group"))  %>%
  phe_rate(., mean_appts, mean_pop, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value")

write.xlsx(gen_pop_ae, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/General population rates 20240402.xlsx", sheetName = "A&E")
write.xlsx(gen_pop_op, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/General population rates 20240402.xlsx", sheetName = "Outpatient", append = TRUE)

#creating repeating data frames to allow for overlaying on cancer population graphs
gen_pop_ae <- gen_pop_ae[rep(seq_len(nrow(gen_pop_ae)), each = 3),]
gen_pop_op <- gen_pop_op[rep(seq_len(nrow(gen_pop_op)), each = 3),]

gen_pop_ae$period <- rep(c("1 year", "2 years", "5 years"), times = 9)
gen_pop_op$period <- rep(c("1 year", "2 years", "5 years"), times = 9)
