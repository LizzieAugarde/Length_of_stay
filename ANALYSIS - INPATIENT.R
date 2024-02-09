###################### Length of stay 2023 ######################

#Script to analyse HES APC data for LOS calculations

#Created February 2024 by Lizzie Augarde 
#Change log:
############################################################### 


##### DENOMINATOR DATA FRAME - NUMBER OF PATIENTS ALIVE AT EACH TIME PERIOD ##### 
survival_cohorts <- los2014_apc_patient_agg %>% 
  select(-starts_with("total")) %>%
  pivot_longer(-patientid, names_to = "time_period", values_to = "alive") %>%
  group_by(time_period, alive) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = alive, values_from = count, values_fill = 0)


##### 
