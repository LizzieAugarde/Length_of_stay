###################### Length of stay 2023 ######################

#Script to analyse HES A&E data for LOS calculations

#Created January 2024 by Lizzie Augarde 
#Change log:
############################################################### 


#denominator data frame (number of patients alive at each time period)
survival_cohorts <- los2014_ae_patient_agg_months %>% 
  select(-starts_with("att")) %>%
  pivot_longer(-patientid, names_to = "time_period", values_to = "alive") %>%
  group_by(time_period, alive) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = alive, values_from = count, values_fill = 0)

#numerator data frame (number of patients alive at each time period)
attendances_by_time_period <- los2014_ae_patient_agg_months %>%
  mutate(dummy = "dummy") %>%
  group_by(dummy) %>%
  summarize(att_3months = sum(att_3months), att_6months = sum(att_6months), att_9months = sum(att_9months), att_12months = sum(att_12months), 
            att_1.5years = sum(att_1.5years), att_2years = sum(att_2years), 
            att_2.5years = sum(att_2.5years), att_3years = sum(att_3years),
            att_3.5years = sum(att_3.5years), att_4years = sum(att_4years),
            att_4.5years = sum(att_4.5years), att_5years = sum(att_5years)) %>%
  pivot_longer(-dummy, names_to = "time_period", values_to = "number_attendances") %>%
  select(-dummy)