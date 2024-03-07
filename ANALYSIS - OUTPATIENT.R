###################### Length of stay 2023 ######################

#Script to analyse HES outpatient data for LOS calculations

#Created March 2024 by Lizzie Augarde 
#Change log:
############################################################### 

require(PHEindicatormethods)
library(PHEindicatormethods)

##### DENOMINATOR DATA FRAME - NUMBER OF PATIENTS ALIVE AT EACH TIME PERIOD ##### 
survival_cohorts <- los2014_op_patient_agg %>% 
  select(-starts_with("appt")) %>%
  pivot_longer(-patientid, names_to = "time_period", values_to = "alive") %>%
  group_by(time_period, alive) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = alive, values_from = count, values_fill = 0)


##### TOTAL APPOINTMENTS BY TIME PERIOD #####
total_los <- los2014_op_patient_agg %>% 
  select(-starts_with("alive")) %>%
  pivot_longer(-patientid, names_to = "time_period", values_to = "los") %>%
  mutate(los = ifelse(is.na(los), 0, los)) %>%
  group_by(time_period) %>%
  summarize(los = sum(los)) 

##### APPOINTMENTS PER PATIENT BY TIME PERIOD #####
survival_cohorts <- survival_cohorts %>%
  mutate(time_period = sub(".*_", "", time_period)) 

total_los <- total_los %>%
  mutate(time_period = sub(".*_", "", time_period)) 

periods_order <- c("3months", "6months", "9months", "12months", "1.5years", "2years", "2.5years", "3years", "3.5years", "4years", "4.5years", "5years")

los_per_patient <- left_join(total_los, survival_cohorts, by = "time_period") %>%
  select(-No) %>%
  rename("patients_alive" = "Yes") %>%
  phe_rate(., los, patients_alive, type = "standard", confidence = 0.95, multiplier = 1) %>%
  mutate(time_period = factor(time_period, levels = periods_order)) %>%
  arrange(time_period)

write.csv(los_per_patient, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/OP LOS per patient 20240307.csv")  

ggplot(los_per_patient, aes(x = time_period, y = value, group = 1)) + 
  geom_bar(stat = "identity")
  