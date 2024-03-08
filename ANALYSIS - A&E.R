###################### Length of stay 2023 ######################

#Script to analyse HES A&E data for LOS calculations

#Created January 2024 by Lizzie Augarde 
#Change log:
#07/03/2024 updated methods to include cumulative and alternative LOS measurement
#and rates calculations
############################################################### 

require(PHEindicatormethods)
library(PHEindicatormethods)

periods_order <- c("3months", "6months", "9months", "12months", "1.5years", "2years", "2.5years", "3years", "3.5years", "4years", "4.5years", "5years")

##### DENOMINATOR DATA FRAME - NUMBER OF PATIENTS ALIVE AT EACH TIME PERIOD ##### 
survival_cohorts <- los2014_ae_patient_agg %>% 
  select(-starts_with("att")) %>%
  pivot_longer(-patientid, names_to = "time_period", values_to = "alive") %>%
  group_by(time_period, alive) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = alive, values_from = count, values_fill = 0) %>%
  mutate(time_period = sub(".*_", "", time_period)) 


##### CUMULATIVE TOTAL ATTENDANCES BY TIME PERIOD #####
#attends occurring from diag to end of current e.g. up to 6 months
cumulative_total_los <- los2014_ae_patient_agg %>% 
  select(-starts_with("alive")) %>%
  pivot_longer(-patientid, names_to = "time_period", values_to = "cum_los") %>%
  mutate(cum_los = ifelse(is.na(cum_los), 0, cum_los)) %>%
  group_by(time_period) %>%
  summarize(cum_los = sum(cum_los)) %>%
  mutate(time_period = sub(".*_", "", time_period))


##### ALTERNATIVE TOTAL ATTENDANCES BY TIME PERIOD #####
#attends occurring between end of previous time period and end of current e.g. between 3 and 6 months
alternative_total_los <- cumulative_total_los %>% 
  mutate(time_period = factor(time_period, levels = periods_order)) %>%
  arrange(time_period) %>%
  mutate(los = c(cum_los[1], diff(cum_los))) %>%
  select(-cum_los)


##### ATTENDANCES PER PATIENT BY TIME PERIOD #####
ae_los_per_patient <- left_join(alternative_total_los, cumulative_total_los, by = "time_period") %>%
  left_join(., survival_cohorts, by = "time_period") %>%
  select(-No) %>%
  rename("patients_alive" = "Yes") %>%
  phe_rate(., cum_los, patients_alive, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate_cum" = "value", "lowerci_ratecum" = "lowercl", "upperci_ratecum" = "uppercl") %>%
  mutate(time_period = factor(time_period, levels = periods_order)) %>%
  arrange(time_period) %>%
  phe_rate(., los, patients_alive, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value", "lowerci_rate" = "lowercl", "upperci_rate" = "uppercl")

ae_cum_los_per_patient_plot <- ggplot(ae_los_per_patient, aes(x = time_period, y = rate_cum, group = 1)) + 
  geom_bar(stat = "identity") 

ae_los_per_patient_plot <-ggplot(ae_los_per_patient, aes(x = time_period, y = rate, group = 1)) + 
  geom_bar(stat = "identity")

write.csv(ae_los_per_patient, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/A&E LOS per patient 20240307.csv")  


