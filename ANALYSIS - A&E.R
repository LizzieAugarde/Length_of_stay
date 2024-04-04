###################### Length of stay 2023 ######################

#Script to analyse HES A&E data for LOS calculations

#Created January 2024 by Lizzie Augarde 
#Change log:
#07/03/2024 updated methods to include cumulative and alternative LOS measurement
#and rates calculations
############################################################### 

require(PHEindicatormethods)
library(PHEindicatormethods)

##### DENOMINATOR DATA FRAME - NUMBER OF PATIENTS ALIVE AT EACH TIME PERIOD ##### 
survival_cohort_1yr <- los2014_ae_patient_agg %>% 
  select(patientid, age_1yr_postdiag, alive_12months) %>%
  filter(alive_12months == "Yes") %>%
  group_by(age_1yr_postdiag) %>%
  summarize(number_alive_at_period_end = n()) %>%
  rename("age_group" = "age_1yr_postdiag") %>%
  mutate(period = "1 year")

survival_cohort_2yrs <- los2014_ae_patient_agg %>% 
  select(patientid, age_2yrs_postdiag, alive_2years) %>%
  filter(alive_2years == "Yes") %>%
  group_by(age_2yrs_postdiag) %>%
  summarize(number_alive_at_period_end = n()) %>%
  rename("age_group" = "age_2yrs_postdiag") %>%
  mutate(period = "2 years")

survival_cohort_5yrs <- los2014_ae_patient_agg %>% 
  select(patientid, age_5yrs_postdiag, alive_5years) %>%
  filter(alive_5years == "Yes") %>%
  group_by(age_5yrs_postdiag) %>%
  summarize(number_alive_at_period_end = n()) %>%
  rename("age_group" = "age_5yrs_postdiag") %>%
  mutate(period = "5 years")

survival_cohort <- rbind(survival_cohort_1yr, survival_cohort_2yrs, survival_cohort_5yrs)


##### TOTAL ATTENDANCES BY TIME PERIOD #####
#attends occurring from diag to end of current e.g. up to 6 months
total_atts_1yr <- los2014_ae_patient_agg %>% 
  select(patientid, age_1yr_postdiag, att_12months) %>%
  group_by(age_1yr_postdiag) %>%
  summarize(atts_in_period = sum(att_12months)) %>%
  rename("age_group" = "age_1yr_postdiag") %>%
  mutate(period = "1 year")

total_atts_2yrs <- los2014_ae_patient_agg %>% 
  select(patientid, age_2yrs_postdiag, att_2years) %>%
  group_by(age_2yrs_postdiag) %>%
  summarize(atts_in_period = sum(att_2years)) %>%
  rename("age_group" = "age_2yrs_postdiag") %>%
  mutate(period = "2 years")

total_atts_5yrs <- los2014_ae_patient_agg %>% 
  select(patientid, age_5yrs_postdiag, att_5years) %>%
  group_by(age_5yrs_postdiag) %>%
  summarize(atts_in_period = sum(att_5years)) %>%
  rename("age_group" = "age_5yrs_postdiag") %>%
  mutate(period = "5 years")

total_atts <- rbind(total_atts_1yr, total_atts_2yrs, total_atts_5yrs)


##### ATTENDANCES PER PATIENT BY TIME PERIOD #####
ae_los_per_patient <- left_join(total_atts, survival_cohort, by = c("age_group", "period"))

ae_los_per_patient <- ae_los_per_patient %>%
  phe_rate(., atts_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value")

write.csv(ae_los_per_patient, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/A&E LOS per patient by age 20240402.csv")  

#plot
ae_los_per_patient_plot <- ggplot(ae_los_per_patient, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  geom_point(data = gen_pop_ae, aes(x = period, y = rate, fill = age_group), 
             position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "Time post-diagnosis", y = "Attendances per patient", fill = "Age group",
       caption = "Bars indicate attendances in those diagnosed with cancer in 2014. \nCircles indicate average attendances in the general population between 2013/14 and 2019/20",
       title = "A&E attendances per patient in the cancer population and general population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))


