###################### Length of stay 2023 ######################

#Script to analyse HES outpatient data for LOS calculations

#Created March 2024 by Lizzie Augarde 
#Change log:
############################################################### 

require(PHEindicatormethods)
library(PHEindicatormethods)

##### DENOMINATOR DATA FRAME - NUMBER OF PATIENTS ALIVE AT EACH TIME PERIOD ##### 
survival_cohort_1yr <- los2014_op_patient_agg %>% 
  select(patientid, age_1yr_postdiag, alive_12months) %>%
  filter(alive_12months == "Yes") %>%
  group_by(age_1yr_postdiag) %>%
  summarize(number_alive_at_period_end = n()) %>%
  rename("age_group" = "age_1yr_postdiag") %>%
  mutate(period = "1 year")

survival_cohort_2yrs <- los2014_op_patient_agg %>% 
  select(patientid, age_2yrs_postdiag, alive_2years) %>%
  filter(alive_2years == "Yes") %>%
  group_by(age_2yrs_postdiag) %>%
  summarize(number_alive_at_period_end = n()) %>%
  rename("age_group" = "age_2yrs_postdiag") %>%
  mutate(period = "2 years")

survival_cohort_5yrs <- los2014_op_patient_agg %>% 
  select(patientid, age_5yrs_postdiag, alive_5years) %>%
  filter(alive_5years == "Yes") %>%
  group_by(age_5yrs_postdiag) %>%
  summarize(number_alive_at_period_end = n()) %>%
  rename("age_group" = "age_5yrs_postdiag") %>%
  mutate(period = "5 years")

survival_cohort <- rbind(survival_cohort_1yr, survival_cohort_2yrs, survival_cohort_5yrs)


##### TOTAL APPOINTMENTS BY TIME PERIOD #####
#appts occurring from diag to end of current e.g. up to 6 months
total_appts_1yr <- los2014_op_patient_agg %>% 
  select(patientid, age_1yr_postdiag, appt_12months) %>%
  group_by(age_1yr_postdiag) %>%
  summarize(appts_in_period = sum(appt_12months)) %>%
  rename("age_group" = "age_1yr_postdiag") %>%
  mutate(period = "1 year")

total_appts_2yrs <- los2014_op_patient_agg %>% 
  select(patientid, age_2yrs_postdiag, appt_2years) %>%
  group_by(age_2yrs_postdiag) %>%
  summarize(appts_in_period = sum(appt_2years)) %>%
  rename("age_group" = "age_2yrs_postdiag") %>%
  mutate(period = "2 years")

total_appts_5yrs <- los2014_op_patient_agg %>% 
  select(patientid, age_5yrs_postdiag, appt_5years) %>%
  group_by(age_5yrs_postdiag) %>%
  summarize(appts_in_period = sum(appt_5years)) %>%
  rename("age_group" = "age_5yrs_postdiag") %>%
  mutate(period = "5 years")

total_appts <- rbind(total_appts_1yr, total_appts_2yrs, total_appts_5yrs)


##### APPOINTMENTS PER PATIENT BY TIME PERIOD #####
op_los_per_patient <- left_join(total_appts, survival_cohort, by = c("age_group", "period"))

op_los_per_patient <- op_los_per_patient %>%
  phe_rate(., appts_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value")

write.csv(op_los_per_patient, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/OP LOS per patient by age 20240402.csv")  

#plot
op_los_per_patient_plot <- ggplot(op_los_per_patient, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  geom_point(data = gen_pop_op, aes(x = period, y = rate, fill = age_group), 
             position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  scale_y_continuous(limits = c(0, 80), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  labs(x = "Time post-diagnosis", y = "Appointments per patient", fill = "Age group",
       caption = "Bars indicate appointments in those diagnosed with cancer in 2014. \nCircles indicate average attendances in the general population between 2013/14 and 2019/20",
       title = "Outpatient appointments per patient in the cancer population and general population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))


