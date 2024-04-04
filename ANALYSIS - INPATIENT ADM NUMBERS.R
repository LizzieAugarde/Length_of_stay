###################### Length of stay 2023 ######################

#Script to analyse HES APC data for number of admissions calculations
#ie for comparison to general population rather than LOS

#Created April 2024 by Lizzie Augarde 
#Change log:

############################################################### 

#uses cleaned episode record-level APC data (line 196 in DATA PREPARATION - INPATIENT.R) and survival data frame 

apc_adms_patient_agg <- los2014_apc_events %>%
  group_by(patientid, episode_cancer_related) %>%
  summarize(total_adms_12months = sum(ifelse(los_12months > 0, 1, 0)),
            total_adms_2years = sum(ifelse(los_2years > 0, 1, 0)),
            total_adms_5years = sum(ifelse(los_5years > 0, 1, 0))) %>%
  
  #adding survival for each patient
  left_join(select(los2014_apc_patients_survival, patientid, alive_3months, alive_6months, alive_9months, alive_12months, alive_1.5years, alive_2years, 
                   alive_2.5years, alive_3years, alive_3.5years, alive_4years, alive_4.5years, alive_5years), by = "patientid")


##### ADDING VARIABLES NEEDED FOR AGE BREAKDOWNS #####   
apc_adms_patient_agg <- apc_adms_patient_agg %>%
  mutate(patientid = as.character(patientid)) %>%
  
  #adding in DOB and calculating age at diag
  left_join(select(los2014_cohort, patientid, diagnosisdatebest, birthdatebest), by = "patientid") %>%
  mutate(diag_age_days = difftime(as.Date(diagnosisdatebest), as.Date(birthdatebest), units = "days")) %>%
  
  #ageing on for 1, 2 and 5 years post-diagnosis 
  mutate(age_1yr_postdiag = as.numeric(diag_age_days + 365),
         age_2yrs_postdiag = as.numeric(diag_age_days + 730),
         age_5yrs_postdiag = as.numeric(diag_age_days + 1825)) %>%
  mutate(age_1yr_postdiag = floor(age_1yr_postdiag/365),
         age_2yrs_postdiag = floor(age_2yrs_postdiag/365),
         age_5yrs_postdiag = floor(age_5yrs_postdiag/365)) %>%
  
  #converting to age groups
  mutate(age_1yr_postdiag = case_when(age_1yr_postdiag < 10 ~ "0-9",
                                      age_1yr_postdiag < 20 & age_1yr_postdiag > 9 ~ "10-19",
                                      age_1yr_postdiag < 30 & age_1yr_postdiag > 19 ~ "20-29",
                                      age_1yr_postdiag < 40 & age_1yr_postdiag > 29 ~ "30-39",
                                      age_1yr_postdiag < 50 & age_1yr_postdiag > 39 ~ "40-49",
                                      age_1yr_postdiag < 60 & age_1yr_postdiag > 49 ~ "50-59",
                                      age_1yr_postdiag < 70 & age_1yr_postdiag > 59 ~ "60-69",
                                      age_1yr_postdiag < 80 & age_1yr_postdiag > 69 ~ "70-79",
                                      age_1yr_postdiag > 79 ~ "80+")) %>%
  mutate(age_2yrs_postdiag = case_when(age_2yrs_postdiag < 10 ~ "0-9",
                                       age_2yrs_postdiag < 20 & age_2yrs_postdiag > 9 ~ "10-19",
                                       age_2yrs_postdiag < 30 & age_2yrs_postdiag > 19 ~ "20-29",
                                       age_2yrs_postdiag < 40 & age_2yrs_postdiag > 29 ~ "30-39",
                                       age_2yrs_postdiag < 50 & age_2yrs_postdiag > 39 ~ "40-49",
                                       age_2yrs_postdiag < 60 & age_2yrs_postdiag > 49 ~ "50-59",
                                       age_2yrs_postdiag < 70 & age_2yrs_postdiag > 59 ~ "60-69",
                                       age_2yrs_postdiag < 80 & age_2yrs_postdiag > 69 ~ "70-79",
                                       age_2yrs_postdiag > 79 ~ "80+")) %>%
  mutate(age_5yrs_postdiag = case_when(age_5yrs_postdiag < 10 ~ "0-9",
                                       age_5yrs_postdiag < 20 & age_5yrs_postdiag > 9 ~ "10-19",
                                       age_5yrs_postdiag < 30 & age_5yrs_postdiag > 19 ~ "20-29",
                                       age_5yrs_postdiag < 40 & age_5yrs_postdiag > 29 ~ "30-39",
                                       age_5yrs_postdiag < 50 & age_5yrs_postdiag > 39 ~ "40-49",
                                       age_5yrs_postdiag < 60 & age_5yrs_postdiag > 49 ~ "50-59",
                                       age_5yrs_postdiag < 70 & age_5yrs_postdiag > 59 ~ "60-69",
                                       age_5yrs_postdiag < 80 & age_5yrs_postdiag > 69 ~ "70-79",
                                       age_5yrs_postdiag > 79 ~ "80+")) %>%
  select(-c(diagnosisdatebest, birthdatebest, diag_age_days))


##### DENOMINATOR DATA FRAME - NUMBER OF PATIENTS ALIVE AT EACH TIME PERIOD ##### 
survival_cohort_1yr <- apc_adms_patient_agg %>% 
  select(patientid, age_1yr_postdiag, alive_12months) %>%
  filter(alive_12months == "Yes") %>%
  group_by(age_1yr_postdiag) %>%
  summarize(number_alive_at_period_end = n()) %>%
  rename("age_group" = "age_1yr_postdiag") %>%
  mutate(period = "1 year")

survival_cohort_2yrs <- apc_adms_patient_agg %>% 
  select(patientid, age_2yrs_postdiag, alive_2years) %>%
  filter(alive_2years == "Yes") %>%
  group_by(age_2yrs_postdiag) %>%
  summarize(number_alive_at_period_end = n()) %>%
  rename("age_group" = "age_2yrs_postdiag") %>%
  mutate(period = "2 years")

survival_cohort_5yrs <- apc_adms_patient_agg %>% 
  select(patientid, age_5yrs_postdiag, alive_5years) %>%
  filter(alive_5years == "Yes") %>%
  group_by(age_5yrs_postdiag) %>%
  summarize(number_alive_at_period_end = n()) %>%
  rename("age_group" = "age_5yrs_postdiag") %>%
  mutate(period = "5 years")

survival_cohort <- rbind(survival_cohort_1yr, survival_cohort_2yrs, survival_cohort_5yrs)


##### TOTAL ADMISSIONS BY TIME PERIOD - ALL ADMISSIONS #####
#admissions starting from diag to end of current e.g. up to 6 months
total_adms_1yr <- apc_adms_patient_agg %>% 
  mutate(total_adms_12months = ifelse(is.na(total_adms_12months), 0, total_adms_12months)) %>%
  select(patientid, age_1yr_postdiag, total_adms_12months) %>%
  group_by(age_1yr_postdiag) %>%
  summarize(adms_in_period = sum(total_adms_12months)) %>%
  rename("age_group" = "age_1yr_postdiag") %>%
  mutate(period = "1 year")

total_adms_2yrs <- apc_adms_patient_agg %>% 
  select(patientid, age_2yrs_postdiag, total_adms_2years) %>%
  mutate(total_adms_2years = ifelse(is.na(total_adms_2years), 0, total_adms_2years)) %>%
  group_by(age_2yrs_postdiag) %>%
  summarize(adms_in_period = sum(total_adms_2years)) %>%
  rename("age_group" = "age_2yrs_postdiag") %>%
  mutate(period = "2 years")

total_adms_5yrs <- apc_adms_patient_agg %>% 
  select(patientid, age_5yrs_postdiag, total_adms_5years) %>%
  mutate(total_adms_5years = ifelse(is.na(total_adms_5years), 0, total_adms_5years)) %>%
  group_by(age_5yrs_postdiag) %>%
  summarize(adms_in_period = sum(total_adms_5years)) %>%
  rename("age_group" = "age_5yrs_postdiag") %>%
  mutate(period = "5 years")

total_adms <- rbind(total_adms_1yr, total_adms_2yrs, total_adms_5yrs)


##### ALL ADMISSIONS PER PATIENT BY TIME PERIOD #####
apc_adms_per_patient <- left_join(total_adms, survival_cohort, by = c("age_group", "period"))

apc_adms_per_patient <- apc_adms_per_patient %>%
  phe_rate(., adms_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value")

write.csv(apc_adms_per_patient, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/APC number admissions per patient by age 20240402.csv")  

#plot
apc_adms_per_patient_plot <- ggplot(apc_adms_per_patient, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  geom_point(data = gen_pop_apc, aes(x = period, y = rate, fill = age_group), 
             position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  #scale_y_continuous(limits = c(0, 10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "Time post-diagnosis", y = "Admissions per patient", fill = "Age group",
       caption = "Bars indicate all admissions in those diagnosed with cancer in 2014. \nCircles indicate average admissions in the general population between 2013/14 and 2019/20",
       title = "All inpatient admissions attendances per patient in the cancer population and general population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))


##### TOTAL ADMISSIONS BY TIME PERIOD - CANCER-RELATED ADMISSIONS #####
#admissions starting from diag to end of current e.g. up to 6 months
total_adms_1yr_cr <- apc_adms_patient_agg %>% 
  mutate(total_adms_12months = ifelse(is.na(total_adms_12months), 0, total_adms_12months)) %>%
  filter(episode_cancer_related == "Y") %>%
  select(patientid, age_1yr_postdiag, total_adms_12months) %>%
  group_by(age_1yr_postdiag) %>%
  summarize(adms_in_period = sum(total_adms_12months)) %>%
  rename("age_group" = "age_1yr_postdiag") %>%
  mutate(period = "1 year")

total_adms_2yrs_cr <- apc_adms_patient_agg %>%
  mutate(total_adms_2years = ifelse(is.na(total_adms_2years), 0, total_adms_2years)) %>%
  filter(episode_cancer_related == "Y") %>%
  select(patientid, age_2yrs_postdiag, total_adms_2years) %>%
  group_by(age_2yrs_postdiag) %>%
  summarize(adms_in_period = sum(total_adms_2years)) %>%
  rename("age_group" = "age_2yrs_postdiag") %>%
  mutate(period = "2 years")

total_adms_5yrs_cr <- apc_adms_patient_agg %>% 
  mutate(total_adms_5years = ifelse(is.na(total_adms_5years), 0, total_adms_5years)) %>%
  filter(episode_cancer_related == "Y") %>%
  select(patientid, age_5yrs_postdiag, total_adms_5years) %>%
  group_by(age_5yrs_postdiag) %>%
  summarize(adms_in_period = sum(total_adms_5years)) %>%
  rename("age_group" = "age_5yrs_postdiag") %>%
  mutate(period = "5 years")

total_adms_cr <- rbind(total_adms_1yr_cr, total_adms_2yrs_cr, total_adms_5yrs_cr)


##### ALL ADMISSIONS PER PATIENT BY TIME PERIOD #####
cr_apc_adms_per_patient <- left_join(total_adms_cr, survival_cohort, by = c("age_group", "period"))

cr_apc_adms_per_patient <- cr_apc_adms_per_patient %>%
  phe_rate(., adms_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value")

write.csv(cr_apc_adms_per_patient, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/APC number CR admissions per patient by age 20240402.csv")  

#plot
cr_apc_adms_per_patient_plot <- ggplot(cr_apc_adms_per_patient, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  geom_point(data = gen_pop_apc, aes(x = period, y = rate, fill = age_group), 
             position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  scale_y_continuous(limits = c(0, 25), breaks = c(0,5,10,15,20,25)) +
  labs(x = "Time post-diagnosis", y = "Admissions per patient", fill = "Age group",
       caption = "Bars indicate cancer-related admissions in those diagnosed with cancer in 2014. \nCircles indicate average admissions in the general population between 2013/14 and 2019/20",
       title = "Cancer-related inpatient admissions attendances per patient\nin the cancer population and all admissions in the general population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

