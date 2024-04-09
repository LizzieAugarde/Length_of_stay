###################### Length of stay 2023 ######################

#Script to analyse HES A&E data for LOS calculations

#Created January 2024 by Lizzie Augarde 
#Change log:
#07/03/2024 updated methods to include cumulative and alternative LOS measurement
#and rates calculations
############################################################### 

require(PHEindicatormethods)
library(PHEindicatormethods)

time_intervals <- c("3months", "6months", "9months", "12months", "1.5years", "2years", "2.5years", "3years", "3.5years", "4years", "4.5years", "5years")

##### DENOMINATOR DATA FRAME - NUMBER OF PATIENTS ALIVE AT EACH TIME PERIOD ----------------
create_survival_cohort <- function(data, variable, alive_variable, period) {
  data %>%
    select(patientid, !!variable, !!alive_variable) %>%
    filter(!!alive_variable == "Yes") %>%
    group_by(!!variable) %>%
    summarise(number_alive_at_period_end = n()) %>%
    rename("age_group" := !!variable) %>%
    mutate(period = period)
} #creates data frame of patients alive by age group at end of a specified period

survival_cohorts <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  alive_variable <- sym(paste0("alive_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_survival_cohort(los2014_ae_patient_agg, age_variable, alive_variable, period)
}) #runs create_survival_cohort across all time periods in a specified data frame

names(survival_cohorts) <- paste0("survival_cohort_", time_intervals)
list2env(survival_cohorts, envir = .GlobalEnv)
rm(survival_cohorts)
survival_cohort_objects <- ls(pattern = "^survival_cohort")
survival_cohort_list <- mget(survival_cohort_objects)
combined_survival_cohort <- do.call(rbind, survival_cohort_list)

##### PERIOD-SPECIFIC ATTENDANCES BY TIME PERIOD #####
#i.e. atts 6 months is those occurring between 3 and 6 months post diag
create_ps_total_atts <- function(data, age_variable, atts_variable, period) {
  data %>%
    mutate(!!atts_variable := ifelse(is.na(!!atts_variable), 0, !!atts_variable)) %>%
    select(patientid, !!age_variable, !!atts_variable) %>%
    group_by(!!age_variable) %>%
    summarise(atts_in_period = sum(!!atts_variable)) %>%
    rename("age_group" := !!age_variable) %>%
    mutate(period = period)
} #creates data frame of attendances by age group in a specified period

ps_total_attends <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  atts_variable <- sym(paste0("ps_att_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_ps_total_atts(los2014_ae_patient_agg, age_variable, atts_variable, period)
}) #runs create_ps_total_admissions across all time periods in a specified data frame

names(ps_total_attends) <- paste0("ps_total_atts", time_intervals)
list2env(ps_total_attends, envir = .GlobalEnv)
rm(ps_total_attends)
ps_total_attends_objects <- ls(pattern = "^ps_total_atts")
ps_total_attends_list <- mget(ps_total_attends_objects)
combined_ps_total_attends <- do.call(rbind, ps_total_attends_list)


##### CUMULATIVE ATTENDANCES BY TIME PERIOD #####
create_cum_total_atts <- function(data, age_variable, atts_variable, period) {
  data %>%
    mutate(!!atts_variable := ifelse(is.na(!!atts_variable), 0, !!atts_variable)) %>%
    select(patientid, !!age_variable, !!atts_variable) %>%
    group_by(!!age_variable) %>%
    summarise(atts_in_period = sum(!!atts_variable)) %>%
    rename("age_group" := !!age_variable) %>%
    mutate(period = period)
} #creates data frame of attendances by age group in a specified period

cum_total_attends <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  atts_variable <- sym(paste0("cum_att_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_cum_total_atts(los2014_ae_patient_agg, age_variable, atts_variable, period)
}) #runs create_cum_total_admissions across all time periods in a specified data frame

names(cum_total_attends) <- paste0("cum_total_atts", time_intervals)
list2env(cum_total_attends, envir = .GlobalEnv)
rm(cum_total_attends)
cum_total_attends_objects <- ls(pattern = "^cum_total_atts")
cum_total_attends_list <- mget(cum_total_attends_objects)
combined_cum_total_attends <- do.call(rbind, cum_total_attends_list)

##### ATTENDANCES PER PATIENT BY TIME PERIOD #####
#period-specific
ae_atts_per_patient <- left_join(combined_ps_total_attends, combined_survival_cohort, by = c("age_group", "period"))

ae_atts_per_patient <- ae_atts_per_patient %>%
  phe_rate(., atts_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals))

write.csv(ae_atts_per_patient, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/AE ps attends per patient by age 20240409.csv")  

ae_atts_per_patient_plot <- ggplot(ae_atts_per_patient, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  geom_point(data = gen_pop_ae, aes(x = period, y = rate, fill = age_group), 
             position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  scale_y_continuous(limits = c(0, 0.8), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  labs(x = "Time post-diagnosis", y = "Attendances per patient", fill = "Age group",
       caption = "Bars indicate period-specific attendances in those diagnosed with cancer in 2014 i.e. '6 months' refers to those occurring between 3 and 6 months post-diagnosis.
       \nCircles indicate average attendances in the general population between 2013/14 and 2019/20",
       title = "A&E attendances per patient in the cancer population and general population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

#cumulative
ae_atts_per_patient <- left_join(combined_cum_total_attends, combined_survival_cohort, by = c("age_group", "period"))

ae_atts_per_patient <- ae_atts_per_patient %>%
  phe_rate(., atts_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals))

write.csv(ae_atts_per_patient, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/AE cum attends per patient by age 20240409.csv")  

ae_atts_per_patient_plot <- ggplot(ae_atts_per_patient, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  geom_point(data = gen_pop_ae, aes(x = period, y = rate, fill = age_group), 
             position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  scale_y_continuous(limits = c(0, 10), breaks = c(0,2, 4, 6, 8, 10)) +
  labs(x = "Time post-diagnosis", y = "Attendances per patient", fill = "Age group",
       caption = "Bars indicate period-specific attendances in those diagnosed with cancer in 2014 i.e. '6 months' refers to those occurring between 3 and 6 months post-diagnosis.
       \nCircles indicate average attendances in the general population between 2013/14 and 2019/20",
       title = "A&E attendances per patient in the cancer population and general population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))



