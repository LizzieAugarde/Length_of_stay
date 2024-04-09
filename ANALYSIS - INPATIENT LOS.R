###################### Length of stay 2023 ######################

#Script to analyse HES APC data for LOS calculations

#Created February 2024 by Lizzie Augarde 
#Change log:
#07/03/2024 updated methods to include cumulative and alternative LOS measurement
#and rates calculations
############################################################### 

require(PHEindicatormethods)
library(PHEindicatormethods)

time_intervals <- c("3months", "6months", "9months", "12months", "1.5years", "2years", "2.5years", "3years", "3.5years", "4years", "4.5years", "5years")

##### DENOMINATOR DATA FRAME - NUMBER OF PATIENTS ALIVE AT EACH TIME PERIOD ##### 
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
  
  create_survival_cohort(los2014_apc_patient_agg_all, age_variable, alive_variable, period)
}) #runs create_survival_cohort across all time periods in a specified data frame

names(survival_cohorts) <- paste0("survival_cohort_", time_intervals)
list2env(survival_cohorts, envir = .GlobalEnv)
rm(survival_cohorts)
survival_cohort_objects <- ls(pattern = "^survival_cohort")
survival_cohort_list <- mget(survival_cohort_objects)
combined_survival_cohort <- do.call(rbind, survival_cohort_list)


####### PERIOD-SPECIFIC LOS #########
#cancer-related admissions 
create_ps_total_los <- function(data, age_variable, los_variable, period) {
  data %>%
    mutate(!!los_variable := ifelse(is.na(!!los_variable), 0, !!los_variable)) %>%
    select(patientid, !!age_variable, !!los_variable) %>%
    group_by(!!age_variable) %>%
    summarise(los_in_period = sum(!!los_variable)) %>%
    rename("age_group" := !!age_variable) %>%
    mutate(period = period)
} #creates data frame of LOS by age group in a specified period

ps_total_los_cr <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  los_variable <- sym(paste0("ps_total_los_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_ps_total_los(los2014_apc_patient_agg_cr, age_variable, los_variable, period)
}) #runs create_ps_total_los across all time periods in a specified data frame

names(ps_total_los_cr) <- paste0("ps_total_los_cr", time_intervals)
list2env(ps_total_los_cr, envir = .GlobalEnv)
rm(ps_total_los_cr)
ps_total_los_cr_objects <- ls(pattern = "^ps_total_los_cr")
ps_total_los_cr_list <- mget(ps_total_los_cr_objects)
combined_ps_total_los_cr <- do.call(rbind, ps_total_los_cr_list)


#all admissions
ps_total_los_all <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  los_variable <- sym(paste0("ps_total_los_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_ps_total_los(los2014_apc_patient_agg_all, age_variable, los_variable, period)
})

names(ps_total_los_all) <- paste0("ps_total_los_all_", time_intervals)
list2env(ps_total_los_all, envir = .GlobalEnv)
rm(ps_total_los_all)
ps_total_los_all_objects <- ls(pattern = "^ps_total_los_all")
ps_total_los_all_list <- mget(ps_total_los_all_objects)
combined_ps_total_los_all <- do.call(rbind, ps_total_los_all_list)






##### CUMULATIVE LOS #####
#cancer-related admissions 
create_cum_total_los <- function(data, age_variable, los_variable, period) {
  data %>%
    mutate(!!los_variable := ifelse(is.na(!!los_variable), 0, !!los_variable)) %>%
    select(patientid, !!age_variable, !!los_variable) %>%
    group_by(!!age_variable) %>%
    summarise(los_in_period = sum(!!los_variable)) %>%
    rename("age_group" := !!age_variable) %>%
    mutate(period = period)
} #creates data frame of los by age group in a specified period

cum_total_los_cr <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  los_variable <- sym(paste0("cum_total_los_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_cum_total_los(los2014_apc_patient_agg_cr, age_variable, los_variable, period)
}) #runs create_cum_total_los across all time periods in a specified data frame

names(cum_total_los_cr) <- paste0("cum_total_los_cr", time_intervals)
list2env(cum_total_los_cr, envir = .GlobalEnv)
rm(cum_total_los_cr)
cum_total_los_cr_objects <- ls(pattern = "^cum_total_los_cr")
cum_total_los_cr_list <- mget(cum_total_los_cr_objects)
combined_cum_total_los_cr <- do.call(rbind, cum_total_los_cr_list)


#all admissions
cum_total_los_all <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  los_variable <- sym(paste0("cum_total_los_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_cum_total_los(los2014_apc_patient_agg_all, age_variable, los_variable, period)
}) #runs create_cum_total_los across all time periods in a specified data frame

names(cum_total_los_all) <- paste0("cum_total_los_all", time_intervals)
list2env(cum_total_los_all, envir = .GlobalEnv)
rm(cum_total_los_all)
cum_total_los_all_objects <- ls(pattern = "^cum_total_los_all")
cum_total_los_all_list <- mget(cum_total_los_all_objects)
combined_cum_total_los_all <- do.call(rbind, cum_total_los_all_list)







##### PERIOD-SPECIFIC LOS PER PATIENT BY TIME PERIOD #####
#cancer-related admissions 
apc_los_per_patient_cr <- left_join(combined_ps_total_los_cr, combined_survival_cohort, by = c("age_group", "period"))

apc_los_per_patient_cr <- apc_los_per_patient_cr %>%
  phe_rate(., los_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals))

write.csv(apc_los_per_patient_cr, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/APC ps CR LOS per patient 20240409.csv")  

apc_los_per_patient_plot_cr <- ggplot(apc_los_per_patient_cr, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  #scale_y_continuous(limits = c(0, 12), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  labs(x = "Time post-diagnosis", y = "Days per patient", fill = "Age group",
       caption = "Bars indicate cancer-related period-specific length of stay in those diagnosed with cancer in 2014, i.e. '6 months' refers to LOS occurring between 3 and 6 months post-diagnosis.",
       title = "Cancer-related inpatient length of stay per patient in the cancer population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))


#all admissions 
apc_los_per_patient_all <- left_join(combined_ps_total_los_all, combined_survival_cohort, by = c("age_group", "period"))

apc_los_per_patient_all <- apc_los_per_patient_all %>%
  phe_rate(., los_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals))

write.csv(apc_los_per_patient_all, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/APC ps all LOS per patient 20240409.csv")  

apc_los_per_patient_plot_all <- ggplot(apc_los_per_patient_all, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  #scale_y_continuous(limits = c(0, 10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "Time post-diagnosis", y = "Admissions per patient", fill = "Age group",
       caption = "Bars indicate all period-specific length of stay in those diagnosed with cancer in 2014, i.e. '6 months' refers to LOS occurring between 3 and 6 months post-diagnosis.",
       title = "All inpatient length of stay per patient in the cancer population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))


##### CUMULATIVE LOS PER PATIENT BY TIME PERIOD #####
#cancer-related admissions 
apc_los_per_patient_cr <- left_join(combined_cum_total_los_cr, combined_survival_cohort, by = c("age_group", "period"))

apc_los_per_patient_cr <- apc_los_per_patient_cr %>%
  phe_rate(., los_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals))

write.csv(apc_los_per_patient_cr, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/APC cum CR LOS per patient 20240409.csv")  

apc_los_per_patient_plot_cr <- ggplot(apc_los_per_patient_cr, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  #scale_y_continuous(limits = c(0, 12), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  labs(x = "Time post-diagnosis", y = "Days per patient", fill = "Age group",
       caption = "Bars indicate cancer-related cumulative length of stay in those diagnosed with cancer in 2014, i.e. '6 months' refers to all LOS occurring within 6 months post-diagnosis.",
       title = "Cancer-related inpatient length of stay per patient in the cancer population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))


#all admissions 
apc_los_per_patient_all <- left_join(combined_cum_total_los_all, combined_survival_cohort, by = c("age_group", "period"))

apc_los_per_patient_all <- apc_los_per_patient_all %>%
  phe_rate(., los_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals))

write.csv(apc_los_per_patient_all, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/APC cum all LOS per patient 20240409.csv")  

apc_los_per_patient_plot_all <- ggplot(apc_los_per_patient_all, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  #scale_y_continuous(limits = c(0, 10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "Time post-diagnosis", y = "Days per patient", fill = "Age group",
       caption = "Bars indicate all cumulative length of stay in those diagnosed with cancer in 2014, i.e. '6 months' refers to all LOS occurring within 6 months post-diagnosis.",
       title = "All inpatient length of stay per patient in the cancer population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

