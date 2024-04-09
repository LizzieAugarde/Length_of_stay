###################### Length of stay 2023 ######################

#Script to analyse HES APC data for number of admissions calculations
#ie for comparison to general population rather than LOS

#Created April 2024 by Lizzie Augarde 
#Change log:

############################################################### 

#uses cleaned episode record-level APC data (line 196 in DATA PREPARATION - INPATIENT.R) and survival data frame 

time_intervals <- c("3months", "6months", "9months", "12months", "1.5years", "2years", "2.5years", "3years", "3.5years", "4years", "4.5years", "5years")

###########DATA PREP-----------
apc_adms_patient_agg_cr <- los2014_apc_events %>%
  filter(episode_cancer_related == "Y") %>%
  group_by(patientid) %>%
  
  #cumulative total admissions
  summarize(cum_total_adms_3months = sum(ifelse(los_3months > 0, 1, 0)), cum_total_adms_6months = sum(ifelse(los_6months > 0, 1, 0)),
            cum_total_adms_9months = sum(ifelse(los_9months > 0, 1, 0)), cum_total_adms_12months = sum(ifelse(los_12months > 0, 1, 0)),
            cum_total_adms_1.5years = sum(ifelse(los_1.5years > 0, 1, 0)), cum_total_adms_2years = sum(ifelse(los_2years > 0, 1, 0)),
            cum_total_adms_2.5years = sum(ifelse(los_2.5years > 0, 1, 0)), cum_total_adms_3years = sum(ifelse(los_3years > 0, 1, 0)),
            cum_total_adms_3.5years = sum(ifelse(los_3.5years > 0, 1, 0)), cum_total_adms_4years = sum(ifelse(los_4years > 0, 1, 0)),
            cum_total_adms_4.5years = sum(ifelse(los_4.5years > 0, 1, 0)),cum_total_adms_5years = sum(ifelse(los_5years > 0, 1, 0)),
            
            #period-specific total admissions (i.e. admissions 6 months is those which occur between 3 and 6 months only)
            ps_total_adms_3months = cum_total_adms_3months, ps_total_adms_6months = cum_total_adms_6months-cum_total_adms_3months, 
            ps_total_adms_9months = cum_total_adms_9months-cum_total_adms_6months, ps_total_adms_12months = cum_total_adms_12months-cum_total_adms_9months, 
            ps_total_adms_1.5years = cum_total_adms_1.5years-cum_total_adms_12months, ps_total_adms_2years = cum_total_adms_2years-cum_total_adms_1.5years, 
            ps_total_adms_2.5years = cum_total_adms_2.5years-cum_total_adms_2years, ps_total_adms_3years = cum_total_adms_3years-cum_total_adms_2.5years, 
            ps_total_adms_3.5years = cum_total_adms_3.5years-cum_total_adms_3years, ps_total_adms_4years = cum_total_adms_4years-cum_total_adms_3.5years, 
            ps_total_adms_4.5years = cum_total_adms_4.5years-cum_total_adms_4years, ps_total_adms_5years = cum_total_adms_5years-cum_total_adms_4.5years) %>%
  
  #adding survival for each patient
  left_join(select(los2014_apc_patients_survival, patientid, alive_3months, alive_6months, alive_9months, alive_12months, alive_1.5years, alive_2years, 
                   alive_2.5years, alive_3years, alive_3.5years, alive_4years, alive_4.5years, alive_5years), by = "patientid")


apc_adms_patient_agg_all <- los2014_apc_events %>%
  group_by(patientid) %>%
  
  #cumulative total admissions
  summarize(cum_total_adms_3months = sum(ifelse(los_3months > 0, 1, 0)), cum_total_adms_6months = sum(ifelse(los_6months > 0, 1, 0)),
            cum_total_adms_9months = sum(ifelse(los_9months > 0, 1, 0)), cum_total_adms_12months = sum(ifelse(los_12months > 0, 1, 0)),
            cum_total_adms_1.5years = sum(ifelse(los_1.5years > 0, 1, 0)), cum_total_adms_2years = sum(ifelse(los_2years > 0, 1, 0)),
            cum_total_adms_2.5years = sum(ifelse(los_2.5years > 0, 1, 0)), cum_total_adms_3years = sum(ifelse(los_3years > 0, 1, 0)),
            cum_total_adms_3.5years = sum(ifelse(los_3.5years > 0, 1, 0)), cum_total_adms_4years = sum(ifelse(los_4years > 0, 1, 0)),
            cum_total_adms_4.5years = sum(ifelse(los_4.5years > 0, 1, 0)),cum_total_adms_5years = sum(ifelse(los_5years > 0, 1, 0)),
            
            #period-specific total admissions (i.e. admissions 6 months is those which occur between 3 and 6 months only)
            ps_total_adms_3months = cum_total_adms_3months, ps_total_adms_6months = cum_total_adms_6months-cum_total_adms_3months, 
            ps_total_adms_9months = cum_total_adms_9months-cum_total_adms_6months, ps_total_adms_12months = cum_total_adms_12months-cum_total_adms_9months, 
            ps_total_adms_1.5years = cum_total_adms_1.5years-cum_total_adms_12months, ps_total_adms_2years = cum_total_adms_2years-cum_total_adms_1.5years, 
            ps_total_adms_2.5years = cum_total_adms_2.5years-cum_total_adms_2years, ps_total_adms_3years = cum_total_adms_3years-cum_total_adms_2.5years, 
            ps_total_adms_3.5years = cum_total_adms_3.5years-cum_total_adms_3years, ps_total_adms_4years = cum_total_adms_4years-cum_total_adms_3.5years, 
            ps_total_adms_4.5years = cum_total_adms_4.5years-cum_total_adms_4years, ps_total_adms_5years = cum_total_adms_5years-cum_total_adms_4.5years) %>%
  
  #adding survival for each patient
  left_join(select(los2014_apc_patients_survival, patientid, alive_3months, alive_6months, alive_9months, alive_12months, alive_1.5years, alive_2years, 
                   alive_2.5years, alive_3years, alive_3.5years, alive_4years, alive_4.5years, alive_5years), by = "patientid")



##### ADDING VARIABLES NEEDED FOR AGE BREAKDOWNS #####   
apc_adms_patient_agg_cr <- apc_adms_patient_agg_cr %>%
  mutate(patientid = as.character(patientid)) %>%
  left_join(., los2014_cohort_agevars, by = "patientid") 

apc_adms_patient_agg_all <- apc_adms_patient_agg_all %>%
  mutate(patientid = as.character(patientid)) %>%
  left_join(., los2014_cohort_agevars, by = "patientid") 



##### DENOMINATOR DATA FRAME - NUMBER OF PATIENTS ALIVE AT EACH TIME PERIOD ----------------
#uses the all admissions data frame as denominator is the same for both 
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
  
  create_survival_cohort(apc_adms_patient_agg_all, age_variable, alive_variable, period)
}) #runs create_survival_cohort across all time periods in a specified data frame

names(survival_cohorts) <- paste0("survival_cohort_", time_intervals)
list2env(survival_cohorts, envir = .GlobalEnv)
rm(survival_cohorts)
survival_cohort_objects <- ls(pattern = "^survival_cohort")
survival_cohort_list <- mget(survival_cohort_objects)
combined_survival_cohort <- do.call(rbind, survival_cohort_list)



##### PERIOD-SPECIFIC ADMISSIONS #####
#cancer-related admissions 
create_ps_total_admissions <- function(data, age_variable, adms_variable, period) {
  data %>%
    mutate(!!adms_variable := ifelse(is.na(!!adms_variable), 0, !!adms_variable)) %>%
    select(patientid, !!age_variable, !!adms_variable) %>%
    group_by(!!age_variable) %>%
    summarise(adms_in_period = sum(!!adms_variable)) %>%
    rename("age_group" := !!age_variable) %>%
    mutate(period = period)
} #creates data frame of adms by age group in a specified period

ps_total_admissions_cr <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  adms_variable <- sym(paste0("ps_total_adms_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_ps_total_admissions(apc_adms_patient_agg_cr, age_variable, adms_variable, period)
}) #runs create_ps_total_admissions across all time periods in a specified data frame

names(ps_total_admissions_cr) <- paste0("ps_total_adms_cr", time_intervals)
list2env(ps_total_admissions_cr, envir = .GlobalEnv)
rm(ps_total_admissions_cr)
ps_total_adms_cr_objects <- ls(pattern = "^ps_total_adms_cr")
ps_total_adms_cr_list <- mget(ps_total_adms_cr_objects)
combined_ps_total_adms_cr <- do.call(rbind, ps_total_adms_cr_list)


#all admissions
ps_total_admissions_all <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  adms_variable <- sym(paste0("ps_total_adms_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_ps_total_admissions(apc_adms_patient_agg_all, age_variable, adms_variable, period)
})

names(ps_total_admissions_all) <- paste0("ps_total_adms_all_", time_intervals)
list2env(ps_total_admissions_all, envir = .GlobalEnv)
rm(ps_total_admissions_all)
ps_total_admissions_all_objects <- ls(pattern = "^ps_total_adms_all")
ps_total_admissions_all_list <- mget(ps_total_admissions_all_objects)
combined_ps_total_adms_all <- do.call(rbind, ps_total_admissions_all_list)


##### CUMULATIVE ADMISSIONS #####
#cancer-related admissions 
create_cum_total_admissions <- function(data, age_variable, adms_variable, period) {
  data %>%
    mutate(!!adms_variable := ifelse(is.na(!!adms_variable), 0, !!adms_variable)) %>%
    select(patientid, !!age_variable, !!adms_variable) %>%
    group_by(!!age_variable) %>%
    summarise(adms_in_period = sum(!!adms_variable)) %>%
    rename("age_group" := !!age_variable) %>%
    mutate(period = period)
} #creates data frame of adms by age group in a specified period

cum_total_admissions_cr <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  adms_variable <- sym(paste0("cum_total_adms_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_cum_total_admissions(apc_adms_patient_agg_cr, age_variable, adms_variable, period)
}) #runs create_cum_total_admissions across all time periods in a specified data frame

names(cum_total_admissions_cr) <- paste0("cum_total_adms_cr", time_intervals)
list2env(cum_total_admissions_cr, envir = .GlobalEnv)
rm(cum_total_admissions_cr)
cum_total_adms_cr_objects <- ls(pattern = "^cum_total_adms_cr")
cum_total_adms_cr_list <- mget(cum_total_adms_cr_objects)
combined_cum_total_adms_cr <- do.call(rbind, cum_total_adms_cr_list)


#all admissions
cum_total_admissions_all <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  adms_variable <- sym(paste0("cum_total_adms_", interval))
  period <- interval###will want to change this to something more readable eventually 
  
  create_cum_total_admissions(apc_adms_patient_agg_all, age_variable, adms_variable, period)
})

names(cum_total_admissions_all) <- paste0("cum_total_adms_all_", time_intervals)
list2env(cum_total_admissions_all, envir = .GlobalEnv)
rm(cum_total_admissions_all)
cum_total_admissions_all_objects <- ls(pattern = "^cum_total_adms_all")
cum_total_admissions_all_list <- mget(cum_total_admissions_all_objects)
combined_cum_total_adms_all <- do.call(rbind, cum_total_admissions_all_list)



##### PERIOD-SPECIFIC ADMISSIONS PER PATIENT BY TIME PERIOD #####
#cancer-related admissions 
apc_adms_per_patient_cr <- left_join(combined_ps_total_adms_cr, combined_survival_cohort, by = c("age_group", "period"))

apc_adms_per_patient_cr <- apc_adms_per_patient_cr %>%
  phe_rate(., adms_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals))

write.csv(apc_adms_per_patient_cr, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/APC number admissions CR per patient by age 20240409.csv")  

apc_adms_per_patient_plot_cr <- ggplot(apc_adms_per_patient_cr, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  geom_point(data = gen_pop_apc, aes(x = period, y = rate, fill = age_group), 
             position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  scale_y_continuous(limits = c(0, 12), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  labs(x = "Time post-diagnosis", y = "Admissions per patient", fill = "Age group",
       caption = "Bars indicate cancer-related period-specific admissions in those diagnosed with cancer in 2014, i.e. '6 months' refers to those occurring between 3 and 6 months post-diagnosis.
       \nCircles indicate average admissions in the general population between 2013/14 and 2019/20",
       title = "Cancer-related inpatient admissions per patient in the cancer population\nand all admissions in the general population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))


#all admissions 
apc_adms_per_patient_all <- left_join(combined_ps_total_adms_all, combined_survival_cohort, by = c("age_group", "period"))

apc_adms_per_patient_all <- apc_adms_per_patient_all %>%
  phe_rate(., adms_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals))

write.csv(apc_adms_per_patient_all, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/APC number admissions all per patient by age 20240409.csv")  

apc_adms_per_patient_plot_all <- ggplot(apc_adms_per_patient_all, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  geom_point(data = gen_pop_apc, aes(x = period, y = rate, fill = age_group), 
             position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  #scale_y_continuous(limits = c(0, 10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "Time post-diagnosis", y = "Admissions per patient", fill = "Age group",
       caption = "Bars indicate all period-specific admissions in those diagnosed with cancer in 2014, i.e. '6 months' refers to those occurring between 3 and 6 months post-diagnosis.
       \nCircles indicate average admissions in the general population between 2013/14 and 2019/20",
       title = "All inpatient admissions attendances per patient in the cancer population and general population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))






##### CUMULATIVE ADMISSIONS PER PATIENT BY TIME PERIOD #####
#cancer-related admissions 
apc_adms_per_patient_cr <- left_join(combined_cum_total_adms_cr, combined_survival_cohort, by = c("age_group", "period"))

apc_adms_per_patient_cr <- apc_adms_per_patient_cr %>%
  phe_rate(., adms_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals))

write.csv(apc_adms_per_patient_cr, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/APC cum number admissions CR per patient by age 20240409.csv")  

apc_adms_per_patient_plot_cr <- ggplot(apc_adms_per_patient_cr, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  geom_point(data = gen_pop_apc, aes(x = period, y = rate, fill = age_group), 
             position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  scale_y_continuous(limits = c(0, 12), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  labs(x = "Time post-diagnosis", y = "Admissions per patient", fill = "Age group",
       caption = "Bars indicate cancer-related cumulative admissions in those diagnosed with cancer in 2014, i.e. '6 months' refers to all those occurring within 6 months post-diagnosis.
       \nCircles indicate average admissions in the general population between 2013/14 and 2019/20",
       title = "Cancer-related inpatient admissions per patient in the cancer population\nand all admissions in the general population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))


#all admissions 
apc_adms_per_patient_all <- left_join(combined_cum_total_adms_all, combined_survival_cohort, by = c("age_group", "period"))

apc_adms_per_patient_all <- apc_adms_per_patient_all %>%
  phe_rate(., adms_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals))

write.csv(apc_adms_per_patient_all, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/APC cum number admissions all per patient by age 20240409.csv")  

apc_adms_per_patient_plot_all <- ggplot(apc_adms_per_patient_all, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  geom_point(data = gen_pop_apc, aes(x = period, y = rate, fill = age_group), 
             position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  #scale_y_continuous(limits = c(0, 10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "Time post-diagnosis", y = "Admissions per patient", fill = "Age group",
       caption = "Bars indicate all cumulative admissions in those diagnosed with cancer in 2014, i.e. '6 months' refers to all those occurring within 6 months post-diagnosis.
       \nCircles indicate average admissions in the general population between 2013/14 and 2019/20",
       title = "All inpatient admissions attendances per patient in the cancer population and general population") +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

