###################### Length of stay 2023 ######################

#Script to analyse HES outpatient data for number of appointments
#Uses aggregated number of admissions per patient created in "Data prep - outpatient.R"
#Uses survival cohort denominator table (number alive at each time interval by age group), created in "Data prep - cohort.R"

#June 2024 by Lizzie Augarde 
############################################################### 

##### PREP ##### 
library(PHEindicatormethods)
library(dplyr)

time_intervals <- c("12months", "2years", "3years", "4years", "5years")


##### APPOINTMENTS BY TIME INTERVAL AND AGE GROUP - NUMERATOR #####
#function to create a data frame of numbers of appointments in a time period by age group
create_total_appts <- function(data, age_variable, appts_variable, alive_variable, period) {
  data %>%
    filter(!!alive_variable == "Yes") %>%
    mutate(!!appts_variable := ifelse(is.na(!!appts_variable), 0, !!appts_variable)) %>%
    select(patientid, !!age_variable, !!appts_variable) %>%
    group_by(!!age_variable) %>%
    summarise(appts_in_period = sum(!!appts_variable)) %>%
    rename("age_group" := !!age_variable) %>%
    mutate(period = period)
} 

#function to run the create_total_admissions function across all the time periods in the time_intervals object
total_appts <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  appts_variable <- sym(paste0("ps_appt_", interval))
  alive_variable <- sym(paste0("alive_", interval))
  period <- interval
  
  create_total_appts(op_patient_agg, age_variable, appts_variable, alive_variable, period)
}) 

names(total_appts) <- paste0("total_appts", time_intervals) #converts the total appointments from a list to individual objects
list2env(total_appts, envir = .GlobalEnv) #removes the list object
rm(total_appts)
total_appts_objects <- ls(pattern = "^total_appts")
total_appts_list <- mget(total_appts_objects)
combined_total_appts <- do.call(rbind, total_appts_list) #combines to a single data frame ie the number of appointments in each time interval, by age group


##### RATE OF ADMISSIONS PER PATIENT BY TIME PERIOD #####
op_patient_rate <- left_join(combined_total_appts, combined_survival_cohort, by = c("age_group", "period")) #join the numerator and survival cohort denominator table

op_patient_rate <- op_patient_rate %>%
  #rate of appointments per alive patient, in each time period, by age group
  phe_rate(., appts_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals)) %>%
  
  #making the time periods read better for the graph
  mutate(period = case_when(period == "12months" ~ "1 year",
                            period == "2years" ~ "2 years",
                            period == "3years" ~ "3 years",
                            period == "4years" ~ "4 years",
                            period == "5years" ~ "5 years"
  ))


##### GRAPH #####
op_patient_rate_plot <- ggplot(op_patient_rate, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  scale_fill_manual(values = c("#538FFF", "#B431E6", "#FF5B58", "#F7ED65", "#28D2AB", "#FCA207", "#F6CCF9", "#1EB523", "#C3DAFF")) +

  #adding confidence intervals
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  
  #adding general population data as dots
  geom_point(data = gen_pop_op, aes(x = period, y = rate, fill = age_group), position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 10, 20, 30)) +
  labs(x = "Time post-diagnosis", y = "Appointments per patient", fill = "Age group") +
  theme(axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20), 
        plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

#total appointments across the 5 year period
total_op <- mean(op_patient_agg$sum_appt_5years)