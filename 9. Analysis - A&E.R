###################### Length of stay 2023 ######################

#Script to analyse HES A&E data for number of attendances 
#Uses aggregated number of admissions per patient created in "Data prep - A&E.R"
#Uses survival cohort denominator table (number alive at each time interval by age group), created in "Data prep - cohort.R"

#June 2024 by Lizzie Augarde 
############################################################### 

##### PREP ##### 
library(PHEindicatormethods)
library(dplyr)

time_intervals <- c("12months", "2years", "3years", "4years", "5years")


##### ATTENDANCES BY TIME INTERVAL AND AGE GROUP - NUMERATOR #####
#function to create a data frame of numbers of attendances in a time period by age group
create_total_atts <- function(data, age_variable, atts_variable, alive_variable, period) {
  data %>%
    filter(!!alive_variable == "Yes") %>%
    mutate(!!atts_variable := ifelse(is.na(!!atts_variable), 0, !!atts_variable)) %>%
    select(patientid, !!age_variable, !!atts_variable) %>%
    group_by(!!age_variable) %>%
    summarise(atts_in_period = sum(!!atts_variable)) %>%
    rename("age_group" := !!age_variable) %>%
    mutate(period = period)
} 

#function to run the create_total_admissions function across all the time periods in the time_intervals object
total_attends <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  atts_variable <- sym(paste0("ps_att_", interval))
  alive_variable <- sym(paste0("alive_", interval))
  period <- interval
  
  create_total_atts(ae_patient_agg, age_variable, atts_variable, alive_variable, period)
}) 

names(total_attends) <- paste0("total_attends", time_intervals)
list2env(total_attends, envir = .GlobalEnv)
rm(total_attends)
total_attends_objects <- ls(pattern = "^total_attends")
total_attends_list <- mget(total_attends_objects)
combined_total_attends <- do.call(rbind, total_attends_list)


##### RATE OF ATTENDANCES PER PATIENT BY TIME PERIOD #####
ae_patient_rate <- left_join(combined_total_attends, combined_survival_cohort, by = c("age_group", "period")) #join the numerator and survival cohort denominator table

ae_patient_rate <- ae_patient_rate %>%
  #rate of attendances per alive patient, in each time period, by age group
  phe_rate(., atts_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>%
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
ae_patient_rate_plot <- ggplot(ae_patient_rate, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  scale_fill_manual(values = c("#538FFF", "#B431E6", "#FF5B58", "#F7ED65", "#28D2AB", "#FCA207", "#F6CCF9", "#1EB523", "#C3DAFF")) +

  #adding confidence intervals
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  
  #adding general population data as dots
  geom_point(data = gen_pop_ae, aes(x = period, y = rate, fill = age_group), position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  scale_y_continuous(limits = c(0, 2)) +
  labs(x = "Time post-diagnosis", y = "Attendances per patient", fill = "Age group") +
  theme(axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20), 
        plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

#total attendances across the 5 years
total_ae <- mean(ae_patient_agg$sum_att_5years)
