###################### Length of stay 2023 ######################

#Script to analyse HES APC data for number of admissions 
#Uses aggregated number of admissions per patient created in "Data prep - inpatient.R"
#Uses survival cohort denominator table (number alive at each time interval by age group), created in "Data prep - cohort.R"

#June 2024 by Lizzie Augarde 
############################################################### 

##### PREP ##### 
library(PHEindicatormethods)
library(dplyr)

time_intervals <- c("12months", "2years", "3years", "4years", "5years")


##### ADMISSIONS BY TIME INTERVAL AND AGE GROUP - NUMERATOR #####
#function to create a data frame of numbers of admissions in a time period by age group
create_total_admissions <- function(data, age_variable, adms_variable, alive_variable, period) {
  data %>%
    filter(!!alive_variable == "Yes") %>%
    mutate(!!adms_variable := ifelse(is.na(!!adms_variable), 0, !!adms_variable)) %>%
    select(patientid, !!age_variable, !!adms_variable) %>%
    group_by(!!age_variable) %>%
    summarise(adms_in_period = sum(!!adms_variable)) %>%
    rename("age_group" := !!age_variable) %>%
    mutate(period = period)
} 

#function to run the create_total_admissions function across all the time periods in the time_intervals object
total_admissions <- lapply(time_intervals, function(interval) {
  age_variable <- sym(paste0("age_", interval, "_postdiag"))
  adms_variable <- sym(paste0("ps_los_", interval))
  alive_variable <- sym(paste0("alive_", interval))
  period <- interval
  
  create_total_admissions(apc_patient_agg, age_variable, adms_variable, alive_variable, period)
}) 

names(total_admissions) <- paste0("total_admissions", time_intervals)
list2env(total_admissions, envir = .GlobalEnv) #converts the total admissions from a list to individual objects
rm(total_admissions) #removes the list object
total_admissions_objects <- ls(pattern = "^total_admissions")
total_admissions_list <- mget(total_admissions_objects)
combined_total_admissions <- do.call(rbind, total_admissions_list) #combines to a single data frame ie the number of admissions in each time interval, by age group


##### RATE OF ADMISSIONS PER PATIENT BY TIME PERIOD #####
apc_patient_rate <- left_join(combined_total_admissions, combined_survival_cohort, by = c("age_group", "period")) #join the numerator and survival cohort denominator table

apc_patient_rate <- apc_patient_rate %>%
  #rate of admissions per alive patient, in each time period, by age group
  phe_rate(., adms_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) %>% #
  rename("rate" = "value") %>%
  mutate(period = factor(period, levels = time_intervals)) %>% #converting to a factor for the graph
  
  #making the time periods read better for the graph
  mutate(period = case_when(period == "12months" ~ "1 year", 
                            period == "2years" ~ "2 years",
                            period == "3years" ~ "3 years",
                            period == "4years" ~ "4 years",
                            period == "5years" ~ "5 years"
                        ))


##### GRAPH #####
apc_patient_rate_plot <- ggplot(apc_patient_rate, aes(x = period, y = rate, group = age_group)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = age_group)) + 
  scale_fill_manual(values = c("#538FFF", "#B431E6", "#FF5B58", "#F7ED65", "#28D2AB", "#FCA207", "#F6CCF9", "#1EB523", "#C3DAFF")) +
  
  #adding confidence intervals
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl, group = age_group), position = "dodge", stat = "identity", linewidth = 0.1) +
  
  #adding general population data as dots
  geom_point(data = gen_pop_apc, aes(x = period, y = rate, fill = age_group), position = position_dodge(0.9), size = 2, shape = 21, color = "black", stroke = 1.5) +
  labs(x = "Time post-diagnosis", y = "Admissions per patient", fill = "Age group") +
  scale_y_continuous(limits = c(0,60)) +
  theme(axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

#total admissions across the 5 years
total_av <- apc_patient_agg %>%
  filter(!is.na(sum_los_5years)) 

total_apc <- mean(total_av$sum_los_5years)
