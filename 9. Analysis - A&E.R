###################### Length of stay 2023 ######################

#Script to analyse HES A&E data for number of attendances 
#Uses aggregated number of admissions per patient created in "Data prep - A&E.R"
#Uses survival cohort denominator table (number alive at each time interval by age group), created in "Data prep - cohort.R"

#June 2024 by Lizzie Augarde 
############################################################### 

##### PREP ##### 
library(PHEindicatormethods)
library(dplyr)
library(xlsx)


##### ATTENDANCES BY TIME INTERVAL AND AGE GROUP FOR EACH VALUE OF ANOTHER CHARACTERISTIC- NUMERATOR #####
time_intervals <- c("12months", "2years", "3years", "4years", "5years")

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

#function to run the create_total_atts function across all the time periods in the time_intervals object
#after first filtering to each value of a specified characteristic variable 
#so we end up with a data frame of the total number of attendances by age group within each time period, 
#for each value of the characteristic
generate_total_atts <- function(data, time_intervals, char_variable) {
  
  #get unique values of the characteristic of interest
  unique_values <- unique(data[[char_variable]])
  
  #empty list to store results 
  all_atts_dfs <- list()
  
  for (char_value in unique_values) {
    filtered_data <- data |> 
      filter(!!rlang::sym(char_variable) == char_value)
    
    #generate the attendances table for each time interval
    total_attends <- lapply(time_intervals, function(interval) {
      age_variable <- sym(paste0("age_", interval, "_postdiag"))
      atts_variable <- sym(paste0("ps_att_", interval))
      alive_variable <- sym(paste0("alive_", interval))
      period <- interval
      
      create_total_atts(filtered_data, age_variable, atts_variable, alive_variable, period)
    })
    
    #combine the attendances data frames for this value of the characteristic
    combined_total_atts <- bind_rows(total_attends, .id = "time_interval")
    combined_total_atts <- combined_total_atts |>
      mutate(characteristic = char_value)
    
    #store the combined cohort in the list
    all_atts_dfs[[char_value]] <- combined_total_atts
  }
  
  combined_total_atts <- bind_rows(all_atts_dfs, .id = "char_value")
  
  #name the output with the characteristic variable name 
  output_name <- paste0("combined_total_atts_", char_variable)
  assign(output_name, combined_total_atts, envir = .GlobalEnv)
  
  return(combined_total_atts)
  
}

#specify a characteristic to generate age-specific attendances data frames for
char_variable <- "ethnicity"

#run attendances by age for a specified characteristic variable
combined_total_atts <- generate_total_atts(ae_patient_agg, time_intervals, char_variable)


##### RATE OF ATTENDANCES PER PATIENT BY TIME PERIOD AND CHARACTERISTIC VALUE #####
ae_patient_rate <- left_join(get(paste0("combined_total_atts_", char_variable)), 
                             get(paste0("combined_survival_cohort_", char_variable)), 
                             by = c("age_group", "period", "characteristic")) #join the numerator and survival cohort denominator table

ae_patient_rate <- ae_patient_rate |>
  #rate of attendances per alive patient, in each time period, by age group
  phe_rate(atts_in_period, number_alive_at_period_end, type = "standard", confidence = 0.95, multiplier = 1) |>
  rename("rate" = "value") |>
  mutate(period = factor(period, levels = time_intervals)) |>
  
  #making the time periods read better for the graph
  mutate(period = case_when(period == "12months" ~ "1 year",
                            period == "2years" ~ "2 years",
                            period == "3years" ~ "3 years",
                            period == "4years" ~ "4 years",
                            period == "5years" ~ "5 years")) |>
                            
  #tidying the data frame for write out 
  select(-c(char_value.x, time_interval.x, time_interval.y)) |>
  select(c(period, age_group, characteristic, atts_in_period, number_alive_at_period_end, rate, lowercl, uppercl))


##### WRITE OUT #####
char <- "Ethnicity"
write.xlsx(ae_patient_rate, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Results/October 2024/A&E results.xlsx", 
                                   sheetName = char, append = TRUE)


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
