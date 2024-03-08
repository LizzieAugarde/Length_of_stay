

colnames(apc_los_per_patient)[-1] <- paste0(colnames(apc_los_per_patient)[-1], "_apc")
colnames(op_los_per_patient)[-1] <- paste0(colnames(op_los_per_patient)[-1], "_op")
colnames(ae_los_per_patient)[-1] <- paste0(colnames(ae_los_per_patient)[-1], "_ae")

all_data <- left_join(apc_los_per_patient, op_los_per_patient, by = "time_period") %>%
            left_join(., ae_los_per_patient, by = "time_period")


apc_los_per_patient <- apc_los_per_patient %>% mutate(type = "APC")
op_los_per_patient <- op_los_per_patient %>% mutate(type = "OP")
ae_los_per_patient <- ae_los_per_patient %>% mutate(type = "A&E")

all_data <- rbind(apc_los_per_patient, op_los_per_patient, ae_los_per_patient)

all_data_plot <- ggplot(all_data, aes(x = time_period, y = rate, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge")
