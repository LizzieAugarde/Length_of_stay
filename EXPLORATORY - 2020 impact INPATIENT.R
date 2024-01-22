###################### Length of stay 2023######################

#Script to process linked inpatient data/registry data from HES (APC events 
#in first 5 years post diagnosis from HESAPC linked on TUMOURID).
#Basic aggregation of all APC data for all diagnoses, for assessing 
#rough impact of 2020 on the data

#Created December 2023 by Lizzie Augarde 
#Change log:
#20/12/2023 added code to look at COVID codes in cancer-related admission records
############################################################### 

#prep
library(tidyverse)
library(janitor)

#read from csvs
los2014_apc_events_raw_1314 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1314_RAW_20231205.csv")
los2014_apc_events_raw_1415 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1415_RAW_20231205.csv")
los2014_apc_events_raw_1516 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1516_RAW_20231205.csv")
los2014_apc_events_raw_1617 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1617_RAW_20231205.csv")
los2014_apc_events_raw_1718 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1718_RAW_20231205.csv")
los2014_apc_events_raw_1819 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1819_RAW_20231205.csv")
los2014_apc_events_raw_1920 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2014_APC_EVENTS_1920_RAW_20231205.csv")

los2015_apc_events_raw <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2015_APC_EVENTS_RAW_20231204.csv")

los2016_apc_events_raw_1516 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2016_APC_EVENTS_1516_RAW_20231205.csv")
los2016_apc_events_raw_1617 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2016_APC_EVENTS_1617_RAW_20231205.csv")
los2016_apc_events_raw_1718 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2016_APC_EVENTS_1718_RAW_20231205.csv")
los2016_apc_events_raw_1819 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2016_APC_EVENTS_1819_RAW_20231205.csv")
los2016_apc_events_raw_1920 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2016_APC_EVENTS_1920_RAW_20231205.csv")
los2016_apc_events_raw_2021 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2016_APC_EVENTS_2021_RAW_20231205.csv")
los2016_apc_events_raw_2122 <- read.csv("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/LOS2016_APC_EVENTS_2122_RAW_20231205.csv")

los2014_apc_events_raw <- rbind(los2014_apc_events_raw_1314, los2014_apc_events_raw_1415, los2014_apc_events_raw_1516, los2014_apc_events_raw_1617, 
                               los2014_apc_events_raw_1718, los2014_apc_events_raw_1819, los2014_apc_events_raw_1920) %>%
  clean_names() %>% mutate(diag_year = 2014)

los2015_apc_events_raw <- los2015_apc_events_raw %>%
  clean_names() %>% mutate(diag_year = 2015) 

los2016_apc_events_raw <- rbind(los2016_apc_events_raw_1516, los2016_apc_events_raw_1617, los2016_apc_events_raw_1718, los2016_apc_events_raw_1819, 
                               los2016_apc_events_raw_1920, los2016_apc_events_raw_2021, los2016_apc_events_raw_2122) %>%
  clean_names() %>% mutate(diag_year = 2016)

los_apc_events <- rbind(los2014_apc_events_raw, los2015_apc_events_raw, los2016_apc_events_raw)

#defining episodes as cancer-related if a C code is in one of the 3 diag columns
los_apc_events <- los_apc_events %>%
  unique() %>%
  mutate(diag1_cancer = case_when(str_detect(diag_1, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag2_cancer = case_when(str_detect(diag_2, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag3_cancer = case_when(str_detect(diag_3, "C") ~ "Y", TRUE ~ "N")) %>%
  mutate(episode_cancer_related = case_when(diag1_cancer == "Y" | diag2_cancer == "Y" | diag3_cancer == "Y" ~ "Y", TRUE ~ "N")) %>%
  filter(episode_cancer_related == "Y")

diagcols <- c("diag1_cancer", "diag2_cancer", "diag3_cancer")

los_apc_events <- los_apc_events %>% #identifies which position(s) have cancer codes 
  mutate(cancer_code_pos = apply(select(., diagcols) == "Y", 1, function(x) toString(which(x)))) %>%
  mutate(cancer_code_pos = ifelse(cancer_code_pos == "", "NA", cancer_code_pos))

#date adjustments
los_apc_events <- los_apc_events %>%
  mutate(follow_up_start = as.Date(follow_up_start),
         admidate = as.Date(admidate),
         disdate = as.Date(disdate),
         follow_up_end = as.Date(follow_up_end)) %>%
  filter(disdate >= admidate) %>%
  filter(admidate >= follow_up_start) %>%
  filter(admidate <= follow_up_end) %>%
  mutate(adj_disdate = as.Date(if_else(disdate > follow_up_end, follow_up_end, disdate))) %>% #caps discharge dates to date of death or FU end if they exceed 
  mutate(adm_days_post_diag = admidate-follow_up_start) %>% #days between diagnosis and admission
  mutate(adm_years_post_diag = case_when(adm_days_post_diag < 366 ~ paste0("Less than 1 year\n(", diag_year, "-", diag_year+1, ")"),
                                         adm_days_post_diag > 365 & adm_days_post_diag < 731 ~ paste0("1-2 years\n(", diag_year+1, "-", diag_year+2, ")"),
                                         adm_days_post_diag > 730 & adm_days_post_diag < 1096 ~ paste0("2-3 years\n(", diag_year+2, "-", diag_year+3, ")"),
                                         adm_days_post_diag > 1095 & adm_days_post_diag < 1461 ~ paste0("3-4 years\n(", diag_year+3, "-", diag_year+4, ")"),
                                         adm_days_post_diag > 1460 ~ paste0("4-5 years\n(", diag_year+4, "-", diag_year+5, ")")))

#calculating LOS for each stay
los_apc_events <- los_apc_events %>%
  mutate(stay_length = adj_disdate-admidate) %>%
  mutate(stay_length = ifelse(stay_length <= 0, 1, stay_length)) #stays with same admission and discharge date counted as 1 day

#group by years post-diagnosis
los_apc_los_by_month <- los_apc_events %>%
  group_by(diag_year, adm_years_post_diag) %>%
  summarise(total_los = sum(stay_length)) %>%
  mutate(num_for_factor = as.numeric(gsub("\\D", "", adm_years_post_diag))) %>%
  mutate(adm_years_post_diag = fct_reorder(adm_years_post_diag, num_for_factor)) %>%
  ungroup()

ggplot(los_apc_los_by_month, aes(x = adm_years_post_diag, y = total_los)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~diag_year, nrow = 1, scales = "free_x")  


########## COVID ###########
#how many cancer-related APC episodes have COVID in diag codes 1-3?
los_apc_events_covid <- los_apc_events %>%
  mutate(diag1_covid = case_when(str_detect(diag_1, "B342|U071|U072|B972") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag2_covid = case_when(str_detect(diag_2, "B342|U071|U072|B972") ~ "Y", TRUE ~ "N")) %>%
  mutate(diag3_covid = case_when(str_detect(diag_3, "B342|U071|U072|B972") ~ "Y", TRUE ~ "N")) %>%
  mutate(episode_covid_related = case_when(diag1_covid == "Y" | diag2_covid == "Y" | diag3_covid == "Y" ~ "Y", TRUE ~ "N"))
  
