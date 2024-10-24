###################### Length of stay 2023 ######################

#Data prep - inpatient
#Script to process HES APC data for 2014 diagnoses into LOS data frame

#June 2024 by Lizzie Augarde 
############################################################### 

##### PREP AND READ IN RAW DATA ##### 
library(NDRSAfunctions)
library(tidyverse)
library(janitor)

casref01 <- createConnection()

#extract query for 2 years - need to change and redo for 2years at a time then rbind, takes ages 
apc_events_query <- "select a.tumourid, 
       a.patientid, 
       a.follow_up_start, 
       a.follow_up_end, 
       a.deathdatebest, 
       c.epikeyanon,
       c.datayear,
       c.admidate, 
       c.disdate, 
       c.epiend_raw,
       c.admimeth,
       max(case when d.pos = 1 then d.diag_4 end) as diag_1,
       max(case when d.pos = 2 then d.diag_4 end) as diag_2,
       max(case when d.pos = 3 then d.diag_4 end) as diag_3
from analysiselizabethaugarde.los2014_cohort a left join 
        heslive.hes_linkage_av_apc@casref01 b on a.patientid = b.patientid left join --link to HES linkage fields
        heslive.hesapc@casref01 c on b.epikeyanon = c.epikeyanon and b.datayear = c.datayear left join --link to HES inpatient records
        heslive.hesapc_diag@casref01 d on c.epikeyanon = d.epikeyanon and c.datayear = d.datayear --link to HES inpatient diagnostic codes
where c.datayear in ('1718', '1819') --, '1314', '1415', '1516', '1617', '1718', '1819', '1920'
group by a.tumourid,
    a.patientid,
    a.follow_up_start,
    a.follow_up_end,
    a.deathdatebest,
    c.admidate,
    c.disdate,
    c.epiend_raw,
    c.admimeth"

apc_events_1718_1819 <- dbGetQueryOracle(casref01, apc_events_query, rowlimit = NA)

save(apc_events_1314_1415, file = "apc_events_1314_1415.RData")
save(apc_events_1516_1617, file = "apc_events_1516_1617.RData")
save(apc_events_1718_1819, file = "apc_events_1718_1819.RData")
save(apc_events_1920, file = "apc_events_1920.RData")

#read from RData files - originally extracted from CAS as abovebut takes ages to run, Claire happy with SQL query so read from these files instead please
load("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/apc_events_1314_1415.RData")
load("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/apc_events_1516_1617.RData")
load("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/apc_events_1718_1819.RData")
load("N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/apc_events_1920.RData")

apc_events <- rbind(apc_events_1314_1415, apc_events_1516_1617, apc_events_1718_1819, apc_events_1920)

#adjusting discharge dates for records with weird dates/missing discharge date, EPIEND_RAW is the raw episode end date which should usually match DISDATE
apc_events <- apc_events |> 
  mutate(EPIEND_RAW = dmy(EPIEND_RAW)) |>
  mutate(DISDATE = as.character(DISDATE)) |>
  mutate(DISDATE = ifelse(DISDATE %in% c("1801-01-01", "1800-01-01"), as.character(EPIEND_RAW), 
                          ifelse(is.na(DISDATE), as.character(EPIEND_RAW), DISDATE))) |>
  mutate(DISDATE = as.Date(DISDATE)) |>

#small number of EPIKEYANONS appear twice, same admissions associated with 2 patients, excluding all these EPIKEYANONS
  group_by(EPIKEYANON) |>
  filter(n() == 1) |>
  ungroup() |>

#filtering based on episode dates
#FOLLOW_UP_START is diagnosis date, created in cohort SQL query
#FOLLOW_UP_END is 5 years after diagnosis date, created in cohort SQL query
  filter((interval(FOLLOW_UP_START, ADMIDATE) / days(1)) >= 0) |> #keeping only episodes starting on or after diagnosis date, ADMIDATE is admission date
  filter((interval(ADMIDATE, FOLLOW_UP_END) / days(1)) >= 0) #keeping only episodes starting on or before follow up end date


##### IDENTIFYING CANCER-RELATED ADMISSIONS ##### 
#defining episodes as cancer-related based on C code in first 3 diagnosis code positions
apc_events <- apc_events |>
  clean_names() |>
  unique() |> #removing duplicate events
  mutate(diag1_cancer = case_when(str_detect(diag_1, "C") ~ "Y", TRUE ~ "N")) |>
  mutate(diag2_cancer = case_when(str_detect(diag_2, "C") ~ "Y", TRUE ~ "N")) |>
  mutate(diag3_cancer = case_when(str_detect(diag_3, "C") ~ "Y", TRUE ~ "N")) |>
  mutate(episode_cancer_related = case_when(diag1_cancer == "Y" | diag2_cancer == "Y" | diag3_cancer == "Y" ~ "Y", TRUE ~ "N")) |>
  filter(episode_cancer_related == "Y") #keeping only cancer-related episodes


##### CALCULATING LENGTH OF STAY ##### 
apc_events <- apc_events |>
  #capping discharge dates to date of death or follow up end date if they exceed 5 years post diagnosis 
  mutate(disdate = if_else(disdate > follow_up_end, follow_up_end, disdate)) |> 
  
  #calculating length of each episode in days - adding one to count the start date as 1 day and prevent any 0 length episodes
  mutate(epi_length = difftime(as.Date(disdate), as.Date(admidate), units = "days") + 1) |>
  mutate(epi_length = as.numeric(epi_length)) |>
  
  #set up dates to mark the time periods for each patient - 12 months post diag, 2 years etc 
  mutate(fus_plus12months = as.Date(follow_up_start) + 365,
         fus_plus2years   = as.Date(follow_up_start) + 730,
         fus_plus3years   = as.Date(follow_up_start) + 1095,
         fus_plus4years   = as.Date(follow_up_start) + 1460,
         fus_plus5years   = as.Date(follow_up_start) + 1825) |>
  
  #set up time difference variables - time between admission and each follow up date 
  mutate(diff_12months = difftime(as.Date(fus_plus12months), as.Date(admidate), units = "days"),
         diff_2years   = difftime(as.Date(fus_plus2years), as.Date(admidate), units = "days"),
         diff_3years   = difftime(as.Date(fus_plus3years), as.Date(admidate), units = "days"),
         diff_4years   = difftime(as.Date(fus_plus4years), as.Date(admidate), units = "days"),
         diff_5years   = difftime(as.Date(fus_plus5years), as.Date(admidate), units = "days")) |>
  
  #calculating length of each episode within each time period 
  mutate(los_12months = ifelse(diff_12months < 0, 0, ifelse(epi_length > diff_12months, diff_12months, epi_length)),
         los_2years   = ifelse(diff_2years < 0, 0, ifelse(epi_length > diff_2years, diff_2years, epi_length)),
         los_3years   = ifelse(diff_3years < 0, 0, ifelse(epi_length > diff_3years, diff_3years, epi_length)),
         los_4years   = ifelse(diff_4years < 0, 0, ifelse(epi_length > diff_4years, diff_4years, epi_length)),
         los_5years   = ifelse(diff_5years < 0, 0, ifelse(epi_length > diff_5years, diff_5years, epi_length)))


##### TOTAL LENGTH OF STAY PER PATIENT PER TIME PERIOD ##### 
apc_patient_agg <- apc_events |>#
  mutate(patientid = as.character(patientid)) |>
  group_by(patientid) |>
  
  #cumulative total LOS - adding up all admissions for each patient in each time period 
  summarize(sum_los_12months = sum(los_12months), 
            sum_los_2years = sum(los_2years), 
            sum_los_3years = sum(los_3years), 
            sum_los_4years = sum(los_4years), 
            sum_los_5years = sum(los_5years),
  
  #period-specific total LOS (i.e. LOS 2 years is the admissions which occur between 12 months and 2 years only)
            ps_los_12months = sum_los_12months, 
            
            #subtract the sum of admissions in 12 months from the sum in 2 years to get the total number between 12 months and 2 years
            ps_los_2years = sum_los_2years-sum_los_12months, 
            ps_los_3years = sum_los_3years-sum_los_2years, 
            ps_los_4years = sum_los_4years-sum_los_3years, 
            ps_los_5years = sum_los_5years-sum_los_4years) |>
  
  #adding survival and demographic variables for each patient
  left_join(cohort_clean, by = "patientid") 


  


  