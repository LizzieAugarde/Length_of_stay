###################### Length of stay 2023 ######################

#Data prep - A&E
#Script to process HES A&E data for 2014 diagnoses into LOS data frame

#June 2024 by Lizzie Augarde 
############################################################### 

##### PREP AND READ IN RAW DATA ##### 
library(NDRSAfunctions)
library(tidyverse)
library(janitor)

casref01 <- createConnection()

#extract of all A&E attendances for the cohort, takes approx 5 minutes
ae_events_query <- "select a.tumourid, 
                    a.patientid, 
                    a.follow_up_start, 
                    a.follow_up_end, 
                    a.deathdatebest, 
                    c.aekeyanon,
                    c.datayear,
                    c.arrivaldate_raw
      from analysiselizabethaugarde.los2014_cohort a left join 
          heslive.hes_linkage_av_ae@casref01 b on a.patientid = b.patientid left join
          heslive.hesae@casref01 c on b.aekeyanon = c.aekeyanon and b.datayear = c.datayear
      where c.datayear in ('1314', '1415', '1516', '1617', '1718', '1819', '1920', '2021')
      and a.patientid is not null"

ae_events <- dbGetQueryOracle(casref01, ae_events_query, rowlimit = NA)

#convert arrival date variable to date 
ae_events <- ae_events |> 
  mutate(ARRIVALDATE = as.POSIXct(ARRIVALDATE_RAW, format = "%d%m%Y")) |>

#filtering based on episode dates, takes 1 minute
#FOLLOW_UP_START is diagnosis date, created in cohort SQL query
#FOLLOW_UP_END is 5 years after diagnosis date, created in cohort SQL query
  filter((interval(FOLLOW_UP_START, ARRIVALDATE) / days(1)) >= 0) |> #keeping only episodes starting on or after diagnosis date
  filter((interval(ARRIVALDATE, FOLLOW_UP_END) / days(1)) >= 0) #keeping only episodes starting on or before follow up end date


##### CALCULATING HOW LONG AFTER DIAGNOSIS EACH ATTENDANCE OCCURS ##### 
ae_events <- ae_events |> 
  clean_names() |>
  unique() |> #removing duplicate events, takes 1 minute
  mutate(att_days_post_diag = difftime(as.Date(arrivaldate), as.Date(follow_up_start), units = "days")) |>
  mutate(att_12months = ifelse(att_days_post_diag < 366, 1, 0),
         att_2years = ifelse(att_days_post_diag < 731, 1, 0),
         att_3years = ifelse(att_days_post_diag < 1096, 1, 0),
         att_4years = ifelse(att_days_post_diag < 1461, 1, 0),
         att_5years = ifelse(att_days_post_diag < 1826, 1, 0)) 


##### TOTAL ATTENDANCES PER PATIENT PER TIME PERIOD ##### 
ae_patient_agg <- ae_events |>
  mutate(attend_count = 1) |> #counting each attendance as 1 day to enable LOS calculations
  group_by(patientid) |>
  
  #cumulative total LOS - adding up all attendances for each patient in each time period 
  summarize(sum_att_12months = sum(att_12months), 
            sum_att_2years = sum(att_2years), 
            sum_att_3years = sum(att_3years),
            sum_att_4years = sum(att_4years),
            sum_att_5years = sum(att_5years),
            
            #period-specific total LOS (i.e. LOS 2 years is the attendances which occur between 12 months and 2 years only)
            ps_att_12months = sum_att_12months, 
            
            #subtract the sum of appointments in 12 months from the sum in 2 years to get the total number between 12 months and 2 years
            ps_att_2years = sum_att_2years-sum_att_12months, 
            ps_att_3years = sum_att_3years-sum_att_2years, 
            ps_att_4years = sum_att_4years-sum_att_3years, 
            ps_att_5years = sum_att_5years-sum_att_4years) |>
  
  #adding survival and demographic variables for each patient
  left_join(cohort_clean, by = "patientid") 
