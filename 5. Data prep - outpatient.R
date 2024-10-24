###################### Length of stay 2023 ######################

#Data prep - outpatient
#Script to process HES outpatient data for 2014 diagnoses into LOS data frame

#June 2024 by Lizzie Augarde 
############################################################### 

##### PREP AND RAW DATA ##### 
library(NDRSAfunctions)
library(tidyverse)
library(janitor)

casref01 <- createConnection()

#extract of all outpatient appointments for the cohort, where the treatment specialty (tretspef) is in the Macmillan-agreed list of 'cancer-related' specialties
#takes approx 50 minutes
op_events_query <- "select 
    a.tumourid, 
    a.patientid, 
    a.follow_up_start, 
    a.follow_up_end, 
    a.deathdatebest, 
    c.attendkeyanon,
    c.datayear,
    c.apptdate,
    c.attended,
    c.stafftyp,
    c.tretspef
from analysiselizabethaugarde.los2014_cohort a
left join heslive.hes_linkage_av_op@casref01 b on a.patientid = b.patientid
left join heslive.hesop@casref01 c on b.attendkeyanon = c.attendkeyanon and b.datayear = c.datayear
where c.datayear in ('1314', '1415', '1516', '1617', '1718', '1819', '1920')
  and c.tretspef in ('370', '800', '318', '101', '100', '130', '103', '812', '303', '330', '160', '340',   
                     '320', '110', '120', '104', '315', '650', '140', '502', '301', '410', '300', '503', '324')
  and a.patientid is not null"

op_events <- dbGetQueryOracle(casref01, op_events_query, rowlimit = NA)

#small number of ATTENDKEYANONs appear twice, same appointments associated with 2 patients, excluding all these ATTENDKEYANONs
#takes approx 5 minutes
op_events <- op_events |>
  group_by(ATTENDKEYANON) |>
  filter(n() == 1) |>
  ungroup()

#filtering based on appointment dates
#FOLLOW_UP_START is diagnosis date, created in cohort SQL query
#FOLLOW_UP_END is 5 years after diagnosis date, created in cohort SQL query
op_events <- op_events |> 
  filter((interval(FOLLOW_UP_START, APPTDATE) / days(1)) >= 0) |> #keeping only appointments occurring on or after diagnosis date
  filter((interval(APPTDATE, FOLLOW_UP_END) / days(1)) >= 0) #keeping only appointments occurring on or before follow up end date


##### CALCULATING HOW LONG AFTER DIAGNOSIS EACH APPOINTMENT OCCURS ##### 
op_events <- op_events |>
  clean_names() |>
  mutate(appt_days_post_diag = difftime(as.Date(apptdate), as.Date(follow_up_start), units = "days")) |>
  mutate(appt_12months = ifelse(appt_days_post_diag < 366, 1, 0),
         appt_2years = ifelse(appt_days_post_diag < 731, 1, 0),
         appt_3years = ifelse(appt_days_post_diag < 1096, 1, 0),
         appt_4years = ifelse(appt_days_post_diag < 1461, 1, 0),
         appt_5years = ifelse(appt_days_post_diag < 1826, 1, 0))
  

##### TOTAL APPOINTMENTS PER PATIENT PER TIME PERIOD ##### 
op_patient_agg <- op_events |>
  group_by(patientid) |>  
  
  #cumulative total LOS - adding up all appointments for each patient in each time period 
  summarize(sum_appt_12months = sum(appt_12months), 
            sum_appt_2years = sum(appt_2years), 
            sum_appt_3years = sum(appt_3years), 
            sum_appt_4years = sum(appt_4years), 
            sum_appt_5years = sum(appt_5years),

            #period-specific total LOS (i.e. LOS 2 years is the appointments which occur between 12 months and 2 years only)
            ps_appt_12months = sum_appt_12months, 
            
            #subtract the sum of appointments in 12 months from the sum in 2 years to get the total number between 12 months and 2 years
            ps_appt_2years = sum_appt_2years-sum_appt_12months, 
            ps_appt_3years = sum_appt_3years-sum_appt_2years, 
            ps_appt_4years = sum_appt_4years-sum_appt_3years,
            ps_appt_5years = sum_appt_5years-sum_appt_4years) |>
  
  #adding survival and demographic variables for each patient
  left_join(cohort_clean, by = "patientid") 
