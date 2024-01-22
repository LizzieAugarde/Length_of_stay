###################### Length of stay 2023######################

#Exploratory descriptive statistics generation 

#Created November 2023 by Lizzie Augarde 
#Change log:
#16/11/2023 added basic descriptive stats for the diagnosis cohorts
############################################################### 

#packages
library(openxlsx)

#number of people diagnosed in each year
los2014_cohort_count <- dbGetQueryOracle(casref01, "SELECT COUNT (DISTINCT PATIENTID) FROM LOS2014_COHORT")
los2015_cohort_count <- dbGetQueryOracle(casref01, "SELECT COUNT (DISTINCT PATIENTID) FROM LOS2015_COHORT")
los2016_cohort_count <- dbGetQueryOracle(casref01, "SELECT COUNT (DISTINCT PATIENTID) FROM LOS2016_COHORT")

los2014_cohort_count <- as.numeric(los2014_cohort_count[1,]) #223,033
los2015_cohort_count <- as.numeric(los2015_cohort_count[1,]) #237,721
los2016_cohort_count <- as.numeric(los2016_cohort_count[1,]) #243,394


#number of people diagnosed in each year who have any contact
#inpatient
n_distinct(los2014_apc_events$PATIENTID) #214,924

#A&E
los2014_ae_patients_count <- as.numeric(n_distinct(los2014_ae_events$patientid)) #126,514
los2015_ae_patients_count <- as.numeric(n_distinct(los2015_ae_events$patientid)) #135,893
los2016_ae_patients_count <- as.numeric(n_distinct(los2016_ae_events$patientid)) #133,595


#number alive at each year post-diagnosis - NEED TO REDO WITH THE COHORT TABLES
los2014_ae_deaths <- los2014_ae_events %>%
  select(c(patientid, alive_years_post_diag)) %>%
  unique() 

los2015_ae_deaths <- los2015_ae_events %>%
  select(c(patientid, alive_years_post_diag)) %>%
  unique() %>%
  group_by(alive_years_post_diag) %>%
  summarise(count = n()) 

los2016_ae_deaths <- los2016_ae_events %>%
  select(c(patientid, alive_years_post_diag)) %>%
  unique() %>%
  group_by(alive_years_post_diag) %>%
  summarise(count = n()) 

dflist <- list("2014" = los2014_ae_deaths, "2015" = los2015_ae_deaths, "2016" = los2016_ae_deaths)
write.xlsx(dflist, file = "Patients alive each year post diagnosis 20231114.xlsx")