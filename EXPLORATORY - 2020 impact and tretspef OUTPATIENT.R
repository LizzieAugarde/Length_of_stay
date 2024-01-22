###################### Length of stay 2023######################

#Script to extract a sample of OP data to look at 2020 impact and
#distribution of appointments by treatment specification, in first 
#5 years post diagnosis from HESOP linked on TUMOURID).

#Created December 2023 by Lizzie Augarde 
#Change log:
#20/12/2023 added code to review treatment specialties among subsets 
#of patients by tumour type. Limited to 2014 diagnoses based on 
#2020 impact investigation work
############################################################### 

#prep
library(tidyverse)
library(janitor)
library(NDRSAfunctions)

casref01 <- createConnection()

query <- "with randomsample as (
    select *
    from analysiselizabethaugarde.los2014_cohort_tg
    where rownum <=5000
    order by dbms_random.value
)
select 
    rs.tumourid, 
    rs.patientid, 
    rs.site_icd10_o2_3char,
    rs.follow_up_start, 
    rs.follow_up_end, 
    rs.deathdatebest, 
    c.attendkeyanon,
    c.datayear,
    c.apptdate,
    c.attended,
    c.stafftyp,
    c.tretspef
from randomsample rs
left join heslive.hes_linkage_av_op@casref01 b on rs.patientid = b.patientid
left join heslive.hesop@casref01 c on b.attendkeyanon = c.attendkeyanon and b.datayear = c.datayear
where c.datayear in ('1314', '1415', '1516', '1617', '1718', '1819', '1920')
  and rs.patientid is not null"

los2014_sample_op_events <- dbGetQueryOracle(casref01, query, rowlimit = NA)

#removed 20/12/2023 - using 2014 diagnoses 
#los2014_sample_op_events$diag_year <- 2014
#los2015_sample_op_events$diag_year <- 2015
#los2016_sample_op_events$diag_year <- 2016

#los_sample_op_events <- rbind(los2014_sample_op_events, los2015_sample_op_events, los2016_sample_op_events)

#date adjustments
los_sample_op_events <- los2014_sample_op_events %>%
  clean_names() %>%
  mutate(diag_year = 2014) %>%
  mutate(follow_up_start = as.Date(follow_up_start),
         follow_up_end = as.Date(follow_up_end),
         apptdate = as.Date(apptdate)) %>%
  filter(follow_up_start <= apptdate) %>%
  filter(apptdate <= follow_up_end) %>%
  mutate(appt_days_post_diag = apptdate-follow_up_start) %>% #days between diagnosis and appointment
  mutate(appt_years_post_diag = case_when(appt_days_post_diag < 366 ~ paste0("Less than 1 year\n(", diag_year, "-", diag_year+1, ")"),
                                          appt_days_post_diag > 365 & appt_days_post_diag < 731 ~ paste0("1-2 years\n(", diag_year+1, "-", diag_year+2, ")"),
                                          appt_days_post_diag > 730 & appt_days_post_diag < 1096 ~ paste0("2-3 years\n(", diag_year+2, "-", diag_year+3, ")"),
                                          appt_days_post_diag > 1095 & appt_days_post_diag < 1461 ~ paste0("3-4 years\n(", diag_year+3, "-", diag_year+4, ")"),
                                          appt_days_post_diag > 1460 ~ paste0("4-5 years\n(", diag_year+4, "-", diag_year+5, ")"))) 

#grouping
los_sample_op_events_agg <- los_sample_op_events %>%
  group_by(diag_year, appt_years_post_diag, stafftyp, tretspef) %>%
  summarise(total_appts = n())

#appointments by years post-diagnosis
appts_by_year <- los_sample_op_events_agg %>%
  group_by(diag_year, appt_years_post_diag) %>%
  summarise(total_appts = sum(total_appts)) %>%
  mutate(num_for_factor = as.numeric(gsub("\\D", "", appt_years_post_diag))) %>%
  mutate(appt_years_post_diag = fct_reorder(appt_years_post_diag, num_for_factor)) %>%
  ungroup()

ggplot(appts_by_year, aes(x = appt_years_post_diag, y = total_appts)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~diag_year, nrow = 1, scales = "free_x")  

#most common treatment specialties 
common_tretspefs <- los_sample_op_events_agg %>%
  mutate(appt_years_post_diag = sub("\\(.*", "", appt_years_post_diag)) %>%
  group_by(appt_years_post_diag, tretspef) %>%
  summarise(total_appts = sum(total_appts)) %>%
  pivot_wider(., names_from = "appt_years_post_diag", values_from = "total_appts", values_fill = 0) %>%
  ungroup()

common_tretspefs <- common_tretspefs[,c(1,6,2,3,4,5)]

write.csv(common_tretspefs, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Sample_OP_appts_by_tretspef_20240112_sample3.csv")

#treatment specialties in subsets of patients 
tretspefs_by_tg <- los_sample_op_events %>%
  group_by(site_icd10_o2_3char, tretspef) %>%
  summarise(total_appts = n())

write.csv(tretspefs_by_tg, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Tretspefs_by_tumour_group_20240116.csv")


####### Treatment specialties for patients with radiotherapy records ########
query <- "with randomsample as (
    select *
    from analysiselizabethaugarde.los2014_cohort
    where rownum <= 5000
    order by dbms_random.value
)
select 
    rs.tumourid, 
    rs.patientid, 
    rs.follow_up_start, 
    rs.follow_up_end, 
    rs.deathdatebest, 
    c.attendkeyanon,
    c.datayear,
    c.apptdate,
    c.attended,
    c.stafftyp,
    c.tretspef, 
    a.event_date
from randomsample rs
left join av2019.at_pathway@casref01 a on rs.tumourid = a.tumourid
left join heslive.hes_linkage_av_op@casref01 b on rs.patientid = b.patientid
left join heslive.hesop@casref01 c on b.attendkeyanon = c.attendkeyanon and b.datayear = c.datayear
where c.datayear in ('1314', '1415', '1516', '1617', '1718', '1819', '1920')
  and rs.patientid is not null
  and a.event_type = 18"

los2014_sample_op_events_RT <- dbGetQueryOracle(casref01, query, rowlimit = NA)

write.csv(los2014_sample_op_events_RT, "N:/INFO/_LIVE/NCIN/Macmillan_Partnership/Length of Stay - 2023/Data/Tretspefs_RT_patients_20240104.csv")

