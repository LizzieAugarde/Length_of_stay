library(NDRSAfunctions)
library(tidyverse)
library(janitor)

casref01 <- createConnection()

los2014_op_events_2014_query <- "
SELECT a.tumourid, 
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
FROM analysiselizabethaugarde.los2014_cohort a 
LEFT JOIN heslive.hes_linkage_av_op@casref01 b ON a.patientid = b.patientid 
LEFT JOIN heslive.hesop@casref01 c ON b.attendkeyanon = c.attendkeyanon AND b.datayear = c.datayear
WHERE (c.datayear > 2013 AND c.datayear < 2015)
AND a.patientid IS NOT NULL
"

# Fetch data
los2014_op_events_2014 <- dbGetQueryOracle(casref01, los2014_op_events_2014_query, rowlimit = NA)

