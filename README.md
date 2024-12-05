# Length_of_stay
NCRAS-Macmillan project exploring length of stay for cancer patients diagnosed in 2014

This project uses Hospital Episode Statistics (HES) data linked to cancer registrations, to explore secondary care activity for cancer patients diagnosed in 2014, for up to 5 years post-diagnosis. The project includes extraction and linkage of data for inpatient admissions, outpatient appointments, and A&E attendances. It cleans and prepares these datasets, and analyses the data to calculate age-specific rates of admissions, appointments, and attendances per patient per year post-diagnosis, stratified by clinical and demographic characteristics. 

This project is part of the partnership between the National Cancer Registration and Analysis Service (NCRAS) in NDRS, and Macmillan Cancer Support. 

Project status: 
This project is in development. The code will be actively maintained annually by the analyst(s) working on the project, though there is no specified end point for code development. 


Point of contact: 
TBC 


Data requirements: 
- cancer registrations in AV2022 and related tables for IMD, cancer site 
- routes to diagnosis data from AV2020.RTD2020
- HES data in HESLIVE, including linkage tables 


Outputs: 
The project produces age-specific rates of admissions, appointments and attendances per patient per year post-diagnosis, stratified by clinical and demographic characteristics. It has the functionality to output these to Excel workbooks for each type of HES data, and to generate bar graphs of the data. 


Prerequisites:
None beyond standard Level 2 CAS access


How to install and use:
Clone the Github repo and run the scripts in numerical order and/or as required for analysis purposes


License:
MIT (see license file)


Other legal or regulatory requirements:
None

Other notes:
The below is taken from a technical summary document developed for handover purposes by the analyst who developed this project (Lizzie Augarde) in November 2024:

-----
Data extraction:
The project uses a cohort of patients diagnosed in 2014. The table of this cohort is in Lizzie’s analysis space (ask dev team for access), but can also be recreated using the 1.Cohort extract.sql query. There are various inclusion and exclusion criteria applied to the cohort, detailed in the comments in the query file. 

Data prep:
There are then 4 data prep scripts, one for the cohort, one for the general population, and one for each part of the HES data (inpatient admissions, outpatient appointments, and A&E attendances).  
-	2. Data prep – cohort. R – this pulls in the cohort table from SQL and creates a cleaned cohort data frame which contains all the details of each patient diagnosed in 2014, including their age at 12 months/2 years/3 years/4 years/5 years post-diagnosis and whether they were alive at each of these points. There is then a function which cycles through all values of a given characteristic (e.g. ethnicity) and calculates the number of patients in each 10 year age band who were alive at the end of each period. It then combines all these data frames (e.g. one for White, one for Asian etc) into a single data frame containing the number of patients who were alive at each time post-diagnosis, by 10 year age band and ethnicity, for example. This can be run multiple times to get survival cohorts for each characteristic of interest
-	3. Data prep – general population data.R – this script was created to enable comparisons with general population HES data for the HACA poster (see folder). The script reads in published inpatient, outpatient and A&E HES data (pre-processed in this folder), then calculates age-specific admission/appointment/attendance rates for the general population
-	4. Data prep – inpatient.R – this extracts linked inpatient admissions data for the patient cohort. The query takes a long time to run, so it is best to run it for one or two years of data at a time, or load in the existing RData files (lines 56-59) (last extracted on 04/12/2024). The script then cleans the data, filters to keep only cancer-related admissions, then aggregates to calculate the number of admissions for each patient within each time period. These are 'period-specific', meaning the number of admissions in 3 years is the total number for that patient between 2 years and 3 years post-diagnosis.
-	5. Data prep – Outpatient.R – this script does the same as 4 for outpatient HES data. Outpatient appointments are included if the treatment specialty for the appointment is one of the top 25 specialties associated with all appointments for cancer patients in the first 5 years post-diagnosis (see EXPLORATORY scripts). A full list of these is here. 
-	6. Data prep – A&E.R – this script does the same as 4 and 5 for A&E HES data. A&E data are not filtered for cancer-related attendances, as it is not possible to identify whether an attendance is a consequence of the patient’s cancer. 

Stage_functions_tidy.R:
-	Contains 2 functions to create grouped stage variables from various staging systems 
-	Limits stage reporting to cancers with stage_pi_detail == “Y” i.e. is considered stageable and has a valid stage value 
-	Provided by Chloe Bright, used in the national statistics CMA publication 

Analysis:
There are 3 analysis scripts which contain the code required to the analysis produced in October 2024. Each contain a similar function across inpatient, outpatient and A&E data. It uses the aggregated LOS per patient data frames (ae_patient_agg, apc_patient_agg, op_patient_agg) created in the "Data prep" scripts. The function cycles through all values of a given characteristic (e.g. ethnicity) and calculates the number of admissions/appointments/attendances in each 10 year age band which occur during each period post-diagnosis, after filtering to only those patients alive at the end of that time period. It then combines all these data frames (e.g. one for White, one for Asian etc) into a single data frame containing the number of admissions/appointments/attendances in each time period, amongst patients who survived to at least the end of that time period, by 10 year age band and ethnicity, for example.
The scripts then create an age-specific rate of admissions/appointments/attendances by time period post-diagnosis, using the phe_rate function (package here). It then suppresses the results 
Finally, the analysis scripts joins the 2 data frames generated by these functions ("combined_survival_cohort_ethnicity" and "combined_total_atts_ethnicity" for example) on time period, age group and characteristic value, and calculates a rate of admissions/appointments/attendances per patient within each group, for each time period post-diagnosis (using the phe_rate function). This final table is then written out into a single workbook containing all the results for all characteristics. Any numbers (of patients or admissions) between 0 and 4 are converted to "<5" then all remaining numbers are rounded the nearest 5. Rates and CIs are calculated before any suppression is applied
There is also code for a bar graph of the age-specific rates, though this doesn’t (yet) account for the other variable (e.g. ethnicity). 

Other scripts:
There are a few other scripts in the project, which represent parts of the exploratory data analysis completed for this project. These are not up to date or fully commented. 

