# Libraries and dependencies ----
# Libraries
library(dplyr)
library(stringr)

# Stage
stage_table <- function(data){
  data %>% mutate(
    SUMMARY_STAGE = case_when(
      (stage_pi_detail == "Y" & stage_best_system == "Binet" & str_sub(stage_best, 1, 1) %in% c("A", "B", "C")) ~ "Staged - Other",
      (stage_pi_detail == "Y" & stage_best_system == "Chang" & str_sub(stage_best, 1, 2) %in% c("M0", "M1", "M2", "M3", "M4")) ~ "Staged - Other", 
      (stage_pi_detail == "Y" & stage_best_system == "INRG" & str_sub(stage_best, 1, 1) %in% c("L", "M")) ~ "Staged - Other",
      (stage_pi_detail == "Y" & stage_best_system == "ISS" & str_sub(stage_best, 1, 1) %in% c("1", "2", "3")) ~ "Staged - Other",
      (stage_pi_detail == "Y" & stage_best_system == "Wilms" & str_sub(stage_best, 1, 1) %in% c("5")) ~ "4",
      (stage_pi_detail == "Y" & str_sub(stage_best, 1, 1) %in% c("1","2","3","4")) ~ str_sub(stage_best, 1, 1),
      (stage_pi_detail %in% c("Y", "N")) ~ "Missing",
      (stage_pi_detail %in% c("U", "NA")) ~ "Unstageable",
      .default = "Error"
    ),
    EARLY_ADVANCED_STAGE = case_when(
      (stage_pi_detail == "Y" & stage_best_system == "Binet" & str_sub(stage_best, 1, 1) %in% c("A", "B")) ~ "Stage 1, 2",
      (stage_pi_detail == "Y" & stage_best_system == "Binet" & str_sub(stage_best, 1, 1) %in% c("C")) ~ "Stage 3, 4",
      (stage_pi_detail == "Y" & stage_best_system == "Chang" & str_sub(stage_best, 1, 2) %in% c("M0")) ~ "Stage 1, 2", 
      (stage_pi_detail == "Y" & stage_best_system == "Chang" & str_sub(stage_best, 1, 2) %in% c("M1", "M2", "M3", "M4")) ~ "Stage 3, 4",
      (stage_pi_detail == "Y" & stage_best_system == "INRG" & str_sub(stage_best, 1, 1) %in% c("L")) ~ "Stage 1, 2",
      (stage_pi_detail == "Y" & stage_best_system == "INRG" & str_sub(stage_best, 1, 1) %in% c("M")) ~ "Stage 3, 4",
      (stage_pi_detail == "Y" & stage_best_system == "Wilms" & str_sub(stage_best, 1, 1) %in% c("5")) ~ "Stage 3, 4",
      (stage_pi_detail == "Y" & str_sub(stage_best, 1, 1) %in% c("1", "2")) ~ "Stage 1, 2",
      (stage_pi_detail == "Y" & str_sub(stage_best, 1, 1) %in% c("3", "4")) ~ "Stage 3, 4",
      .default = "Error"
    ),
    STAGE = case_when(
      (stage_pi_detail == "Y" & stage_best_system == "Binet" & str_sub(stage_best, 1, 1) %in% c("A", "B")) ~ "Staged - other early",
      (stage_pi_detail == "Y" & stage_best_system == "Binet" & str_sub(stage_best, 1, 1) %in% c("C")) ~ "Staged - other advanced",
      (stage_pi_detail == "Y" & stage_best_system == "Chang" & str_sub(stage_best, 1, 2) %in% c("M0")) ~ "Staged - other early", 
      (stage_pi_detail == "Y" & stage_best_system == "Chang" & str_sub(stage_best, 1, 2) %in% c("M1", "M2", "M3", "M4")) ~ "Staged - other advanced",
      (stage_pi_detail == "Y" & stage_best_system == "INRG" & str_sub(stage_best, 1, 1) %in% c("L")) ~ "Staged - other early",
      (stage_pi_detail == "Y" & stage_best_system == "INRG" & str_sub(stage_best, 1, 1) %in% c("M")) ~ "Staged - other advanced",
      (stage_pi_detail == "Y" & stage_best_system == "ISS" & str_sub(stage_best, 1, 1) %in% c("1", "2")) ~ "Staged - other early",
      (stage_pi_detail == "Y" & stage_best_system == "ISS" & str_sub(stage_best, 1, 1) %in% c("3")) ~ "Staged - other advanced",
      (stage_pi_detail == "Y" & stage_best_system == "Wilms" & str_sub(stage_best, 1, 1) %in% c("5")) ~ "4",
      (stage_pi_detail == "Y" & str_sub(stage_best, 1, 1) %in% c("1", "2", "3", "4")) ~ str_sub(stage_best, 1, 1),
      (stage_pi_detail %in% c("Y", "N")) ~ "Missing",
      (stage_pi_detail %in% c("U", "NA")) ~ "Unstageable",
      .default = "Error"
    )
  )
}  

stage_site_group_table <- function(data){
  data %>% mutate(
    STAGE_CANCER = case_when(
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) == "C34") ~ "Lung",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) == "C15" | SITE_ICD10R4_O2_FROM2013 == "C160") ~ "Oesophagus including cardia and gastroesophageal junction",
      (SITE_ICD10R4_O2_FROM2013 %in% c("C161", "C162", "C163", "C164", "C165", 
                                       "C166", "C167", "C168", "C169")) ~ "Stomach excluding cardia and gastroesophageal junction",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) == "C18") ~ "Colon",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C19", "C20")) ~ "Rectum and rectosigmoid junction",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) == "C25") ~ "Pancreas",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) == "C43") ~ "Melanoma of skin",
      ((str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) == "C50") & GENDER == 2) ~ "Breast",
      ((str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) == "C53") & GENDER == 2) ~ "Cervix",
      ((str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C54", "C55")) & GENDER == 2) ~ "Uterus",
      (((str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) == "C56") |
        (SITE_ICD10R4_O2_FROM2013 %in% c("C570", "C571", "C572", "C573", "C574", 
                                         "C575", "C576")) |
        (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) == "C48" & 
          !(MORPH_FIX %in% c(8693, 8800, 8801, 8802, 8803, 
                             8804, 8805, 8806, 8810, 8963, 
                             8990, 8991, 9040, 9041, 9042, 
                             9043, 9044, 9490, 9500)) & 
          !(MORPH_FIX %in% c(8811:8921)) &
          !(MORPH_FIX %in% c(9120:9373)) &
          !(MORPH_FIX %in% c(9530:9582))
         )) & GENDER == 2) ~ "Ovary, fallopian tube and primary peritoneal carcinomas",
      ((str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C61")) & GENDER == 1) ~ "Prostate",
      ((str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C62")) & GENDER == 1)  ~ "Testis",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C64")) ~ "Kidney, except renal pelvis",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C67")) ~ "Bladder",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C81")) ~ "Hodgkin lymphoma",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C73")) ~ "Thyroid",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) == "C32" | SITE_ICD10R4_O2_FROM2013 == "C101") ~ "Larynx including anterior surface of epiglottis",
      (DIAGNOSISYEAR <= 2018 & (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C01", "C09") | SITE_ICD10R4_O2_FROM2013 %in% c("C051", "C052", "C100", "C102", "C103",
                                                                                                                             "C104", "C105", "C106", "C107", "C108",
                                                                                                                             "C109"))) ~ "Oropharynx, base of tongue, tonsil, soft palate and uvula",
      (DIAGNOSISYEAR > 2018 & (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C01", "C09") | SITE_ICD10R4_O2_FROM2013 %in% c("C051", "C052", "C100", "C102", "C103",
                                                                                                                            "C104", "C105", "C106", "C107", "C108",
                                                                                                                            "C109", "C024"))) ~ "Oropharynx, base of tongue, tonsil, soft palate and uvula",
      (DIAGNOSISYEAR <= 2018 & (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C02", "C03", "C04", "C06") | SITE_ICD10R4_O2_FROM2013 %in% c("C050","C003","C004","C005"))) ~ "Oral cavity, hard palate and lip (inner aspect)",
      (DIAGNOSISYEAR > 2018 & (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C03", "C04", "C06") | SITE_ICD10R4_O2_FROM2013 %in% c("C050", "C003", "C004", "C005", "C020",
                                                                                                                                  "C021", "C022", "C023", "C028", "C029"))) ~ "Oral cavity, hard palate and lip (inner aspect)",
      (str_sub(SITE_ICD10R4_O2_FROM2013, 1, 3) %in% c("C82", "C83", "C84", "C85", "C86")) ~ "Non-Hodgkin lymphoma",
      .default = "Other"
    )
  )
}