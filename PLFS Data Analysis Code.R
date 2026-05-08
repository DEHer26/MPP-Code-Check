---
  ---
  title: "PLFS Data 2023-24"
output:
  html_document:
  keep_md: true
pdf_document: default

---
  ---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

install.packages(c(
  "haven",
  "dplyr",
  "tidyr",
  "ggplot2",
  "labelled",
  "survey",
  "srvyr",
  "gtsummary",
  "scales",
  "forcats",
  "matrixStats",
  "moments",
  "Hmisc",
  "margins",
  "MASS",
  "brant",
  "lmtest",
  "sandwich",
  "car",
  "DescTools",
  "sampleSelection",
  "cowplot",
  "conflicted",
  "ordinal",
  "kableExtra",
  "modelsummary",
  "broom",
  "gt",
  "texreg",
  "estimatr",
  "fixest"
))

install.packages("pandoc")



```{r}
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(labelled)
library(survey)
library(srvyr)
library(gtsummary)
library(scales)
library(forcats)
library(matrixStats)
library(moments)
library(Hmisc)
library(margins)
library(MASS)
library(brant)
library(lmtest)
library(sandwich)
library(car)
library(DescTools)
library(sampleSelection)
library(cowplot)
library(conflicted)

library(modelsummary)
library(broom)
library(gt)
library(fixest)
library(ordinal)
library(knitr)
library(kableExtra)
library (pandoc)
library(openxlsx)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::count)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::recode)


```


```{r}
# Set working directory 
setwd ()
getwd()

```



```{r}

# STAGE 1: Load .dta to R

sink("stage1_output.txt", split = TRUE) 

df <- read_dta("plfs2023_24_complete.dta")

# Check rows, columns and variables

cat("Dimensions (rows x columns):", dim(df), "\n")
cat("\nAll variable names:\n")
print(names(df))
glimpse(df)

# Check important variables for this study

required_vars <- c(
  "age", "sex", "sector", "social_group", "religion", "marital_status",
  "gen_edu", "tech_edu", "yrs_frml_edu", "curr_attnd",
  "voc_train", "dur_train", "train_field", "type_train",
  "source_fund_train", "train_compl",
  "prnc_status_code", "subs_status_code", "subs_cap",
  "prnc_ind_code", "prnc_occ_code",
  "prnc_ent_code", "prnc_workers", "prnc_job_cntrct",
  "prnc_paid_leave", "prnc_ssb",
  "subs_ent_code", "subs_job_cntrct", "subs_ssb",
  "ERSW_1", "ERSW_2",
  "hh_cons_exp", "hh_size", "hh_type",
  "comb_wt", "state", "district", "hhid", "pid"
)

all(required_vars %in% names(df))

# Check and clean missing variables

missing_vars <- required_vars[!required_vars %in% names(df)]

if (length(missing_vars) == 0) {
  cat("\nAll required variables present.\n")
} else {
  cat("\nMISSING VARIABLES — check before proceeding:\n")
  print(missing_vars)
}

# Missing value counts on all key variables

cat("\n--- Missing value counts on key variables ---\n")

key_check <- c("age", "sex", "sector", "gen_edu", "yrs_frml_edu",
               "voc_train", "dur_train", "train_field",
               "prnc_status_code", "subs_status_code",
               "prnc_ent_code", "prnc_job_cntrct", "prnc_ssb",
               "ERSW_1", "ERSW_2", "hh_cons_exp", "hh_size", "comb_wt")

print(sapply(df[key_check], function(x) sum(is.na(x))))

# Vocational training vs duration consistency , voc_train==1 (formally trained)

cat("\n--- Vocational training vs duration cross-check ---\n")
cat("(Row = voc_train, Col = dur_train. \n")

print(table(as.numeric(df$voc_train), as.numeric(df$dur_train), useNA = "ifany"))

# Earnings distribution before cleaning

cat("\n--- ERSW_1 summary before cleaning ---\n")
print(summary(as.numeric(df$ERSW_1)))

cat("\n--- ERSW_2 summary before cleaning ---\n")
print(summary(as.numeric(df$ERSW_2)))

# hh_cons_exp check 

cat("\n--- hh_cons_exp summary ---\n")
print(summary(as.numeric(df$hh_cons_exp)))

cat("Zero or negative hh_cons_exp:", sum(as.numeric(df$hh_cons_exp) <= 0, na.rm = TRUE), "\n")

# Job contract data check (mostly for prnc_status_code==31)

cat("\n--- Job contract by principal status (top rows shown) ---\n")
print(table(as.numeric(df$prnc_status_code), as.numeric(df$prnc_job_cntrct),
            useNA = "ifany")[1:10, ])

# Age range and weights

cat("\n--- Age range in raw data ---\n")
print(summary(df$age))

cat("\n--- Survey weight summary ---\n")
print(summary(df$comb_wt))

cat("Zero or negative weights:", sum(df$comb_wt <= 0, na.rm = TRUE), "\n")

sum(df$age == 0, na.rm = TRUE)
sum(df$age > 100, na.rm = TRUE)
sum(df$comb_wt > 100000, na.rm = TRUE)

# Saving raw data 
saveRDS(df, "plfs2023_24_raw.rds")

# Read raw data in case of crash
df_raw <- read_dta("plfs2023_24_complete.dta")

# CSV file - original file is .dta
write.csv(df_raw, "plfs2023_24_complete.csv", row.names = FALSE)
file.exists("plfs2023_24_complete.csv")

sink()
```


```{r}
sink("stage2_output.txt", split = TRUE)

# STAGE 2: Create Sample

cat("\n========== STAGE 2: SAMPLE SELECTION ==========\n")

cat("Raw data N:", nrow(df), "\n")

# Age 18-50

df_s1 <- dplyr::filter(df, age >= 18 & age <= 50)
cat("After age 18-50:", nrow(df_s1), "\n")

# Education Group (gen_edu >= 5, excluding not literate/literate no school)

df_s2 <- dplyr::filter(df_s1, as.numeric(gen_edu) >= 5)
cat("After education floor (gen_edu >= 5):", nrow(df_s2), "\n")

# prnc_status_code non-missing - Principal status code (main economic activity)

df_s3 <- dplyr::filter(df_s2, !is.na(prnc_status_code))
cat("After non-missing prnc_status_code:", nrow(df_s3), "\n")

# Removing currently enrolled students - 91 (principal activity = attending education)

df_s4 <- dplyr::filter(df_s3, as.numeric(prnc_status_code) != 91)
cat("After excluding students (prnc==91):", nrow(df_s4), "\n")

# Weight is not missing and greater than 0

df_sample <- dplyr::filter(df_s4, !is.na(comb_wt) & comb_wt > 0)
cat("After non-missing positive weight:", nrow(df_sample), "\n")

cat("\nFINAL ANALYTICAL SAMPLE (Sample A):", nrow(df_sample), "\n") 

# Data to see step wise drop

attrition <- data.frame(
  Step        = c("Raw data", "Age 18-50", "Education >= below primary",
                  "prnc_status_code non-missing", "Exclude students",
                  "Non-missing positive weight"),
  N_remaining = c(nrow(df), nrow(df_s1), nrow(df_s2),
                  nrow(df_s3), nrow(df_s4), nrow(df_sample)),
  N_dropped   = c(0,
                  nrow(df) - nrow(df_s1),
                  nrow(df_s1) - nrow(df_s2),
                  nrow(df_s2) - nrow(df_s3),
                  nrow(df_s3) - nrow(df_s4),
                  nrow(df_s4) - nrow(df_sample))
)

print(attrition)

write.csv(attrition, "attrition_table.csv", row.names = FALSE)

sink()
```


```{r}
sink("stage3_output.txt", split = TRUE)

# STAGE 3: Creating Variables for Analysis

cat("\n========== STAGE 3: VARIABLE CREATION ==========\n")

# Converting to numeric form

df_sample <- df_sample %>%
  mutate(
    edu_num    = as.numeric(gen_edu),
    voc_num    = as.numeric(voc_train),
    dur_num    = as.numeric(dur_train),
    prnc_num   = as.numeric(prnc_status_code),
    subs_num   = as.numeric(subs_status_code),
    ent_num    = as.numeric(prnc_ent_code),
    cntrct_num = as.numeric(prnc_job_cntrct),
    ssb_num    = as.numeric(prnc_ssb),
    sex_num    = as.numeric(sex),
    sector_num = as.numeric(sector),
    sg_num     = as.numeric(social_group),
    rel_num    = as.numeric(religion),
    ms_num     = as.numeric(marital_status),
    ERSW_1_num = as.numeric(ERSW_1),
    ERSW_2_num = as.numeric(ERSW_2),
    hh_cons_num = as.numeric(hh_cons_exp),
    hh_size_num = as.numeric(hh_size),
    district_num = as.numeric(district),
    state_num    = as.numeric(state),
    yrs_edu_num  = as.numeric(yrs_frml_edu),
    dur_train_num = as.numeric(dur_train),
    train_field_num = as.numeric(train_field),
    type_train_num  = as.numeric(type_train),
    fund_num = as.numeric(source_fund_train),
    ind_num  = as.numeric(prnc_ind_code),
    occ_num  = as.numeric(prnc_occ_code),
    workers_num = as.numeric(prnc_workers)
  )

# Labour Force Status 

df_sample <- df_sample %>%
  mutate(
    lab_for = case_when(
      prnc_num %in% c(11,12,21,31,41,51) ~ 1,  
      prnc_num == 81 ~ 2,                      
      TRUE ~ 0                                 
    )
  )

# Education category

df_sample <- df_sample %>%
  mutate(
    lab_comp = case_when(
      prnc_num == 91 ~ 3,   
      TRUE ~ lab_for
    )
  )

# Categories based on Principal status code

df_sample <- df_sample %>%
  mutate(
    lab_cb = case_when(
      lab_comp == 3 ~ 1,  
      lab_comp == 1 ~ 2,  
      lab_comp == 2 ~ 3,  
      lab_comp == 0 ~ 4,  
      TRUE ~ NA_real_
    )
  )

#  Subsidiary Code Status

df_sample <- df_sample %>%
  mutate(
    subs_num_clean = case_when(
      subs_num %in% c(11,12,21,31,41,51) ~ subs_num,
      TRUE ~ NA_real_
    )
  )


# Including subsidiary to labor force

df_sample <- df_sample %>%
  mutate(
    lab_cbpss = lab_cb,
    lab_cbpss = if_else(!is.na(subs_num_clean), 2, lab_cbpss)
  )

# UPSS Renaming numbers to labels
df_sample <- df_sample %>%
  mutate(
    lab_cbpss_f = factor(lab_cbpss,
                         levels = 1:4,
                         labels = c("Education", "Employed", "Unemployed", "OLF"))
  )

#  Labor Force Participation Rate (LFPR)

df_sample <- df_sample %>%
  mutate(
    lab_forpss = case_when(
      lab_cbpss %in% c(2,3) ~ 1,
      lab_cbpss %in% c(1,4) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Unemployment indicator 

df_sample <- df_sample %>%
  mutate(
    ur_psss = case_when(
      lab_cbpss == 3 ~ 1,
      lab_cbpss == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Work force Participation Rate
df_sample <- df_sample %>%
  mutate(
    wpr = if_else(lab_cbpss == 2, 1, 0)
  )

# Demographic variables 

# Groups based on Age

df_sample <- df_sample %>%
  mutate(
    age_group = case_when(
      age >= 18 & age <= 24 ~ "Youth (18-24)",
      age >= 25 & age <= 34 ~ "Young adults (25-34)",
      age >= 35 & age <= 50 ~ "Mid-career (35-50)",
      TRUE ~ NA_character_
    )
  )

# Converting to fixed oredred variable
df_sample <- df_sample %>%
  mutate(
    age_group = factor(age_group,
                       levels = c("Youth (18-24)", "Young adults (25-34)", "Mid-career (35-50)"))
  )

# Age squared
df_sample <- df_sample %>%
  mutate(
    age_squared = age^2
  )

#Dummy marital status and sex 

df_sample <- df_sample %>%
  mutate(
    female = if_else(sex_num == 2, 1L, 0L, missing = NA_integer_),
    married = if_else(ms_num == 2, 1L, 0L, missing = 0L)
  )

# Urban - Rural

df_sample <- df_sample %>%
  mutate(
    urban = if_else(sector_num == 2, 1L, 0L, missing = NA_integer_)
  )

# Youth

df_sample <- df_sample %>%
  mutate(
    youth_1824 = if_else(age >= 18 & age <= 24, 1L, 0L)
  )

cat("\nAge group distribution:\n")
print(table(df_sample$age_group, useNA = "ifany"))
modelsummary::datasummary_skim(df_sample, output = "skim.html")


# Socio-economic variables 

df_sample <- df_sample %>%
  mutate(
    log_hh_cons_exp = if_else(hh_cons_num > 0, log(hh_cons_num), NA_real_)
  )                          # Log household consumptiom

df_sample <- df_sample %>%
  mutate(
    SC    = if_else(sg_num == 2, 1L, 0L, missing = 0L),
    ST    = if_else(sg_num == 1, 1L, 0L, missing = 0L),
    OBC   = if_else(sg_num == 3, 1L, 0L, missing = 0L),
    SC_ST = if_else(sg_num %in% c(1, 2), 1L, 0L, missing = 0L)
  )                             # Social group into binary, Reference - General/Other

df_sample <- df_sample %>%
  mutate(
    Muslim    = if_else(rel_num == 2, 1L, 0L, missing = 0L),
    Christian = if_else(rel_num == 3, 1L, 0L, missing = 0L),
    Other_rel = if_else(rel_num %in% c(4, 5, 6, 7, 9), 1L, 0L, missing = 0L)
  )                              # Religion , Reference - Hindu


cat("\nlog_hh_cons_exp summary:\n")
print(summary(df_sample$log_hh_cons_exp))


# Education variables 

df_sample <- df_sample %>%
  mutate(
    edu_group = case_when(
      edu_num %in% c(1,2,3,4) ~ "Not literate / literate no school",
      edu_num == 5 ~ "Below primary",
      edu_num == 6 ~ "Primary",
      edu_num == 7 ~ "Middle",
      edu_num == 8 ~ "Secondary",
      edu_num == 10 ~ "Higher secondary",
      edu_num == 11 ~ "Diploma / certificate",
      edu_num == 12 ~ "Graduate",
      edu_num == 13 ~ "Postgraduate and above",
      TRUE ~ NA_character_
    )
  )                                              # Education categories

# To create a order - lowest to highest

df_sample <- df_sample %>%
  mutate(
    edu_group = factor(edu_group,
                       levels = c(
                         "Not literate / literate no school",
                         "Below primary", "Primary", "Middle",
                         "Secondary", "Higher secondary",
                         "Diploma / certificate", "Graduate",
                         "Postgraduate and above"
                       )
    )
  )


df_sample <- df_sample %>%
  mutate(
    secondary_complete = if_else(edu_num %in% c(8,10), 1L, 0L, missing = 0L),
    graduate_above     = if_else(edu_num %in% c(11,12,13), 1L, 0L, missing = 0L)
  )                                                  # Education Attainment Binary



cat("\nEducation group distribution:\n")
print(table(df_sample$edu_group, useNA = "ifany"))

# --- 3.7: Vocational training variables ---
# Non-formal training (voc_num codes 2-5) excluded from ALL treatment variables

df_sample <- df_sample %>%
  mutate(
    # Formal vocational training received (voc_train == 1 only)
    formal_voc_received = if_else(voc_num == 1, 1L, 0L, missing = 0L),
    
    # SHORT-DURATION proxy (< 12 months formal training)
    # dur_num 1=<3m, 2=3-6m, 3=6-12m
    short_dur_voc = if_else(
      voc_num == 1 & dur_num %in% c(1, 2, 3), 1L, 0L, missing = 0L),
    
    # TRADITIONAL VOCATIONAL (>= 12 months formal training)
    # dur_num 4=12-18m, 5=18-24m, 6=24m+
    traditional_voc = if_else(
      voc_num == 1 & dur_num %in% c(4, 5, 6), 1L, 0L, missing = 0L),
    
    # HYBRID: short-duration + secondary completed
    hybrid_credential = if_else(
      short_dur_voc == 1 & secondary_complete == 1, 1L, 0L, missing = 0L),
    
    # Vocational type for descriptives
    voc_type = case_when(
      voc_num == 1             ~ "Formal vocational",
      voc_num %in% c(2,3,4,5) ~ "Non-formal vocational",
      voc_num == 6             ~ "No training",
      TRUE                     ~ NA_character_
    ),
    voc_type = factor(voc_type, levels = c(
      "No training", "Non-formal vocational", "Formal vocational")),
    
    # Duration label (for descriptives among formally trained only)
    dur_label = case_when(
      dur_num == 1 ~ "<3 months",
      dur_num == 2 ~ "3-6 months",
      dur_num == 3 ~ "6-12 months",
      dur_num == 4 ~ "12-18 months",
      dur_num == 5 ~ "18-24 months",
      dur_num == 6 ~ "24+ months",
      TRUE         ~ NA_character_
    ),
    dur_label = factor(dur_label, levels = c(
      "<3 months","3-6 months","6-12 months",
      "12-18 months","18-24 months","24+ months"))
  )



# --- 3.8: Credential type ---


df_sample <- df_sample %>%
  mutate(
    credential_type = case_when(
      graduate_above == 1                                     ~ "Graduate and above",
      hybrid_credential == 1                                  ~ "Hybrid: short-duration + secondary",
      short_dur_voc == 1 & secondary_complete == 0            ~ "Short-duration vocational only",
      traditional_voc == 1                                    ~ "Traditional vocational (>=12m)",
      secondary_complete == 1 & formal_voc_received == 0      ~ "General secondary only",
      edu_num < 8                                             ~ "Below secondary",
      TRUE                                                    ~ NA_character_
    ),
    
    # Reference category: General secondary only
    
    # In regression, set reference using relevel()
    
    credential_type = factor(credential_type, levels = c(
      "General secondary only",        # REFERENCE — listed first
      "Below secondary",
      "Traditional vocational (>=12m)",
      "Short-duration vocational only",
      "Hybrid: short-duration + secondary",
      "Graduate and above"))
  )

cat("\n--- Credential type distribution (unweighted) ---\n")
print(table(df_sample$credential_type, useNA = "ifany"))



# --- 3.9: Formal employment variables ---

df_sample <- df_sample %>%
  mutate(
    # Component indicators
    has_ssb        = if_else(ssb_num %in% 1:7, 1L, 0L, missing = 0L),
    has_contract   = if_else(cntrct_num %in% c(2,3,4), 1L, 0L, missing = 0L),
    govt_corporate = if_else(ent_num %in% c(5,6,7,8), 1L, 0L, missing = 0L),
    
    # PRIMARY definition: (contract OR govt/corporate) AND SSB
    formal_employment = if_else(
      (has_contract == 1 | govt_corporate == 1) & has_ssb == 1,
      1L, 0L, missing = 0L),
    # Set to NA for non-employed
    formal_employment = if_else(lab_cbpss == 2, formal_employment, NA_integer_),
    
    # STRICT: contract > 1 year AND SSB
    formal_strict = if_else(
      cntrct_num %in% c(3,4) & has_ssb == 1, 1L, 0L, missing = 0L),
    formal_strict = if_else(lab_cbpss == 2, formal_strict, NA_integer_),
    
    # ENTERPRISE ONLY
    formal_enterprise = if_else(govt_corporate == 1, 1L, 0L, missing = 0L),
    formal_enterprise = if_else(lab_cbpss == 2, formal_enterprise, NA_integer_),
    
    # CONTRACT ONLY
    formal_contract = if_else(has_contract == 1, 1L, 0L, missing = 0L),
    formal_contract = if_else(lab_cbpss == 2, formal_contract, NA_integer_),
    
    # SSB component indicators
    ssb_pf_pension = if_else(ssb_num %in% c(1,4,5,7), 1L, 0L, missing = 0L),
    ssb_gratuity   = if_else(ssb_num %in% c(2,4,6,7), 1L, 0L, missing = 0L),
    ssb_health_mat = if_else(ssb_num %in% c(3,5,6,7), 1L, 0L, missing = 0L),
    ssb_none       = if_else(ssb_num == 8, 1L, 0L, missing = 0L)
  )

# --- 3.10: Employment status (ordinal 0-3 for Model 3) ---

df_sample <- df_sample %>%
  mutate(
    employment_status_num = case_when(
      lab_cbpss == 4                            ~ 0L,  # Not in LF
      lab_cbpss == 3                            ~ 1L,  # Unemployed
      lab_cbpss == 2 & formal_employment == 0   ~ 2L,  # Informal employed
      lab_cbpss == 2 & formal_employment == 1   ~ 3L,  # Formal employed
      TRUE                                      ~ NA_integer_
    ),
    employment_status = factor(employment_status_num,
                               levels = 0:3,
                               labels = c("Not in LF", "Unemployed", "Informal employed", "Formal employed"),
                               ordered = TRUE)
  )

cat("\nEmployment status distribution:\n")
print(table(df_sample$employment_status, useNA = "ifany"))

# --- 3.11: Earnings ---

df_sample <- df_sample %>%
  mutate(
    monthly_earnings = case_when(
      !is.na(ERSW_1_num) & !is.na(ERSW_2_num) ~ ERSW_1_num + ERSW_2_num,
      !is.na(ERSW_1_num)                        ~ ERSW_1_num,
      TRUE                                       ~ NA_real_
    ),
    monthly_earnings = if_else(monthly_earnings <= 0, NA_real_, monthly_earnings),
    log_earnings = log(monthly_earnings)
  )

cat("\nMonthly earnings summary (wage workers only):\n")
print(summary(df_sample$monthly_earnings))

# --- 3.12: Worker type and factor labels ---

df_sample <- df_sample %>%
  mutate(
    worker_type = factor(case_when(
      age >= 18 & age <= 24 ~ "Youth (18-24)",
      age >= 25 & age <= 34 ~ "Young adults (25-34)",
      age >= 35 & age <= 50 ~ "Mid-career (35-50)",
      TRUE ~ NA_character_
    ), levels = c("Youth (18-24)", "Young adults (25-34)", "Mid-career (35-50)")),
    
    sex_f     = factor(sex_num, 1:3, c("Male","Female","Third gender")),
    sector_f  = factor(sector_num, 1:2, c("Rural","Urban")),
    sg_f      = factor(sg_num, c(1,2,3,9),
                       c("ST","SC","OBC","General/Others")),
    rel_f     = factor(rel_num, c(1,2,3,4,5,6,7,9),
                       c("Hindu","Islam","Christian","Sikh",
                         "Jain","Buddhist","Zoroastrian","Others")),
    ms_f      = factor(ms_num, 1:4,
                       c("Never married","Currently married",
                         "Widowed","Divorced/Separated")),
    state_f   = factor(state_num)
  )

sink()
```


```{r}
sink("stage4_output.txt", split = TRUE)

################################################################################
# STAGE 4:  Vertification of contructed variables
################################################################################


# 4.1 Formal employment rates by all four definitions (among employed only)
cat("Formal employment rates by definition (among UPSS employed, weighted):\n")

df_sample %>%
  dplyr::filter(lab_cbpss == 2) %>%
  summarise(
    pct_primary    = weighted.mean(formal_employment, comb_wt, na.rm = TRUE) * 100,
    pct_strict     = weighted.mean(formal_strict,     comb_wt, na.rm = TRUE) * 100,
    pct_enterprise = weighted.mean(formal_enterprise, comb_wt, na.rm = TRUE) * 100,
    pct_contract   = weighted.mean(formal_contract,   comb_wt, na.rm = TRUE) * 100
  ) %>% print()


# 4.2 Earnings distribution after cleaning

cat("\nEarnings after cleaning (should be positive only):\n")
print(summary(df_sample$monthly_earnings))

cat("Share of employed with earnings data:\n")

df_sample %>%
  dplyr::filter(lab_cbpss == 2) %>%
  summarise(
    pct_with_earnings = mean(!is.na(monthly_earnings)) * 100
  ) %>% print()

# 4.3 Employed with missing enterprise code

cat("\nEmployed with missing enterprise code:\n")

df_sample %>%
  dplyr::filter(lab_cbpss == 2) %>%
  summarise(missing_ent = sum(is.na(ent_num))) %>% print()

# 4.4 Training-duration consistency in sample

cat("\nVoc_train==1 with missing dur_train (should be 0 or near 0):\n")

df_sample %>%
  dplyr::filter(voc_num == 1) %>%
  summarise(missing_dur = sum(is.na(dur_num))) %>% print()

# 4.5 NA check on all factor variables

cat("\nNA check on all new variables:\n")

check_vars <- c("age_group","female","married","urban","youth_1824",
                "SC","ST","OBC","SC_ST","Muslim","Christian",
                "secondary_complete","graduate_above",
                "short_dur_voc","traditional_voc","hybrid_credential",
                "credential_type","formal_employment","employment_status_num",
                "log_hh_cons_exp")

sapply(df_sample[check_vars], function(x) sum(is.na(x)))

sink()
```



```{r}
sink("stage5_output.txt", split = TRUE)
################################################################################
# STAGE 5: Survey Design
################################################################################



# Full analytical sample (Sample A) — used for Model 3 and all descriptives
plfs_design <- svydesign(
  ids     = ~hhid,
  weights = ~comb_wt,
  data    = df_sample,
  nest    = TRUE
)

# Employed subsample (Sample B) — used for Models 1, 2, descriptive Tables 7-11

plfs_design_emp <- subset(plfs_design, lab_cbpss == 2)

# Wage worker subsample (Sample C) — used for earnings regression and Table 13

plfs_design_wages <- subset(plfs_design, lab_cbpss == 2 & !is.na(monthly_earnings))

cat("Survey design objects created:\n")

cat("  plfs_design (Sample A):", nrow(df_sample), "obs\n")

cat("  plfs_design_emp (Sample B):", sum(df_sample$lab_cbpss == 2), "obs\n")

cat("  plfs_design_wages (Sample C):",
    sum(df_sample$lab_cbpss == 2 & !is.na(df_sample$monthly_earnings)), "obs\n")

# Create subsets of df_sample for interactions

df_emp   <- dplyr::filter(df_sample, lab_cbpss == 2)
df_wages <- dplyr::filter(df_sample, lab_cbpss == 2 & !is.na(monthly_earnings))

# Save cin case of crash

saveRDS(df_sample, "plfs_analytical_sample.rds")

cat("Reload next session with: df_sample <- readRDS('plfs_analytical_sample.rds')\n")

readRDS('plfs_analytical_sample.rds')
sink()
```


```{r}
sink("stage6_output.txt", split = TRUE)


################################################################################
# STAGE 6: DESCRIPTIVE STATISTICS (Tables 1-13, Figures 1-6)
################################################################################


# Weighted mode helper function
weighted_mode <- function(x, w) {
  x <- x[!is.na(x) & !is.na(w)]
  w <- w[!is.na(x) & !is.na(w)]
  tab <- tapply(w, x, sum)
  as.numeric(names(tab)[which.max(tab)])
}

# Weighted frequency table helper function
wtd_freq <- function(data, var, wt_var = "comb_wt") {
  data %>%
    dplyr::count({{ var }}, wt = .data[[wt_var]]) %>%
    mutate(Pct = round(n / sum(n) * 100, 2),
           N_wtd = round(n)) %>%
    dplyr::select(-n)
}

```
```{r TABLE 1: BALANCE TABLE}
cat("\n=== TABLE 1: BALANCE TABLE BY CREDENTIAL TYPE ===\n")

balance_cont <- df_sample %>%
  dplyr::filter(!is.na(credential_type)) %>%
  group_by(credential_type) %>%
  summarise(
    mean_age        = round(weighted.mean(age, comb_wt, na.rm = TRUE), 2),
    mean_yrs_edu    = round(weighted.mean(yrs_edu_num, comb_wt, na.rm = TRUE), 2),
    mean_log_hh_exp = round(weighted.mean(log_hh_cons_exp, comb_wt, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(balance_cont)

balance_cont %>%
  kable(format = "html", caption = "Balance Table: Continuous Variables") %>%
  kable_styling() %>%
  save_kable("Balance_Continuous.html")
write.csv(balance_cont, "Balance_Continuous.csv", row.names = FALSE)


balance_cat <- df_sample %>%
  dplyr::filter(!is.na(credential_type)) %>%
  group_by(credential_type) %>%
  summarise(
    pct_female  = round(weighted.mean(female,  comb_wt, na.rm = TRUE) * 100, 2),
    pct_rural   = round(weighted.mean(1-urban, comb_wt, na.rm = TRUE) * 100, 2),
    pct_SC      = round(weighted.mean(SC,      comb_wt, na.rm = TRUE) * 100, 2),
    pct_ST      = round(weighted.mean(ST,      comb_wt, na.rm = TRUE) * 100, 2),
    pct_OBC     = round(weighted.mean(OBC,     comb_wt, na.rm = TRUE) * 100, 2),
    pct_Muslim  = round(weighted.mean(Muslim,  comb_wt, na.rm = TRUE) * 100, 2),
    pct_married = round(weighted.mean(married, comb_wt, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )
print(balance_cat)
write.csv(balance_cat, "Balance_Categorical.csv", row.names = FALSE)


# Balance tests
cat("\nBalance tests:\n")
svychisq(~sector_f + credential_type, plfs_design)
svychisq(~sex_f + credential_type, plfs_design)
svychisq(~sg_f + credential_type, plfs_design)


cat("\nANOVA: age vs credential_type:\n")
svyglm(age ~ credential_type, design = plfs_design)
reg <- svyglm(age ~ credential_type, design = plfs_design)
anova(reg)


# Chi-square: sector vs credential_type
chi_sector <- chisq.test(table(df_sample$sector_f, df_sample$credential_type))
chi_sector_df <- data.frame(
  Observed = as.vector(chi_sector$observed),
  Expected = as.vector(chi_sector$expected),
  Row = rep(rownames(chi_sector$observed), times = ncol(chi_sector$observed)),
  Column = rep(colnames(chi_sector$observed), each = nrow(chi_sector$observed))
)

write.csv(chi_sector_df, "Chi_Sector_vs_Credential.csv", row.names = FALSE)

#  Chi-square: sex vs credential_type
chi_sex <- chisq.test(table(df_sample$sex_f, df_sample$credential_type))
chi_sex_df <- data.frame(
  Observed = as.vector(chi_sex$observed),
  Expected = as.vector(chi_sex$expected),
  Row = rep(rownames(chi_sex$observed), times = ncol(chi_sex$observed)),
  Column = rep(colnames(chi_sex$observed), each = nrow(chi_sex$observed))
)
write.csv(chi_sex_df, "Chi_Sex_vs_Credential.csv", row.names = FALSE)



#  Chi-square: social group vs credential_type
chi_sg <- chisq.test(table(df_sample$sg_f, df_sample$credential_type))
chi_sg_df <- data.frame(
  Observed = as.vector(chi_sg$observed),
  Expected = as.vector(chi_sg$expected),
  Row = rep(rownames(chi_sg$observed), times = ncol(chi_sg$observed)),
  Column = rep(colnames(chi_sg$observed), each = nrow(chi_sg$observed))
)
write.csv(chi_sg_df, "Chi_SG_vs_Credential.csv", row.names = FALSE)


#  ANOVA: age vs credential_type
anova_age <- aov(age ~ credential_type, data = df_sample, weights = comb_wt)
anova_df <- as.data.frame(summary(anova_age)[[1]])
write.csv(anova_df, "ANOVA_Age_vs_Credential.csv", row.names = TRUE)

```

```{r }
cat("\n=== TABLE 2: UNIVARIATE STATISTICS — CONTINUOUS VARIABLES ===\n")

cont_stats <- function(x, w, label) {
  x_clean <- x[!is.na(x) & !is.na(w)]
  w_clean <- w[!is.na(x) & !is.na(w)]
  cat("\n---", label, "---\n")
  stats <- data.frame(
    N        = sum(!is.na(x)),
    Mean     = round(weighted.mean(x_clean, w_clean), 2),
    Median   = round(weightedMedian(x_clean, w_clean), 2),
    Mode     = weighted_mode(x, w),
    SD       = round(sqrt(Hmisc::wtd.var(x_clean, w_clean)), 2),
    Variance = round(Hmisc::wtd.var(x_clean, w_clean), 2),
    Min      = round(min(x_clean), 2),
    Max      = round(max(x_clean), 2),
    Range    = round(max(x_clean) - min(x_clean), 2),
    IQR      = round(diff(wtd.quantile(x_clean, w_clean, probs = c(0.25,0.75))), 2),
    CV_pct   = round(sqrt(Hmisc::wtd.var(x_clean, w_clean)) /
                       weighted.mean(x_clean, w_clean) * 100, 2),
    P10      = round(wtd.quantile(x_clean, w_clean, probs = 0.10), 2),
    P25      = round(wtd.quantile(x_clean, w_clean, probs = 0.25), 2),
    P75      = round(wtd.quantile(x_clean, w_clean, probs = 0.75), 2),
    P90      = round(wtd.quantile(x_clean, w_clean, probs = 0.90), 2),
    P99      = round(wtd.quantile(x_clean, w_clean, probs = 0.99), 2),
    Skewness = round(skewness(x_clean), 3),
    Kurtosis = round(kurtosis(x_clean), 3)
  )
  print(stats)
}

cont_stats(df_sample$age,df_sample$comb_wt, "Age")
cont_stats(df_sample$yrs_edu_num, df_sample$comb_wt, "Years of formal education")
cont_stats(df_sample$log_hh_cons_exp, df_sample$comb_wt, "Log household consumption expenditure")
cont_stats(df_wages$monthly_earnings, df_wages$comb_wt,  "Monthly earnings (wage workers only)")


# NEW FOR TABLES

cont_stats_main <- function(x, w, label) {
  x_clean <- x[!is.na(x) & !is.na(w)]
  w_clean <- w[!is.na(x) & !is.na(w)]
  
  data.frame(
    Variable = label,
    N        = sum(!is.na(x)),
    Mean     = round(weighted.mean(x_clean, w_clean), 2),
    Median   = round(weightedMedian(x_clean, w_clean), 2),
    SD       = round(sqrt(Hmisc::wtd.var(x_clean, w_clean)), 2),
    Min      = round(min(x_clean), 2),
    Max      = round(max(x_clean), 2),
    Skewness = round(moments::skewness(x_clean), 3),
    Kurtosis = round(moments::kurtosis(x_clean), 3)
  )
}

# Combine variables
table2_main <- rbind(
  cont_stats_main(df_sample$age, df_sample$comb_wt, "Age"),
  cont_stats_main(df_sample$yrs_edu_num, df_sample$comb_wt, "Years of education"),
  cont_stats_main(df_sample$log_hh_cons_exp, df_sample$comb_wt, "Log household consumption"),
  cont_stats_main(df_wages$monthly_earnings, df_wages$comb_wt, "Monthly earnings (wage workers)")
)

# Save
write.csv(table2_main, "Table_2_Continuous_Main.csv", row.names = FALSE)

gt::gt(table2_main) %>%
  gt::gtsave("output/tables/table2_main.docx")


#For Appendix
# Full detailed function (everything included)
cont_stats_full <- function(x, w, label) {
  x_clean <- x[!is.na(x) & !is.na(w)]
  w_clean <- w[!is.na(x) & !is.na(w)]
  
  data.frame(
    Variable = label,
    N        = sum(!is.na(x)),
    Mean     = round(weighted.mean(x_clean, w_clean), 2),
    Median   = round(weightedMedian(x_clean, w_clean), 2),
    Mode     = weighted_mode(x, w),
    SD       = round(sqrt(Hmisc::wtd.var(x_clean, w_clean)), 2),
    Variance = round(Hmisc::wtd.var(x_clean, w_clean), 2),
    Min      = round(min(x_clean), 2),
    Max      = round(max(x_clean), 2),
    Range    = round(max(x_clean) - min(x_clean), 2),
    IQR      = round(diff(wtd.quantile(x_clean, w_clean, probs = c(0.25, 0.75))), 2),
    CV_pct   = round(sqrt(Hmisc::wtd.var(x_clean, w_clean)) /
                       weighted.mean(x_clean, w_clean) * 100, 2),
    P10      = round(wtd.quantile(x_clean, w_clean, probs = 0.10), 2),
    P25      = round(wtd.quantile(x_clean, w_clean, probs = 0.25), 2),
    P75      = round(wtd.quantile(x_clean, w_clean, probs = 0.75), 2),
    P90      = round(wtd.quantile(x_clean, w_clean, probs = 0.90), 2),
    P99      = round(wtd.quantile(x_clean, w_clean, probs = 0.99), 2),
    Skewness = round(moments::skewness(x_clean), 3),
    Kurtosis = round(moments::kurtosis(x_clean), 3)
  )
}

# Combine variables
table2_appendix <- rbind(
  cont_stats_full(df_sample$age, df_sample$comb_wt, "Age"),
  cont_stats_full(df_sample$yrs_edu_num, df_sample$comb_wt, "Years of education"),
  cont_stats_full(df_sample$log_hh_cons_exp, df_sample$comb_wt, "Log household consumption"),
  cont_stats_full(df_wages$monthly_earnings, df_wages$comb_wt, "Monthly earnings (wage workers)")
)

# Save
write.csv(table2_appendix, "Table_2_Continuous_Main_appendix.csv", row.names = FALSE)




```
```{r }
cat("\n=== TABLE 3: UNIVARIATE CATEGORICAL FREQUENCIES ===\n")

cat("\nSex:\n");              print(wtd_freq(df_sample, sex_f))
cat("\nSector:\n");            print(wtd_freq(df_sample, sector_f))
cat("\nSocial group:\n");      print(wtd_freq(df_sample, sg_f))
cat("\nReligion:\n");          print(wtd_freq(df_sample, rel_f))
cat("\nMarital status:\n");    print(wtd_freq(df_sample, ms_f))
cat("\nEducation group:\n");   print(wtd_freq(df_sample, edu_group))
cat("\nVocational type:\n");   print(wtd_freq(df_sample, voc_type))
cat("\nDuration (formally trained only):\n")
print(wtd_freq(dplyr::filter(df_sample, voc_num == 1), dur_label))
cat("\nCredential type:\n");   print(wtd_freq(df_sample, credential_type))
cat("\nEmployment status:\n"); print(wtd_freq(df_sample, employment_status))


#NEW TABLE FORMAT

wtd_freq <- function(data, var, wt_var = "comb_wt") {
  data %>%
    dplyr::count({{ var }}, wt = .data[[wt_var]]) %>%
    dplyr::rename(Category = 1) %>%   # <- critical fix
    mutate(
      Pct = round(n / sum(n) * 100, 2),
      N_wtd = round(n)
    ) %>%
    dplyr::select(Category, Pct, N_wtd)
}

sex_tbl    <- wtd_freq(df_sample, sex_f)
sector_tbl <- wtd_freq(df_sample, sector_f)
sg_tbl     <- wtd_freq(df_sample, sg_f)
rel_tbl    <- wtd_freq(df_sample, rel_f)
ms_tbl     <- wtd_freq(df_sample, ms_f)
edu_tbl    <- wtd_freq(df_sample, edu_group)
voc_tbl    <- wtd_freq(df_sample, voc_type)
dur_tbl    <- wtd_freq(dplyr::filter(df_sample, voc_num == 1), dur_label)
cred_tbl   <- wtd_freq(df_sample, credential_type)
emp_tbl    <- wtd_freq(df_sample, employment_status)


table3 <- rbind(
  cbind(Variable = "Sex", sex_tbl),
  cbind(Variable = "Sector", sector_tbl),
  cbind(Variable = "Social group", sg_tbl),
  cbind(Variable = "Religion", rel_tbl),
  cbind(Variable = "Marital status", ms_tbl),
  cbind(Variable = "Education group", edu_tbl),
  cbind(Variable = "Vocational type", voc_tbl),
  cbind(Variable = "Training duration", dur_tbl),
  cbind(Variable = "Credential type", cred_tbl),
  cbind(Variable = "Employment status", emp_tbl)
)


write.csv(table3, "Table3_Categorical.csv", row.names = FALSE)




```
```{r TABLE 4: EDUCATIONAL ATTAINMENT}
cat("\n=== TABLE 4: EDUCATIONAL ATTAINMENT  ===\n")

cross_pct <- function(data, row_var, col_var) {
  data %>%
    dplyr::filter(!is.na({{ row_var }}), !is.na({{ col_var }})) %>%
    dplyr::count({{ col_var }}, {{ row_var }}, wt = comb_wt) %>%
    group_by({{ col_var }}) %>%
    mutate(Pct = round(n / sum(n) * 100, 2)) %>%
    dplyr::select(-n) %>%
    pivot_wider(names_from = {{ col_var }}, values_from = Pct)
}

cat("\nEducation by sector:\n");     print(cross_pct(df_sample, edu_group, sector_f))
cat("\nEducation by sex:\n");        print(cross_pct(df_sample, edu_group, sex_f))
cat("\nEducation by age group:\n");  print(cross_pct(df_sample, edu_group, age_group))


```
```{r }
cat("\n=== TABLE 5: CREDENTIAL TYPE DISTRIBUTION ===\n")

cat("\n Overall:\n")

print(df_sample %>%
        dplyr::filter(!is.na(credential_type)) %>%
        dplyr::count(credential_type, wt = comb_wt) %>%
        mutate(Pct = round(n / sum(n) * 100, 2), N_wtd = round(n)) %>%
        dplyr::select(-n))

write.csv(Overall, "credential_overall.csv", row.names = FALSE)

cat("\nBy sector:\n");       print(cross_pct(df_sample, credential_type, sector_f))
cat("\nBy sex:\n");          print(cross_pct(df_sample, credential_type, sex_f))
cat("\nBy age group:\n");    print(cross_pct(df_sample, credential_type, age_group))
cat("\nBy social group:\n"); print(cross_pct(df_sample, credential_type, sg_f))



```
```{r TABLE 6: UPSS LABOUR FORCE STATUS}
cat("\n=== TABLE 6: UPSS LABOUR FORCE STATUS ===\n")

cat("\nOverall:\n")

print(wtd_freq(df_sample, lab_cbpss_f))

cat("\nLFPR and WPR by sector x sex (survey-correct with SE):\n")

svyby(~lab_forpss + wpr,
      ~sector_f + sex_f,
      subset(plfs_design, age >= 15),
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(LFPR = round(lab_forpss * 100, 2),
         WPR  = round(wpr * 100, 2)) %>%
  dplyr::select(sector_f, sex_f, LFPR, WPR) %>%
  print()

cat("\nUR by sector x sex (within labour force):\n")

svyby(~ur_psss,
      ~sector_f + sex_f,
      subset(plfs_design, lab_cbpss %in% c(2,3)),
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(UR = round(ur_psss * 100, 2)) %>%
  dplyr::select(sector_f, sex_f, UR) %>%
  print()

cat("\nLFPR and WPR by education level:\n")

svyby(~lab_forpss + wpr,
      ~edu_group,
      subset(plfs_design, !is.na(edu_group)),
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(LFPR = round(lab_forpss * 100, 2),
         WPR  = round(wpr * 100, 2)) %>%
  dplyr::select(edu_group, LFPR, WPR) %>%
  print()




```
```{r }
cat("\n=== TABLE 7: EMPLOYMENT STATUS BY CREDENTIAL TYPE ===\n")

df_sample %>%
  dplyr::filter(!is.na(credential_type), !is.na(employment_status)) %>%
  dplyr::count(credential_type, employment_status, wt = comb_wt) %>%
  group_by(credential_type) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = employment_status, values_from = Pct) %>%
  print()


# NEw 
table7 <- df_sample %>%
  filter(!is.na(credential_type), !is.na(employment_status)) %>%
  count(credential_type, employment_status, wt = comb_wt) %>%
  group_by(credential_type) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  select(-n) %>%
  pivot_wider(names_from = employment_status, values_from = Pct)

write.csv(table7, "table7_employment_status_by_credential.csv", row.names = FALSE)

```
```{r TABLE 8: FORMAL EMPLOYMENT RATE BY CREDENTIAL TYPE}
cat("\n=== TABLE 8: FORMAL EMPLOYMENT RATE BY CREDENTIAL TYPE ===\n")

cat("\nBy credential type — all three definitions (survey-correct):\n")

svyby(~formal_employment + formal_strict + formal_enterprise + formal_contract,
      ~credential_type,
      subset(plfs_design_emp, !is.na(credential_type)),
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(
    Pct_primary    = round(formal_employment * 100, 2),
    Pct_strict     = round(formal_strict * 100, 2),
    Pct_enterprise = round(formal_enterprise * 100, 2),
    Pct_contract   = round(formal_contract * 100, 2)
  ) %>%
  dplyr::select(credential_type, Pct_primary, Pct_strict,
                Pct_enterprise, Pct_contract) %>%
  print()

# Survey chi-square test

cat("\nSurvey chi-square: credential_type vs formal_employment:\n")
svy_chi_t8 <- svydesign(
  ids = ~hhid, weights = ~comb_wt,
  data = dplyr::filter(df_emp, !is.na(credential_type), !is.na(formal_employment)),
  nest = TRUE)
print(svychisq(~credential_type + formal_employment, svy_chi_t8))

# Stratified formal employment rates

cat("\nFormal employment by credential_type AND age group:\n")
svyby(~formal_employment, ~credential_type + age_group,
      subset(plfs_design_emp, !is.na(credential_type)),
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(Pct = round(formal_employment * 100, 2)) %>%
  dplyr::select(credential_type, age_group, Pct) %>%
  pivot_wider(names_from = age_group, values_from = Pct) %>%
  print()

cat("\nFormal employment by credential_type AND sector:\n")

svyby(~formal_employment, ~credential_type + sector_f,
      subset(plfs_design_emp, !is.na(credential_type)),
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(Pct = round(formal_employment * 100, 2)) %>%
  dplyr::select(credential_type, sector_f, Pct) %>%
  pivot_wider(names_from = sector_f, values_from = Pct) %>%
  print()

cat("\nFormal employment by credential_type AND sex:\n")

svyby(~formal_employment, ~credential_type + sex_f,
      subset(plfs_design_emp, !is.na(credential_type)),
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(Pct = round(formal_employment * 100, 2)) %>%
  dplyr::select(credential_type, sex_f, Pct) %>%
  pivot_wider(names_from = sex_f, values_from = Pct) %>%
  print()

cat("\nFormal employment by credential_type AND social group:\n")

svyby(~formal_employment, ~credential_type + sg_f,
      subset(plfs_design_emp, !is.na(credential_type)),
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(Pct = round(formal_employment * 100, 2)) %>%
  dplyr::select(credential_type, sg_f, Pct) %>%
  pivot_wider(names_from = sg_f, values_from = Pct) %>%
  print()



```
```{r TABLE 9: ENTERPRISE TYPE}
cat("\n=== TABLE 9: ENTERPRISE TYPE (employed only) ===\n")

df_emp %>%
  dplyr::filter(!is.na(ent_num)) %>%
  mutate(enterprise = factor(ent_num,
                             levels = c(1,2,3,4,5,6,7,8,10,11,12,19),
                             labels = c("Proprietary (male)","Proprietary (female)",
                                        "Partnership (same HH)","Partnership (diff HH)",
                                        "Govt/local body","Public sector enterprise",
                                        "Autonomous body","Public/Private ltd company",
                                        "Co-operative","Trust/non-profit",
                                        "Employer household","Others"))) %>%
  dplyr::count(enterprise, wt = comb_wt) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  dplyr::select(-n) %>%
  arrange(desc(Pct)) %>%
  print()


```
```{r TABLE 10: JOB CONTRACT TYPE}
cat("\n=== TABLE 10: JOB CONTRACT TYPE (employed only) ===\n")

df_emp %>%
  dplyr::filter(!is.na(cntrct_num)) %>%
  mutate(contract = factor(cntrct_num, 1:4,
                           c("No written contract","Written: <=1 year",
                             "Written: 1-3 years","Written: >3 years"))) %>%
  dplyr::count(contract, wt = comb_wt) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  dplyr::select(-n) %>%
  print()



```
```{r }
cat("\n=== TABLE 11: SOCIAL SECURITY COVERAGE  ===\n")

cat("\nOverall:\n")

svymean(~has_ssb + ssb_pf_pension + ssb_gratuity + ssb_health_mat + ssb_none,
        plfs_design_emp, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(Pct = round(mean * 100, 2)) %>%
  dplyr::select(Pct) %>%
  print()

cat("\nBy credential type:\n")

svyby(~has_ssb + ssb_pf_pension + ssb_none,
      ~credential_type,
      subset(plfs_design_emp, !is.na(credential_type)),
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(across(c(has_ssb, ssb_pf_pension, ssb_none),
                ~ round(.x * 100, 2))) %>%
  dplyr::select(credential_type, has_ssb, ssb_pf_pension, ssb_none) %>%
  print()

#

```
```{r }
cat("\n=== TABLE 12: VOCATIONAL TRAINING CHARACTERISTICS ===\n")

cat("\nTraining type overall:\n")
print(wtd_freq(dplyr::filter(df_sample, !is.na(voc_type)), voc_type))

cat("\nDuration (formally trained only):\n")
print(wtd_freq(dplyr::filter(df_sample, voc_num == 1), dur_label))

cat("\nTraining field (formally trained only, top 10):\n")
df_sample %>%
  dplyr::filter(voc_num == 1) %>%
  mutate(field_f = as_factor(train_field)) %>%
  dplyr::count(field_f, wt = comb_wt) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(Pct)) %>%
  head(10) %>%
  dplyr::select(-n) %>%
  print()

cat("\nFunding source (formally trained only):\n")
df_sample %>%
  dplyr::filter(voc_num == 1) %>%
  mutate(fund_f = factor(fund_num, c(1,2,9),
                         c("Government","Own funding","Others"))) %>%
  dplyr::count(fund_f, wt = comb_wt) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  dplyr::select(-n) %>%
  print()

cat("\nTraining type on-job vs full-time (formally trained only):\n")
df_sample %>%
  dplyr::filter(voc_num == 1) %>%
  mutate(type_f = factor(type_train_num, c(1,2,3),
                         c("On the job","Part time","Full time"))) %>%
  dplyr::count(type_f, wt = comb_wt) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  dplyr::select(-n) %>%
  print()


```
```{r }
cat("\n=== TABLE 13: EARNINGS STATISTICS (wage workers only) ===\n")

cont_stats(df_wages$monthly_earnings, df_wages$comb_wt, "Overall monthly earnings")

cont_stats(df_wages$log_earnings,     df_wages$comb_wt, "Overall log earnings")

cat("\nMean earnings by credential type (survey-correct):\n")

svyby(~monthly_earnings, ~credential_type,
      subset(plfs_design_wages, !is.na(credential_type)),
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(Mean = round(monthly_earnings, 0), SE = round(se, 0)) %>%
  dplyr::select(credential_type, Mean, SE) %>%
  print()

cat("\nMean earnings by formal vs informal:\n")
svyby(~monthly_earnings, ~formal_employment,
      plfs_design_wages,
      svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(Mean = round(monthly_earnings, 0), SE = round(se, 0)) %>%
  dplyr::select(formal_employment, Mean, SE) %>%
  print()

cat("\nMean earnings by sector:\n")
svyby(~monthly_earnings, ~sector_f, plfs_design_wages, svymean, na.rm=TRUE) %>%
  as.data.frame() %>%
  mutate(Mean = round(monthly_earnings, 0)) %>%
  dplyr::select(sector_f, Mean) %>% print()

cat("\nMean earnings by sex:\n")
svyby(~monthly_earnings, ~sex_f, plfs_design_wages, svymean, na.rm=TRUE) %>%
  as.data.frame() %>%
  mutate(Mean = round(monthly_earnings, 0)) %>%
  dplyr::select(sex_f, Mean) %>% print()



sink()


```


```{r FIGURES}

sink("stage6figures_output.txt", split = TRUE)

cat("\n=== FIGURES 1-6 ===\n")

# Figure 1: Formal employment rate by credential type with 95% CI
fig1_data <- svyby(~formal_employment, ~credential_type,
                   subset(plfs_design_emp, !is.na(credential_type)),
                   svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(Pct = formal_employment * 100,
         CI_lo = (formal_employment - 1.96 * se) * 100,
         CI_hi = (formal_employment + 1.96 * se) * 100)
write.csv(fig1_data, "Figure1_data.csv", row.names = FALSE)


fig1 <- ggplot(fig1_data, aes(x = fct_reorder(credential_type, Pct), y = Pct)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi), width = 0.25) +
  coord_flip() +
  labs(title = "Formal employment rate by credential type",
       x = NULL, y = "% formally employed (UPSS)",
       caption = "Source: PLFS 2023-24. Survey-weighted estimates with 95% CI.") +
  theme_minimal(base_size = 12)
print(fig1)


# Figure 2: Employment status composition by credential type
fig2_data <- svytable(~credential_type + employment_status, plfs_design) %>%
  as.data.frame() %>%
  group_by(credential_type) %>%
  mutate(Pct = round(Freq / sum(Freq) * 100, 2))
write.csv(fig2_data, "Figure2_data.csv", row.names = FALSE)

fig2 <- ggplot(fig2_data, aes(x = credential_type, y = Pct,
                              fill = employment_status)) +
  geom_col(position = "stack") + coord_flip() +
  labs(title = "Employment status composition by credential type",
       x = NULL, y = "%", fill = "Status",
       caption = "Source: PLFS 2023-24. Survey-weighted.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
print(fig2)

# Figure 3: Earnings box plot by credential type (log scale)
fig3 <- df_emp %>%
  dplyr::filter(!is.na(monthly_earnings), !is.na(credential_type)) %>%
  ggplot(aes(x = credential_type, y = monthly_earnings)) +
  geom_boxplot(outlier.alpha = 0.2) + coord_flip() +
  scale_y_log10(labels = comma) +
  labs(title = "Monthly earnings by credential type (log scale)",
       x = NULL, y = "Monthly earnings INR (log scale)",
       caption = "Source: PLFS 2023-24. Wage workers only.") +
  theme_minimal(base_size = 11)
print(fig3)

# Figure 4: LFPR by age group and credential type
fig4_data <- svyby(~lab_forpss, ~credential_type + age_group,
                   subset(plfs_design, !is.na(credential_type) & !is.na(age_group)),
                   svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(LFPR = lab_forpss * 100)
write.csv(fig4_data, "Figure4_data.csv", row.names = FALSE)

fig4 <- ggplot(fig4_data, aes(x = age_group, y = LFPR,
                              group = credential_type,
                              colour = credential_type)) +
  geom_line() + geom_point() +
  labs(title = "LFPR by age group and credential type",
       x = "Age group", y = "LFPR (%)", colour = "Credential",
       caption = "Source: PLFS 2023-24. Survey-weighted.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
print(fig4)

# Figure 5: Formal employment by credential type and sex
fig5_data <- svyby(~formal_employment, ~credential_type + sex_f,
                   subset(plfs_design_emp, !is.na(credential_type)),
                   svymean, na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(Pct = formal_employment * 100)
write.csv(fig5_data, "Figure5_data.csv", row.names = FALSE)

fig5 <- ggplot(fig5_data, aes(x = credential_type, y = Pct, fill = sex_f)) +
  geom_col(position = "dodge") + coord_flip() +
  labs(title = "Formal employment rate by credential type and sex",
       x = NULL, y = "% formally employed", fill = "Sex",
       caption = "Source: PLFS 2023-24. Survey-weighted.") +
  theme_minimal(base_size = 11)
print(fig5)

# Figure 6: Earnings histogram (log scale)
fig6 <- df_wages %>%
  ggplot(aes(x = monthly_earnings)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  scale_x_log10(labels = comma) +
  labs(title = "Distribution of monthly earnings (log scale)",
       x = "Monthly earnings INR (log scale)", y = "Count",
       caption = "Source: PLFS 2023-24. Wage workers only.") +
  theme_minimal(base_size = 11)
print(fig6)

plots <- list(fig1, fig2, fig3, fig4, fig5, fig6)

dir.create("figures", showWarnings = FALSE)

for (i in seq_along(plots)) {
  ggsave(
    filename = paste0("figures/fig", i, ".png"),
    plot = plots[[i]],
    width = 7,
    height = 5,
    dpi = 300
  )
}

sink()
list.files("figures")
list.files("figures", full.names = TRUE)
```


```{r }
save.image("backup_before_rmd.RData")
file.exists("backup_before_rmd.RData")
file.info("backup_before_rmd.RData")$size
saveRDS(df_sample, "df_sample.rds")
saveRDS(df_emp, "df_emp.rds")
saveRDS(df_wages, "df_wages.rds")
saveRDS(plfs_design, "plfs_design.rds")
saveRDS(plfs_design_emp, "plfs_design_emp.rds")
saveRDS(plfs_design_wages, "plfs_design_wages.rds")
```



```{r }
gc()
rm(list = ls(pattern = "fig"))
rm(list = ls(pattern = "table"))
gc()
```


```{r }

load("backup_before_rmd.RData")
ls()

```




sink("stage7_output.txt", split = TRUE)

# STAGE 7: REGRESSION MODELS


# PRE-REGRESSION SETUP


# Primary sample 

df_emp_primary <- df_emp %>%
  filter(edu_num >= 8)

cat("Primary sample N:", nrow(df_emp_primary), "\n")

# Collapse rare states - Fixed effects
state_freq  <- table(df_emp_primary$state_f)
rare_states <- names(state_freq[state_freq < 100])


df_emp_primary <- df_emp_primary %>%
  mutate(
    state_f      = as.character(state_f),
    state_f      = if_else(state_f %in% rare_states, "OTHER", state_f),
    state_f      = factor(state_f),
    comb_wt_norm = comb_wt / mean(comb_wt, na.rm = TRUE)
  )


#  Verify credential distribution 
cat("\nCredential type distribution in primary sample:\n")
print(table(df_emp_primary$credential_type, useNA = "ifany"))

# Formula components 

controls <- paste(
  "age + age_squared + female + married + urban",
  "SC + ST + OBC + Muslim + Christian + Other_rel",
  "log_hh_cons_exp + state_f",
  sep = " + "
)

treatment_vars_primary <- "short_dur_voc + traditional_voc + graduate_above"

treatment_vars_robust <- paste(
  "short_dur_voc + traditional_voc + hybrid_credential +",
  "graduate_above + below_secondary"
)

full_formula_str     <- paste("~", treatment_vars_primary, "+", controls)

full_formula         <- as.formula(paste("formal_employment", full_formula_str))

full_formula_str_rob <- paste("~", treatment_vars_robust,  "+", controls)

full_formula_robust  <- as.formula(paste("formal_employment", full_formula_str_rob))


# - Regression variable lists 
reg_vars <- c(
  "formal_employment", "short_dur_voc", "traditional_voc", "graduate_above",
  "age", "age_squared", "female", "married", "urban",
  "SC", "ST", "OBC", "Muslim", "Christian", "Other_rel",
  "log_hh_cons_exp", "state_f", "district_num"
)

reg_vars_robust <- c(
  "formal_employment", "short_dur_voc", "traditional_voc",
  "hybrid_credential", "graduate_above", "below_secondary",
  "age", "age_squared", "female", "married", "urban",
  "SC", "ST", "OBC", "Muslim", "Christian", "Other_rel",
  "log_hh_cons_exp", "state_f", "district_num"
)

#  Primary model dataset 

df_m1 <- df_emp_primary[complete.cases(df_emp_primary[reg_vars]), ]
cat("Model 1 N:", nrow(df_m1), "\n")

cat("\nFormal employment rate in model sample:",
    round(mean(df_m1$formal_employment, na.rm = TRUE) * 100, 2), "%\n")

# Save in case of crash

saveRDS(df_emp_primary,      "df_emp_primary.rds")
saveRDS(df_m1,               "df_m1.rds")
save(controls,
     treatment_vars_primary,
     treatment_vars_robust,
     full_formula_str,
     full_formula,
     full_formula_str_rob,
     full_formula_robust,
     reg_vars,
     reg_vars_robust,
     rare_states,
     file = "regression_setup.RData")

cat("\nPre-regression checkpoint saved.\n")

cat("                      df_emp_primary <- readRDS('df_emp_primary.rds')\n")
cat("                      df_m1 <- readRDS('df_m1.rds')\n")

load('regression_setup.RData')

df_emp_primary <- readRDS('df_emp_primary.rds')
df_m1 <- readRDS('df_m1.rds')
```




# 

# MODEL 1: OLS LINEAR PROBABILITY MODEL


cat("\n=== MODEL 1: OLS LPM ===\n")

m1 <- lm(full_formula, data = df_m1, weights = comb_wt)

m1_cluster_se <- coeftest(m1, vcov = vcovCL(m1, cluster = ~district_num))
cat("\nModel 1 results:\n")
print(m1_cluster_se)

cat("\n95% CIs:\n")
print(coefci(m1, vcov = vcovCL(m1, cluster = ~district_num)))

cat("\nF-test joint significance:\n")
print(linearHypothesis(m1,
                       c("short_dur_voc = 0", "traditional_voc = 0", "graduate_above = 0"),
                       vcov = vcovCL(m1, cluster = ~district_num)))

cat("\nR-squared:", round(summary(m1)$r.squared, 4), "\n")

cat("Adj R-squared:", round(summary(m1)$adj.r.squared, 4), "\n")

# Define cluster-robust SE function
cluster_se <- function(model, cluster_var){
  vcovCL(model, cluster = cluster_var)
}

# hide state fixed effects
omit_vars <- "^state_f"  

# Save regression table 

modelsummary(
  m1,
  vcov = cluster_se(m1, ~district_num),  # cluster at district
  coef_map = c(
    "short_dur_voc" = "Short-duration vocational training",
    "traditional_voc" = "Traditional vocational training",
    "graduate_above" = "Graduate and above",
    "age" = "Age",
    "age_squared" = "Age squared",
    "female" = "Female",
    "married" = "Married",
    "urban" = "Urban",
    "SC" = "Scheduled Caste",
    "ST" = "Scheduled Tribe",
    "OBC" = "OBC",
    "Muslim" = "Muslim",
    "Christian" = "Christian",
    "Other_rel" = "Other Religion",
    "log_hh_cons_exp" = "Log Household Consumption"
  ),
  gof_omit = "State|Adj|R2", 
  stars = TRUE,
  output = "stage7M1_table.docx", # saves as Word
  notes = "District-clustered SEs. State fixed effects included but omitted from table."
)


#Appendix Table

aic_val <- AIC(m1)
bic_val <- BIC(m1)
loglik_val <- as.numeric(logLik(m1))
rmse_val <- sqrt(mean(residuals(m1)^2))

# Create GOF table
gof_appendix <- data.frame(
  term = c("AIC", "BIC", "Log-Likelihood", "RMSE", "Num. Obs."),
  value = c(aic_val, bic_val, loglik_val, rmse_val, nobs(m1))
)

modelsummary(
  m1,
  vcov = cluster_vcov,
  coef_map = c(),       # no coefficients
  gof_map = list(),     # clear automatic GOF
  add_rows = gof_appendix,
  stars = FALSE,
  output = "OLSappendix_model_diagnostics.docx"
)


# Save Model 1

saveRDS(m1, "model1_lpm.rds")

cat("Reload with: m1 <- readRDS('model1_lpm.rds')\n")

m1 <- readRDS('model1_lpm.rds')




nrow(df_emp)
nrow(df_emp_primary)
nrow(df_m1)
colSums(is.na(df_emp_primary[reg_vars]))
summary(df_m1)

nrow(df_m1)
all.equal(df_emp_primary, df_m1)
summary(m1)$call

sink()




```{r }
sink("stage7M2_output.txt", split = TRUE)


# MODEL 2: LOGISTIC REGRESSION (feglm — avoids convergence warnings)


# Model Logit 
m2 <- feglm(
  fml     = full_formula,
  data    = df_m1,
  weights = ~comb_wt_norm,
  family  = "logit",
  cluster = ~district_num
)

# Odds ratios, 95% CI, p-values 
coef_m2 <- coef(m2)
se_m2   <- sqrt(diag(vcov(m2)))

or_table <- data.frame(
  Variable = names(coef_m2),
  OR       = exp(coef_m2),
  CI_low   = exp(coef_m2 - 1.96*se_m2),
  CI_high  = exp(coef_m2 + 1.96*se_m2),
  p_value  = 2 * (1 - pnorm(abs(coef_m2 / se_m2))),
  stringsAsFactors = FALSE
)

or_table <- or_table %>%
  mutate(
    OR_CI = paste0(round(OR,3), " [", round(CI_low,3), ", ", round(CI_high,3), "]"),
    Stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1  ~ "*",
      TRUE           ~ ""
    )
  ) %>%
  select(Variable, OR_CI, Stars)

# Compute AMEs for key binary variables 
ame_fast <- function(model, data, var){
  d1 <- data; d1[[var]] <- 1
  d0 <- data; d0[[var]] <- 0
  mean(predict(model, newdata = d1, type = "response") -
         predict(model, newdata = d0, type = "response"), na.rm = TRUE)
}

binary_vars <- c("short_dur_voc", "traditional_voc", "graduate_above")
ame_values <- sapply(binary_vars, function(v) ame_fast(m2, df_m1, v))
ame_table <- data.frame(
  Variable = binary_vars,
  AME      = round(ame_values, 4),
  stringsAsFactors = FALSE
)

# 
report_table <- left_join(or_table, ame_table, by = "Variable")

# Hide State FE
report_table <- report_table[!grepl("^state", report_table$Variable), ]

# - McFadden pseudo-R² 

pseudo_r2 <- round(1 - (m2$loglik / m2$ll_null), 4)
report_table <- report_table %>%
  add_row(
    Variable = "McFadden pseudo-R²",
    OR_CI    = as.character(pseudo_r2),
    Stars    = NA,
    AME      = NA
  )

# Table
header <- c("Variable", "OR [95% CI]", "Stars", "AME")
sep_line <- paste(rep("-", 60), collapse = "")

table_lines <- c(
  sprintf("%-28s %-17s %-6s %-6s", header[1], header[2], header[3], header[4]),
  sep_line
)

for(i in 1:nrow(report_table)){
  line <- sprintf(
    "%-28s %-17s %-6s %-6s",
    report_table$Variable[i],
    report_table$OR_CI[i],
    ifelse(is.na(report_table$Stars[i]), "", report_table$Stars[i]),
    ifelse(is.na(report_table$AME[i]), "", report_table$AME[i])
  )
  table_lines <- c(table_lines, line)
}

# ---- Add footnote ----
footnote <- "Notes: State fixed effects included but not shown. Standard errors clustered at district level. Significance levels: *** p<0.01, ** p<0.05, * p<0.1."

# ---- Save table to Word-ready text file ----
writeLines(c(table_lines, "", footnote), "model2_logit_journal.txt")

# ---- Print in console ----
cat(paste(c(table_lines, "", footnote), collapse = "\n"))


# Save Model 2

saveRDS(m2,        "model2_logit.rds")

saveRDS(ame_table, "model2_ame.rds")


m2 <- readRDS('model2_logit.rds')
sink()



sink("stage7M3_output.txt", split = TRUE)


# MODEL 3: ORDERED LOGISTIC REGRESSION



#  Clean Model 3 dataset 

df_m3 <- df_sample %>%
  filter(edu_num >= 8, !is.na(employment_status)) %>%
  mutate(comb_wt_norm = comb_wt / mean(comb_wt, na.rm = TRUE))

#  Variables 
reg_vars_m3 <- c(
  "employment_status",
  "short_dur_voc", "traditional_voc", "graduate_above",
  "age", "female", "married", "urban",
  "SC", "ST", "OBC",
  "Muslim", "Christian", "Other_rel",
  "log_hh_cons_exp"
)

df_m3 <- df_m3[complete.cases(df_m3[, reg_vars_m3]), ]
cat("Model 3 N:", nrow(df_m3), "\n")


#  Ordered logit formula 
ordered_formula <- as.formula(
  "employment_status ~ short_dur_voc + traditional_voc + graduate_above +
   age + female + married + urban +
   SC + ST + OBC + Muslim + Christian + Other_rel +
   log_hh_cons_exp"
)

#  Fit ordered logit
m3 <- clm(
  formula = ordered_formula,
  data    = df_m3,
  weights = comb_wt_norm,
  link    = "logit",
  Hess    = TRUE,
  control = list(maxIter = 200, gradTol = 1e-5)  # faster, stable
)

#  Clustered SEs 
m3_cluster_se <- coeftest(
  m3,
  vcov = vcovCL(m3, cluster = df_m3$district_num)
)

# - Odds Ratios and CI 
or <- exp(coef(m3))
ci <- exp(confint.default(m3))  


# AME

ame_all_categories <- function(model, data, var) {
  # Create datasets with var = 1 and var = 0
  d1 <- data; d1[[var]] <- 1
  d0 <- data; d0[[var]] <- 0
  
  # Predict probabilities for all categories
  p1 <- predict(model, newdata = d1, type = "prob") |> as.data.frame()
  p0 <- predict(model, newdata = d0, type = "prob") |> as.data.frame()
  
  # Ensure numeric
  p1 <- as.matrix(sapply(p1, as.numeric))
  p0 <- as.matrix(sapply(p0, as.numeric))
  
  # Compute AMEs for each category
  colMeans(p1 - p0, na.rm = TRUE)
}


# Variables for AME
binary_vars <- c("short_dur_voc", "traditional_voc", "graduate_above")

# AME output
ame_results <- do.call(rbind, lapply(binary_vars, function(v) {
  res <- ame_sim_se(m3, df_m3, v, sims = 1000)  # AME + SE for highest category
  data.frame(
    Variable = v,
    AME = round(res["AME"], 4),
    SE  = round(res["SE"], 4)
  )
}))

ame_results



# Table

main_table <- data.frame(
  Variable = names(coef(m3)),
  OR = round(or, 3),
  CI_low = round(ci[,1], 3),
  CI_high = round(ci[,2], 3),
  stringsAsFactors = FALSE
)


main_table$AME <- NA
main_table$SE <- NA
for(i in seq_along(ame_results$Variable)){
  var <- ame_results$Variable[i]
  main_table$AME[main_table$Variable == var] <- ame_results$AME[i]
  main_table$SE[main_table$Variable == var] <- ame_results$SE[i]
}

#  Save main table to Word 
write.csv(main_table, "model3_main.csv", row.names = FALSE)




# Save Model 3
saveRDS(m3, "model3_ologit_clean.rds")
saveRDS(df_m3, "df_m3_clean.rds")

m3 <- readRDS('model3_ologit_clean.rds')

df_m3 <- readRDS("df_m3_clean.rds")


sink()

```



```{r }

sink("stage7M5_output.txt", split = TRUE)

# MODEL 5: COMPLEMENTARITY TEST


cat("\n=== MODEL 5: COMPLEMENTARITY TEST (full sample — appendix) ===\n")

#   PREPARE DATA 
df_m5 <- df_emp %>%
  mutate(
    below_secondary    = if_else(edu_num < 8, 1L, 0L, missing = 0L),
    short_dur_voc_only = if_else(short_dur_voc == 1 & secondary_complete == 0, 1L, 0L),
    secondary_only     = if_else(secondary_complete == 1 & formal_voc_received == 0, 1L, 0L),
    state_f = as.character(state_f),
    state_f = if_else(state_f %in% rare_states, "OTHER", state_f),
    state_f = factor(state_f)
  )

# FORMULA 
comp_formula <- as.formula(paste(
  "formal_employment ~ short_dur_voc_only + secondary_only +",
  "hybrid_credential + traditional_voc + graduate_above +",
  "below_secondary +", controls
))

# -Sample clean
m5_vars <- c(
  "formal_employment", "short_dur_voc_only", "secondary_only",
  "hybrid_credential", "traditional_voc", "graduate_above",
  "below_secondary", "age", "age_squared", "female", "married", "urban",
  "SC", "ST", "OBC", "Muslim", "Christian", "Other_rel",
  "log_hh_cons_exp", "state_f", "district_num"
)

df_m5_clean <- df_m5[complete.cases(df_m5[m5_vars]), ]

cat("Model 5 N:", nrow(df_m5_clean), "\n")

# Model 5
m5 <- lm(comp_formula, data = df_m5_clean, weights = comb_wt)


cluster_vcov_m5 <- function(model){
  vcovCL(model, cluster = ~district_num)
}

# Wald Test
wald_test <- linearHypothesis(
  m5,
  "hybrid_credential - short_dur_voc_only - secondary_only = 0",
  vcov = cluster_vcov_m5(m5)
)

wald_f <- unname(wald_test$F[2])
wald_p <- unname(wald_test$`Pr(>F)`[2])

# Save Model 5
saveRDS(m5, "model5_complementarity.rds")



sink ()




```{r }




# STAGE 8: HETEROGENEITY AND INTERACTIONS


# Clustered SE function

cluster_se <- function(model, cluster_var){
  coeftest(model, vcov = vcovCL(model, cluster = cluster_var))
}


# RMSE-safe diagnostics function

model_diag <- function(model, data){
  preds <- predict(model)
  obs <- model$model[[1]]
  data.frame(
    N      = nobs(model),
    R2     = summary(model)$r.squared,
    Adj_R2 = summary(model)$adj.r.squared,
    AIC    = AIC(model),
    BIC    = BIC(model),
    LogLik = as.numeric(logLik(model)),
    RMSE   = sqrt(mean((obs - preds)^2))
  )
}

# Full baseline model

full_formula <- formal_employment ~
  short_dur_voc + traditional_voc + graduate_above +
  age + age_squared + female + married + urban +
  SC + ST + OBC + Muslim + Christian + Other_rel +
  log_hh_cons_exp + state_f

df_clean <- df_m1[complete.cases(df_m1[, all.vars(full_formula)]), ]


# INTERACTION MODELS 

interaction_specs <- list(
  "short_dur_voc * female",
  "short_dur_voc * urban",
  "short_dur_voc * youth_1824",
  "short_dur_voc * SC_ST",
  "short_dur_voc * log_hh_cons_exp",
  "short_dur_voc * age",
  "short_dur_voc * graduate_above",
  "graduate_above * female",
  "short_dur_voc * age_group"
)

interaction_results <- list()
interaction_diag <- list()

for(spec in interaction_specs){
  
  formula_int <- update(full_formula, paste(". ~ . +", spec))
  
  vars_needed <- all.vars(formula_int)
  d <- df_clean[complete.cases(df_clean[, vars_needed]), ]
  
  m <- lm(formula_int, data = d, weights = comb_wt)
  
  interaction_results[[spec]] <- cluster_se(m, d$district_num)
  interaction_diag[[spec]] <- model_diag(m, d)
}


# SPLIT-SAMPLE MODELS

split_conditions <- list(
  Male   = df_clean$sex_num == 1,
  Female = df_clean$sex_num == 2,
  Rural  = df_clean$sector_num == 1,
  Urban  = df_clean$sector_num == 2,
  Youth  = df_clean$age >= 18 & df_clean$age <= 24,
  Young  = df_clean$age >= 25 & df_clean$age <= 34,
  Mid    = df_clean$age >= 35 & df_clean$age <= 50
)

split_results <- list()
split_diag <- list()

for(name in names(split_conditions)){
  
  d_sub <- df_clean[split_conditions[[name]], ]
  
  if(nrow(d_sub) < 200) next
  
  m <- lm(full_formula, data = d_sub, weights = comb_wt)
  
  split_results[[name]] <- cluster_se(m, d_sub$district_num)
  split_diag[[name]] <- model_diag(m, d_sub)
}


#Save

saveRDS(list(
  interactions = interaction_results,
  interaction_diagnostics = interaction_diag,
  split_models = split_results,
  split_diagnostics = split_diag,
  heterogeneity_table = coef_df
), "stage8_full_outputs.rds")



save(
  df_clean,
  coef_df,
  interaction_results,
  interaction_diag,
  split_results,
  split_diag,
  fig8,
  file = "stage8_checkpoint.RData"
)


sink()
```



# STAGE 10–11: ROBUSTNESS CHECKS 
l
# 1. CLEAN DATA

df_m1_clean <- df_m1 %>%
  filter(edu_num >= 8) %>%
  mutate(
    state_f = as.character(state_f),
    state_f = if_else(state_f %in% rare_states, "OTHER", state_f),
    state_f = factor(state_f),
    comb_wt = comb_wt / mean(comb_wt, na.rm = TRUE)
  )

# 2. MAIN FORMULA


full_formula <- formal_employment ~
  short_dur_voc + traditional_voc + graduate_above +
  age + age_squared + female + married + urban +
  SC + ST + OBC + Muslim + Christian + Other_rel +
  log_hh_cons_exp + state_f


# 3. HELPER FUNCTIONS


# Clustered coefficient 
extract_coef <- function(model, var = "short_dur_voc") {
  
  ct <- coeftest(
    model,
    vcov = vcovCL(model, cluster = ~district_num)
  )
  
  if(!(var %in% rownames(ct))) return(NULL)
  
  data.frame(
    Variable = var,
    Beta     = ct[var, "Estimate"],
    SE       = ct[var, "Std. Error"],
    t_stat   = ct[var, "t value"],
    p_value  = ct[var, "Pr(>|t|)"],
    N        = nobs(model)
  )
}

# Formatting for tables
fmt <- function(beta, se, p){
  
  stars <- case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
  
  paste0(
    round(beta, 3), stars,
    "\n(",
    round(se, 3),
    ")"
  )
}


# 4. BASELINE MODEL


m1 <- lm(
  full_formula,
  data = df_m1_clean,
  weights = comb_wt
)

baseline_res <- extract_coef(m1)

# 5. ALTERNATIVE OUTCOMES


alt_outcomes <- c(
  "formal_strict",
  "formal_enterprise",
  "formal_contract"
)

alt_results <- bind_rows(lapply(alt_outcomes, function(y){
  
  f <- as.formula(
    paste(
      y,
      "~ short_dur_voc + traditional_voc + graduate_above +
         age + age_squared + female + married + urban +
         SC + ST + OBC + Muslim + Christian + Other_rel +
         log_hh_cons_exp + state_f"
    )
  )
  
  m <- lm(
    f,
    data = df_m1_clean,
    weights = comb_wt
  )
  
  out <- extract_coef(m)
  out$Specification <- y
  
  out
}))

# 6. ALTERNATIVE TRAINING DEFINITIONS


# 6-month cutoff
df_6m <- df_m1_clean %>%
  mutate(
    short_dur_voc =
      if_else(voc_num == 1 & dur_num %in% c(1,2), 1L, 0L),
    
    traditional_voc =
      if_else(voc_num == 1 & dur_num %in% c(3:6), 1L, 0L)
  )

m_6m <- lm(full_formula, data = df_6m, weights = comb_wt)

res_6m <- extract_coef(m_6m)
res_6m$Specification <- "6-month cutoff"

# 18-month cutoff
df_18m <- df_m1_clean %>%
  mutate(
    short_dur_voc =
      if_else(voc_num == 1 & dur_num %in% c(1:4), 1L, 0L),
    
    traditional_voc =
      if_else(voc_num == 1 & dur_num %in% c(5:6), 1L, 0L)
  )

m_18m <- lm(full_formula, data = df_18m, weights = comb_wt)

res_18m <- extract_coef(m_18m)
res_18m$Specification <- "18-month cutoff"

# 7. PLACEBO TEST


m_placebo <- lm(
  married ~ short_dur_voc + traditional_voc +
    age + female + urban + log_hh_cons_exp,
  data = df_m1_clean,
  weights = comb_wt
)

placebo_res <- extract_coef(m_placebo)

# 8. SUBGROUP ANALYSIS
run_subgroup <- function(df, condition, label){
  
  d <- df[which(condition), , drop = FALSE]
  
  if(nrow(d) < 50){
    return(data.frame(Specification = label, Beta = NA, SE = NA, t = NA, p = NA))
  }
  
  
  d$w <- as.numeric(d$comb_wt)
  
  m <- lm(
    formula = full_formula,
    data = d,
    weights = w
  )
  
  se <- coeftest(m, vcov = vcovCL(m, cluster = d$district_num))
  
  data.frame(
    Specification = label,
    Beta = se["short_dur_voc", "Estimate"],
    SE    = se["short_dur_voc", "Std. Error"],
    t     = se["short_dur_voc", "t value"],
    p     = se["short_dur_voc", "Pr(>|t|)"]
  )
}

subgroup_results <- bind_rows(
  
  run_subgroup(df_m1_clean, df_m1_clean$female == 0, "Male"),
  run_subgroup(df_m1_clean, df_m1_clean$female == 1, "Female"),
  run_subgroup(df_m1_clean, df_m1_clean$sector_num == 1, "Rural"),
  run_subgroup(df_m1_clean, df_m1_clean$sector_num == 2, "Urban")
  
)

print(subgroup_results)



# 9. MULTICOLLINEARITY


vif_results <- vif(m1)

# LOGIT COMPARISON MODEL

m2_rb <- glm(
  formal_employment ~ short_dur_voc + traditional_voc + graduate_above +
    age + age_squared + female + married + urban +
    SC + ST + OBC + Muslim + Christian + Other_rel +
    log_hh_cons_exp + state_f,
  
  data = df_m1_clean,
  family = binomial("logit"),
  weights = comb_wt_norm
)

m2_rb_se <- coeftest(
  m2_rb,
  vcov = vcovCL(m2_rb, cluster = ~district_num)
)

print(m2_rb_se)

#####END OF SCRIPT##########
