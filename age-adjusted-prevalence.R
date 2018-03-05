# This script reproduces some results from 
# "Continuous NHANES Web Tutorial Sample Code & Datasets"
# https://www.cdc.gov/nchs/tutorials/NHANES/Downloads/intro.htm#15

# Using the survey and srvyr packages, reproduce results for tutorial 
#   "Age Standardization and Population Counts"
#   Task 1a: How to Generate Age-Adjusted Prevalence Rates in SUDAAN
#   Step 1: How to Generate Age-Adjusted Prevalence Rates in SUDAAN
# by Michael Laviolette, PhD, MPH, statman54@gmail.com
library(tidyverse)
library(survey)
library(srvyr)
# also used: haven

setwd("My directory")
# Download data and documentation
# If download from R fails, download manually using indicated URL's
# dataset
download.file("ftp://ftp.cdc.gov/pub/health_statistics/nchs/tutorial/nhanes/Continuous/analysis_data.sas7bdat",
              "analysis_data.sas7bdat")
# standard population proportions for NHANES groupings
download.file("https://www.cdc.gov/nchs/tutorials/nhanes/downloads/Continuous/ageadjwt.xls",
              "ageadjwt.xls")
# SAS program to generate age-adjusted prevalence rates
download.file("ftp://ftp.cdc.gov/pub/health_statistics/nchs/tutorial/nhanes/Continuous/adjprev_sudaan.sas",
              "adjprev_sudaan.sas")
# output of SAS program to generate age-adjusted prevalence rates
download.file("https://www.cdc.gov/nchs/tutorials/nhanes/downloads/Continuous/adjprevalence.pdf",
              "adjprevalence.pdf")

# import from SAS data
analysis_data <- haven::read_sas("analysis_data.sas7bdat")

# number and mean of non-missing systolic blood pressure entries for 
#   each respondent
sbp <- analysis_data %>% 
  select(SEQN, starts_with("BPXSY")) %>% 
  gather("key", "value", -SEQN, na.rm = TRUE) %>% 
  group_by(SEQN) %>% 
  summarize(n_sbp = n(),
            mean_sbp = mean(value))
# confirm that unweighted frequencies match output from SAS PROC FREQ
sbp %>% count(n_sbp)

# number and mean of non-missing diastolic blood pressure entries for 
#   each respondent
dbp <- analysis_data %>% 
  select(SEQN, starts_with("BPXDI")) %>% 
  gather("key", "value", -SEQN, na.rm = TRUE) %>% 
  # some DBP values are 0; set to missing
  mutate(value = replace(value, value == 0, NA)) %>% 
  # SEQN is unique respondent ID
  group_by(SEQN) %>% 
  summarize(n_dbp = n(),
            mean_dbp = mean(value, na.rm = TRUE))

# recode as per SAS code
working_data <- 
  # add sbp and dbp to data
  reduce(list(analysis_data, dbp, sbp), inner_join, by = "SEQN") %>% 
  # recode race and age group
  mutate(RIAGENDR = factor(RIAGENDR, 1:2, c("Male", "Female")),
         age = cut(RIDAGEYR, c(20, 40, 60, Inf), right = FALSE),
         race = case_when(RIDRETH1 == 3 ~ "NH-White", 
                          RIDRETH1 == 4 ~ "NH-Black", 
                          RIDRETH1 == 1 ~ "Mex-Am",
                          RIDRETH1 %in% c(2, 5) ~ "Other",
                          TRUE ~ NA_character_),
         race = factor(race),
         # reorder factors as "NH-White", "NH-Black", "Mex-Am", "Other"
         race = fct_relevel(race, "NH-White", "NH-Black")) %>% 
  # construct high blood pressure indicator HBP (0 = No, 1 = Yes)
  # unweighted frequencies match SAS PROC FREQ when rows with n_sbp = 0 (i.e., 
  #   records with no BP measurements) are removed
  mutate(HBP_trt = 
           case_when(BPQ050A == 1 ~ 1,
                     BPQ020 %in% 1:2 & BPQ050A %in% c(2, NA) ~ 0,
                     TRUE ~ NA_real_)) %>% 
  mutate(SBP140 = ifelse(mean_sbp >= 140, 1, 0),
         DBP90 = ifelse(mean_dbp >= 90, 1, 0),
         # counting missings for DBP90 matches SAS results
         DBP90 = replace(DBP90, DBP90 %in% c(0, NA), 0)) %>% 
  mutate(HBP = case_when(HBP_trt == 0 & SBP140 == 0 & DBP90 == 0 ~ 2, 
                         HBP_trt %in% 0:1 ~ 1,
                         # TRUE ~ as.numeric(HBP_trt)
                         TRUE ~ NA_real_),
         HBP = factor(HBP, 1:2, c("Yes", "No")))
rm(sbp, dbp, analysis_data)

# check against SAS 
lapply(working_data[c("RIAGENDR", "age", "race")], summary)

# age       Frequency
# ------------------
#     .         5996
# 20-39         3238
# 40-59         2711
# 60+           3107
# 
# race       Frequency
# --------------------
# NH-White        5954
# NH-Black        3472
# Mex-Am          4348
# Other           1278
#  
# RIAGENDR    Frequency
# ---------------------
#   Male           7309
#   Female         7743

lapply(working_data[c("n_sbp", "n_dbp", "HBP_trt", "SBP140", "DBP90", 
                      "HBP")], table, exclude = NULL)
# n_sbp    Frequency
# ------------------
#   1         413
#   2        1173
#   3       13466
# 
# n_dbp    Frequency
# ------------------
#   1         413
#   2        1174
#   3       13465
# 
# HBP_trt    Frequency
# ------------------
#   .        3967
#   0        9162
#   1        1923
# 
# SBP140    Frequency
# ------------------
#   0       12978
#   1        2074
# 
# DBP90    Frequency
# ------------------
#   0       14471
#   1         581

# HBP    Frequency
# ------------------
#   .        3967
#   1        3201
#   2        7884

# create initial survey object
svy_crude <- working_data %>% 
  as_survey_design(ids = SDMVPSU, strata = SDMVSTRA, weights = WTMEC4YR, 
                   nest = TRUE)

# for each domain, construct adjusted survey object and obtain prevalence
# standard population for age groups 20-39, 40-59, 60+
std_pop <- c(77670, 72816, 45364)

# Population age 20 and older
# Age-adjusted prevalence of high blood pressure per SUDAAN
# --------------------------------
#        Sample           Standard
#        Size   Percent   Error
# --------------------------------
# Total  8960   29.3433   0.8381

# since adjusting over entire survey, need a variable with the same
#   value for all records--since not present, creating on the fly with
#   update function
svystandardize(update(svy_crude, dummy = 1), by = ~ age, over = ~ dummy,
               population = std_pop,
               excluding.missing = ~ age + HBP) %>%
  filter(RIDAGEYR >= 20) %>% 
  summarize(n = unweighted(n()),
    pct = survey_mean(HBP == "Yes")) %>% 
  mutate_at(vars(starts_with("pct")), function(x) 100 * x)

# by gender
# SUDAAN results
# ----------------------------------
#         Sample           Standard
# Gender  Size   Percent   Error
# ----------------------------------
# Male    4228   28.3277   1.2083
# Female  4732   29.9706   0.7126

svystandardize(svy_crude, by = ~ age, over = ~ RIAGENDR,
               population = std_pop,
               excluding.missing = ~ age + RIAGENDR + HBP) %>% 
  filter(RIDAGEYR >= 20) %>% 
  group_by(RIAGENDR) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(HBP == "Yes")) %>% 
  mutate_at(vars(starts_with("pct")), function(x) 100 * x)

# by race
# SUDAAN results
# -----------------------------------------------------
#                 Sample            Standard
# Race-Ethnicity  Size    Percent   Error
# -----------------------------------------------------
# NH-White        4439    27.8638   0.9714
# NH-Black        1677    40.8659   1.0472
# Mex-Am          2087    26.0541   0.9851
# Other            757    30.5625   2.2547

svystandardize(svy_crude, by = ~ age, over = ~ race,
               population = std_pop,
               excluding.missing = ~ age + race + HBP) %>% 
  filter(RIDAGEYR >= 20) %>% 
  group_by(race) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(HBP == "Yes")) %>% 
  mutate_at(vars(starts_with("pct")), function(x) 100 * x)

# by race and gender
# SUDAAN results
# -----------------------------------------------------
#                        Sample           Standard
# Gender  Race-Ethnicity Size   Percent   Error
# -----------------------------------------------------
# Male    NH-White       2143   27.1904   1.3029
# Male    NH-Black        800   39.6611   1.2386
# Male    Mex-Am          957   25.9010   1.4114
# Male    Other           328   27.7263   3.5277
# Female  NH-White       2296   28.1902   0.8825
# Female  NH-Black        877   41.8446   1.4807
# Female  Mex-Am         1130   25.9149   1.1827
# Female  Other           429   32.0618   2.4540

svystandardize(svy_crude, by = ~ age, over = ~ RIAGENDR + race,
               population = std_pop,
               excluding.missing = ~ age + RIAGENDR + race + HBP) %>% 
  filter(RIDAGEYR >= 20) %>% 
  group_by(RIAGENDR, race) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(HBP == "Yes")) %>% 
  mutate_at(vars(starts_with("pct")), function(x) 100 * x)

# next revision will include function to compute prevalence
