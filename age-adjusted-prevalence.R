# This code reproduces results for tutorial for age-adjusted prevalence from 
#   survey data using functions from survey and srvyr packages
# Also uses tidyverse functions
# https://www.cdc.gov/nchs/tutorials/NHANES/Downloads/intro.htm#15
# Age Standardization and Population Counts
# Task 1a: How to Generate Age-Adjusted Prevalence Rates in SUDAAN
# Step 1: How to Generate Age-Adjusted Prevalence Rates in SUDAAN

# Michael Laviolette PhD MPH, statman54@gmail.com

library(tidyverse)
library(survey)
library(srvyr)
# also used: haven

# set project directory or use "here" package
# SAS dataset
download.file("ftp://ftp.cdc.gov/pub/health_statistics/nchs/tutorial/nhanes/Continuous/analysis_data.sas7bdat",
              "analysis_data.sas7bdat", mode = "wb")
# standard proportions for NHANES population groupings
download.file("https://www.cdc.gov/nchs/tutorials/nhanes/downloads/Continuous/ageadjwt.xls",
              "ageadjwt.xls", mode = "wb")
# SAS program to generate age-adjusted prevalence rates, if you have SAS and SUDAAN
download.file("ftp://ftp.cdc.gov/pub/health_statistics/nchs/tutorial/nhanes/Continuous/adjprev_sudaan.sas",
              "adjprev_sudaan.sas", mode = "wb")
# output of program to generate age-adjusted prevalence rates
download.file("https://www.cdc.gov/nchs/tutorials/nhanes/downloads/Continuous/adjprevalence.pdf",
              "adjprevalence.pdf", mode = "wb")

# import from SAS data
analysis_data <- haven::read_sas("analysis_data.sas7bdat")

# Computing prevalence of high blood pressure
# number and mean of non-missing systolic blood pressure entries for 
#   each respondent
sbp <- analysis_data %>% 
  select(SEQN, starts_with("BPXSY")) %>% 
  gather("key", "value", -SEQN, na.rm = TRUE) %>% 
  group_by(SEQN) %>% 
  summarize(n_sbp = n(),
            mean_sbp = mean(value))

# number and mean of non-missing diastolic blood pressure entries for 
#   each respondent
# SEQN is unique respondent ID
dbp <- analysis_data %>% 
  select(SEQN, starts_with("BPXDI")) %>% 
  gather("key", "value", -SEQN, na.rm = TRUE) %>% 
  # set DBP values recorded as 0 to missing
  mutate(value = na_if(value, 0)) %>% 
  group_by(SEQN) %>% 
  summarize(n_dbp = n(),
            mean_dbp = mean(value, na.rm = TRUE)) 

# construct recoded data --------------------------------------------------
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
         # reorder factors as above
         race = fct_relevel(race, "NH-White", "NH-Black")) %>% 
  # construct high blood pressure indicator HBP
  # frequencies match SAS when rows with n_sbp = 0 are removed (i.e., no 
  #   BP measurements)
  mutate(HBP_trt = 
           case_when(BPQ050A == 1 ~ 1,
                     BPQ020 %in% 1:2 & BPQ050A %in% c(2, NA) ~ 0)) %>% 
  mutate(SBP140 = if_else(mean_sbp >= 140, 1, 0),
         DBP90 = if_else(mean_dbp >= 90, 1, 0),
         # counting missings for DBP90 matches SAS results
         DBP90 = replace(DBP90, DBP90 %in% c(0, NA), 0)) %>% 
  mutate(HBP = case_when(HBP_trt == 0 & SBP140 == 0 & DBP90 == 0 ~ "No", 
                         HBP_trt %in% 0:1 ~ "Yes",
                         TRUE ~ NA_character_),
         HBP = factor(HBP))
rm(sbp, dbp, analysis_data)

# check
lapply(working_data[c("RIAGENDR", "age", "race", "HBP")], summary)
lapply(working_data[c("n_sbp", "n_dbp", "HBP_trt", "SBP140", "DBP90")], 
       table, exclude = NULL)

# age-adjusted prevalence -------------------------------------------------
# standard population, age groups 20-39, 40-59, 60+
std_pop <- c(77670, 72816, 45364)
# unadjusted survey object 
svy_crude <- working_data %>% 
  as_survey_design(ids = SDMVPSU, strata = SDMVSTRA, weights = WTMEC4YR, 
                   nest = TRUE)

# For each domain, construct adjusted survey object and obtain prevalence
# Population age 20 and older
# Results from SUDAAN
# --------------------------------
#        Sample           Standard
#        Size   Percent   Error
# --------------------------------
# Total  8960   29.3433   0.8381

# since adjusting over entire dataset, need a variable with the same
#   value throughout--since not present, creating on the fly with
#   update function
svystandardize(update(svy_crude, dummy = 1), by = ~age, over = ~dummy,
               population = std_pop,
               excluding.missing = ~age + HBP) %>%
  filter(RIDAGEYR >= 20) %>% 
  summarize(n = unweighted(n()),
    pct = survey_mean(HBP == "Yes")) %>% 
  # convert to percent
  mutate_at(vars(starts_with("pct")), function(x) 100 * x)

# by gender
# ----------------------------------
#         Sample           Standard
# Gender  Size   Percent   Error
# ----------------------------------
# Male    4228   28.3277   1.2083
# Female  4732   29.9706   0.7126

svystandardize(svy_crude, by = ~age, over = ~RIAGENDR,
               population = std_pop,
               excluding.missing = ~age + RIAGENDR + HBP) %>% 
  filter(RIDAGEYR >= 20) %>% 
  group_by(RIAGENDR) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(HBP == "Yes")) %>% 
  mutate_at(vars(starts_with("pct")), function(x) 100 * x)

# by race
# -----------------------------------------------------
#                 Sample            Standard
# Race-Ethnicity  Size    Percent   Error
# -----------------------------------------------------
# NH-White        4439    27.8638   0.9714
# NH-Black        1677    40.8659   1.0472
# Mex-Am          2087    26.0541   0.9851
# Other            757    30.5625   2.2547

svystandardize(svy_crude, by = ~age, over = ~race,
               population = std_pop,
               excluding.missing = ~age + race + HBP) %>% 
  filter(RIDAGEYR >= 20) %>% 
  group_by(race) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(HBP == "Yes")) %>% 
  mutate_at(vars(starts_with("pct")), function(x) 100 * x)

# by race and gender
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

svystandardize(svy_crude, by = ~age, over = ~RIAGENDR + race,
               population = std_pop,
               excluding.missing = ~age + race + RIAGENDR + HBP) %>% 
  filter(RIDAGEYR >= 20) %>% 
  group_by(RIAGENDR, race) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(HBP == "Yes")) %>% 
  mutate_at(vars(starts_with("pct")), function(x) 100 * x)

# END ---------------------------------------------------------------------

