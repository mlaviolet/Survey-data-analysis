# This code expands on the example provided in documentation to 
#   survey::svystandardize() by replicating all point estimates in NCHS Data Brief No. 92, 
#     April 2012, # "Total and High-density Lipoprotein Cholesterol in Adults: National 
#     Health and Nutrition Examination Survey, 2009-2010"
#   http://www.cdc.gov/nchs/data/databriefs/db92.htm
#   http://www.cdc.gov/nchs/data/databriefs/db92.pdf
# Replicating age-adjusted estimates in Figure 1
#   http://www.cdc.gov/nchs/data/databriefs/db92_fig1.png
# As noted in documentation, standard errors do not exactly match NCHS estimates
# Michael Laviolette PhD MPH, statman54@gmail.com

library(dplyr)
library(srvyr)
library(survey)
data(nhanes)

# convert variables of interest to factor
nhanes <- nhanes %>% 
  # code variables to factors
  #   race: 1 = Hispanic, 2 = non-Hispanic white, 3 = non-Hispanic black, 
  #     4 = other
  #   RIAGENDR (gender): 1 = male, 2 = female
  #   HI_CHOL (high cholesterol): 1 = Yes, 0 = No
  mutate(race = factor(race, 1:4, 
                       c("Hispanic", "Non-Hispanic white", 
                         "Non-Hispanic black", "Other")),
         RIAGENDR = factor(RIAGENDR, 1:2, c("Men", "Women")),
         # indicator for high cholesterol
         HI_CHOL = factor(HI_CHOL, 1:0, c("Yes", "No")),
         # this is to have a variable with same value throughout;
         #   needed to standardize over entire sample
         all_adults = 1) 

# create survey design object
design <- as_survey_design(nhanes, ids = SDMVPSU, strata = SDMVSTRA, 
                           weights = WTMEC2YR, nest = TRUE)

# function to compute estimates of high cholesterol for age 20+, standardized 
#   by age groups
# single argument is subpopulation over which standardization occurs, as string
getPrevalence <- function(over) {
  group_vars <- syms(over)
  svystandardize(design, by = ~ agecat, over = make.formula(over),
                 # using NCHS standard population for ages 6-19, 20-39, 
                 #   40-59, 60+
                 population = c(55901, 77670, 72816, 45364), 
                 # only HI_CHOL has missing values
                 excluding.missing = ~ HI_CHOL) %>% 
    filter(agecat != "(0,19]") %>% 
    group_by(!!!group_vars) %>% 
    summarize(pct = survey_mean(HI_CHOL == "Yes", na.rm = TRUE)) %>% 
    mutate_at("pct", function(x) round(100 * x, 1)) %>% 
    mutate_at("pct_se", function(x) round(100 * x, 3))
  }

# Both sexes, all race and ethnicity groups (that is, all adults age 20+)
# CDC prevalence: 13.4
getPrevalence("all_adults")

# By sex, all race-ethnicity groups
# Men   12.2
# Women 14.3
getPrevalence("RIAGENDR")

# By race-ethnicity group, both sexes
#         Hispanic   Non-Hispanic white   Non-Hispanic black
# Total     14.5            13.5                 10.3
getPrevalence("race")

# By race-ethnicity group and sex
#       Hispanic   Non-Hispanic white   Non-Hispanic black
# Men     15.4            11.4                 10.2
# Women   13.2            15.4                 10.3
getPrevalence(c("race", "RIAGENDR"))

### END

