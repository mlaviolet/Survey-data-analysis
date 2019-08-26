# Reproducing results from
# Bieler et al. (2010) "Estimating model-adjusted risks, risk differences, 
#   and risk ratios from complex survey data"
# American Journal of Epidemiology 171, 618-623
# https://academic.oup.com/aje/article/171/5/618/137889/Estimating-Model-Adjusted-Risks-Risk-Differences

# Also see SUDAAN example at 
# https://sudaanorder.rti.org/examples/Logistic Example 6.pdf

library(survey)
library(srvyr)
library(dplyr)
# also used: haven

download.file("https://sudaanorder.rti.org/examples/samadulted.zip",
              "samadulted.zip", mode = "wb")

nhis <- haven::read_sas(unzip("samadulted.zip")) %>% 
  mutate(educ_3 = cut(EDUC1, c(0, 15, 16, 21), right = FALSE, 
                      include.lowest = TRUE), # recode education
         # make highest education attainment the referent
         educ_3 = relevel(educ_3, "[16,21]"),
         # recode age to groups and make 65+ the referent
         age23_3 = cut(AGE_P, c(25, 45, 65, Inf), right = FALSE),
         age23_3 = relevel(age23_3, "[65,Inf)"),
         # recode marital categories 1:3 to Married, 4 to Widowed, and
         #   5:8 to Unmarried
         mrtl_3 = case_when(R_MARITL %in% 1:3 ~ 1,
                            R_MARITL == 4 ~ 2,
                            R_MARITL %in% 5:8 ~ 3,
                            TRUE ~ NA_real_),
         mrtl_3 = factor(mrtl_3, 1:3, 
                          c("Married", "Widowed", "Unmarried")),
         SEX = factor(SEX, labels = c("Male", "Female")),
         REGION = factor(REGION),
         # make can't afford meds the referent--"
         #   Yes" means unable to afford meds
         cantafmeds = factor(AHCAFYR1, 2:1, c("No", "Yes"))) %>% 
  # create appropriate survey design
  as_survey_design(ids = PSU_P, strata = STRAT_P, weights = WTFA_SA, 
                   nest = TRUE)
# file.remove("samadulted.sas7bdat", "samadulted.zip")
summary(nhis)

nhis$variables %>% 
  select(educ_3, age23_3, mrtl_3, SEX) %>% 
  summary()

# number of observations read; should be 24,275
nrow(nhis)
# number of observations in subpopulation of interest; should be 16,469
nhis$variables %>% 
  filter(AGE_P >= 25 & MRACRPI2 == 1) %>% 
  count()

# Table 1 
# Unadjusted prevalence by age group
# Results in paper slightly off, but results obtained below agree with SUDAAN
# Using logit CIs since that's default in SUDAAN

nhis %>% 
  filter(AGE_P >= 25 & MRACRPI2 == 1) %>% 
  group_by(age23_3) %>% 
  summarize(pct = survey_mean(cantafmeds == "Yes", na.rm = TRUE, 
                              proportion = TRUE, prop_method = "logit"))

# Average marginal predictions
model_1 <- svyglm(cantafmeds ~ SEX + educ_3 + REGION + mrtl_3,
                  design = subset(nhis, AGE_P >= 25 & MRACRPI2 == 1),
                  family = quasibinomial)
predmrg_1 <- svypredmeans(model_1, ~ age23_3)
predmrg_1

# Table 2
# Model-adjusted odds ratio
model_2 <- 
  svyglm(cantafmeds ~ SEX + age23_3 + educ_3 + REGION + mrtl_3, 
         design = subset(nhis, AGE_P >= 25 & MRACRPI2 == 1),
         family = quasibinomial)

# 25-44 vs 65+
# odds ratio and confidence interval
exp(coef(model_2)["age23_3[25,45)"])
exp(confint(model_2)["age23_3[25,45)", ])

# 45-64 vs 65+
# odds ratio and confidence interval
exp(coef(model_2)["age23_3[45,65)"])
exp(confint(model_2)["age23_3[45,65)", ])

# Ratios of average marginal predictions
# 25-44 vs 65+
cntrst_1 <- svycontrast(predmrg_1, quote(`[25,45)` / `[65,Inf)`))
cntrst_1c <- svycontrast(predmrg_1, quote(log(`[25,45)` / `[65,Inf)`)))
cntrst_1
exp(confint(cntrst_1c))                         
                         
# 45-64 vs 65+
cntrst_2 <- svycontrast(predmrg_1, quote(`[45,65)` / `[65,Inf)`))
cntrst_2c <- svycontrast(predmrg_1, quote(log(`[45,65)` / `[65,Inf)`)))
cntrst_2
exp(confint(cntrst_2c))                            

# Table 3
# Age-specific prevalence differences
# Difference in unadjusted prevalences
# need prevelances as "svyby" object
prev_unadj <- 
  svyby(~ I(cantafmeds == "Yes"), ~ age23_3, 
        subset(nhis, AGE_P >= 25 & MRACRPI2 == 1), 
        svyciprop, method = "logit", na.rm = TRUE)
svycontrast(prev_unadj, list(`25-44 vs 65+` = c(-1, 1, 0), 
                             `45-64 vs 65+` = c(-1, 0, 1)))

# Difference in average marginal predictions
svycontrast(predmrg_1, quote(`[25,45)` - `[65,Inf)`))
svycontrast(predmrg_1, quote(`[45,65)` - `[65,Inf)`))
# same using contrast coefficients
svycontrast(predmrg_1, list(`25-44 vs 65+` = c(0, 1, -1), 
                            `45-64 vs 65+` = c(1, 0, -1)))

