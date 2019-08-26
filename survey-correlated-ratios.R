library(survey)
library(srvyr)
library(magrittr)
# also used: stringr

# experimenting with nonlinear contrasts to develop method for computing
#   rate ratios and confidence intervals comparing a subregion (e.g. county)
#   to a parent region (e.g. state)

# using data from survey package
data(api)
dclus1 <- 
  as_survey_design(apiclus1, id = dnum, weights = pw, fpc = fpc)

# compute survey totals for
# A = indicator Yes in subregion
# B = indicator NO in subregion
# C = indicator Yes in parent region
# D = indicator No in parent region
# prevalence in subregion = A / (A + B)
# prevalence in parent region = (A + C) / (A + B + C + D)
# Rate ratio of subregion to parent region is
#   (A / (A + B)) / ((A + C) / (A + B + C + D))

# example is ratio of Met Comparable Improvement Target (comp.imp) of 
#   year-round schools (yr.rnd) to that of all schools

# get estimated totals for categories described above
ab <- svytotal(~ interaction(yr.rnd, comp.imp), dclus1) %>% 
  # simplify level names
  setNames(stringr::str_remove(names(.), "(.*?)\\)"))

# rate ratio
#        A        B        C      D
# "Yes.Yes" "No.Yes" "Yes.No" "No.No"   

ratio_ab <- ab %>% 
  svycontrast(
    quote((Yes.Yes / (No.Yes + Yes.Yes))
          / ((Yes.No + Yes.Yes) / (No.No + Yes.No + No.Yes + Yes.Yes))
    ))  
ratio_ab

# confidence interval
ab %>% 
  svycontrast(
    quote(log((Yes.Yes / (No.Yes + Yes.Yes))
          / ((Yes.No + Yes.Yes) / (No.No + Yes.No + No.Yes + Yes.Yes))
    ))) %>% 
  exp() %>% 
  confint()


# num <- "Yes.Yes"
# den <- "No.Yes"
# svycontrast(ab, quote(Yes.Yes / No.Yes))


# indicator.subregion
#  1         2         3         4
# "No.No"   "Yes.No"  "No.Yes"  "Yes.Yes"
# indicator in subregion, about 0.0602
# (coef(ab)[4] /  sum(coef(ab)[c(3, 4)]) ) /
# # indicator in parent region, about 0.049
# (sum(coef(ab)[c(2, 4)]) / sum(coef(ab)))
# # ratio about 1.22
# 
#             
# ab %>% 
#   svycontrast(c(-1, -1, -1, 1, 1, 1))            

