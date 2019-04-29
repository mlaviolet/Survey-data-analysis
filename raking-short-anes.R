# using anesrake package to rake simple artificial survey
# data source: Addinsoft SARL as demo for XLSTAT Excel add-on
# https://help.xlstat.com/customer/en/portal/articles/2062302-raking-a-survey-sample-using-xlstat?b_id=9283
# data spreadsheet:
# https://help.xlstat.com/customer/portal/kb_article_attachments/59989/original.xls?1437066865
# code by Michael Laviolette PhD MPH, statman54@gmail.com

library(anesrake)
sex_lbl <- c("Male", "Female")
age_lbl <- c("<30", "30-44", "45+")
# construct data frame of survey data
satisfy <- # satisfaction score on scale of 1 to 5 (response variable)
  c(2,5,2,3,4,3,3,3,4,2,2,3,2,3,4,3,3,2,3,3,4,3,3,3,2,
    3,3,3,2,1,4,4,3,3,2,3,4,2,3,3,3,5,3,1,4,3,3,4,4,2,
    3,3,3,5,4,4,5,3,4,4,5,3,3,4,3,3,3,3,2,4,4,3,3,4,3,
    2,4,4,3,4,4,4,5,3,3,4,4,4,3,2,2,4,3,4,3,4,4,3,3,3,
    3,4,4,4,4,3,3,3,3,2,3,3,2,2,5,4,5,2,4,4,4,3,4,4,2,
    4,4,3,4,3,4,2,3,3,2,4,3,4,4,3,5,2,4,4,3,4,5,3,3,3,
    3,2,3,4,4,4,2,4,4,2,3,5,2,2,3,3,3,3,3,4,4,3,3,4,4,
    4,4,4,4,4,4,3,2,3,3,3,3,4,4,4,3,3,4,3,4,4,4,3,3,2)

gender <- 
  factor(c(1,2,1,1,2,1,1,2,1,2,1,1,2,1,1,1,1,2,2,1,2,1,1,2,1,
           2,1,1,2,2,1,1,2,1,2,2,1,1,1,1,2,1,1,1,1,1,1,1,2,1,
           1,1,1,1,2,1,1,2,2,2,2,2,2,2,2,2,1,1,1,1,2,1,2,1,2,
           1,1,2,1,1,2,1,1,1,1,1,1,2,1,1,2,2,2,2,1,1,2,2,1,2,
           1,1,2,1,2,1,2,2,1,1,1,2,1,1,1,2,1,1,2,1,2,2,2,1,1,
           2,2,1,1,1,2,1,2,1,2,2,1,1,1,2,2,1,2,2,2,2,1,2,2,1,
           2,1,2,1,1,2,2,1,1,1,2,2,1,2,2,2,1,2,2,1,1,1,2,2,2,
           1,2,1,2,2,2,2,1,1,2,1,1,1,2,1,1,2,2,1,1,1,1,2,1,1),
         labels = sex_lbl)

age <-
  factor(c(2,3,2,1,2,2,2,2,3,2,2,1,2,2,2,2,2,2,2,2,2,2,2,3,2,
           3,3,3,1,2,2,3,2,2,2,1,3,2,2,2,2,2,2,3,2,2,2,2,2,1,
           3,3,2,3,2,2,2,2,2,2,2,3,2,2,1,2,2,2,1,2,2,3,2,2,1,
           2,2,1,2,2,1,2,2,2,2,2,2,2,3,2,2,1,3,2,2,2,3,2,2,2,
           3,1,2,1,2,2,1,2,2,2,2,2,2,1,2,2,3,1,2,2,2,2,2,2,2,
           2,3,1,1,2,1,2,2,2,2,2,2,2,2,1,3,2,2,2,1,2,1,1,2,1,
           2,1,1,2,2,2,2,2,2,2,2,3,2,1,2,1,1,2,3,3,1,3,3,2,2,
           2,2,2,2,2,2,2,3,2,3,3,2,2,2,3,1,2,1,2,3,2,2,2,3,2),
         labels = age_lbl)

emp_dat <- data.frame(caseid = seq_along(satisfy), gender, age, satisfy)
rm(gender, age, satisfy)
summary(emp_dat)

# set up population targets
target_gender <- c(3800, 6200)
names(target_gender) <- sex_lbl
target_age <- c(2000, 5000, 3000)
names(target_age) <- age_lbl
# list of normalized targets
targets <- lapply(list(target_gender, target_age), function(x) x / sum(x))
names(targets) <- names(emp_dat)[2:3]

# now do the raking using age and sex margins
# sample is SRS with equal weight, so no base weights
outsave <- anesrake(targets, emp_dat, caseid = emp_dat$caseid)

weightassess(targets, emp_dat, outsave$weightvec)

# compare raked and target weights
# sample is 200 from population of 10000, so multiply raked weights by 50
#   and targets by 10000 to get original target values
rake_dat <- cbind(emp_dat, raked_wt = 50 * outsave$weightvec)
aggregate(raked_wt ~ gender, data = rake_dat, sum)
aggregate(raked_wt ~ age, data = rake_dat, sum)
lapply(targets, function(x) x * 10000)


