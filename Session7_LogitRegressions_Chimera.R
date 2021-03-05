############################################################
# BUS 462 | Spring 2021 | Session 7
#  LOGIT REGRESSIONS using Chimera Data
# CK 
# 04 MAR 2021 
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
set.seed(42) # Set a seed to ensure repeatable random samples

# libraries
require(data.table)
require(stargazer)
require(ggplot2)
require(PerformanceAnalytics)

#LOAD DATA  ###
dt <- fread("C:/Users/kalig/Dropbox/SFU TEACHING/BUS 462/datasets and cases/chimeraCSV_full.csv")

str(dt) # check the structure
head(dt)


###########################################

# Model exit as a DV

# DV: Exit
# IV: Boss Survey, job satisfaction, rank, salary 

# let's create a smaller data table
#dt1 <- dt[,c("exit", "boss_survey", "job_satisfaction","rank","salary")]
dt1 <- dt[,c("exit", "boss_survey", "job_satisfaction","salary")]

# 

# summary stats
stargazer(dt1,type="text",summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))
# note that rank and exit are categorial variables

# check if analyzable -- any missing data for admit's?
table(dt1$exit,dt1$rank)
xtabs(~exit + rank, data = dt1) # alternate way of seeing it / with col and row names

#scatterplot and correlation matrix
chart.Correlation(dt1, histogram=TRUE, pch=19) # get's busy

# OLS
m.OLS.KS <- lm(exit ~ ., data=dt1)
summary(m.OLS.KS)
AIC(m.OLS.KS)


stargazer(m.OLS.KS,type="text")

# LOGIT
dt1$exit <- as.factor(dt1$exit)
#dt1$rank <- as.factor(dt1$rank)

m.LOGIT <- glm(exit ~ ., data = dt1, family = "binomial")
summary(m.LOGIT)

# to easily get a McFadden's pseudo R2 for a fitted model in R, use the "pscl" package 
#  http://cran.r-project.org/web/packages/pscl/index.html

library(pscl)
pR2(m.LOGIT)

stargazer(m.OLS.KS, m.LOGIT,type="text")
AIC(m.OLS.KS)
exp(cbind(OR = coef(m.LOGIT), confint(m.LOGIT)))

# pseudo r-squared is higher, even though AIC is lower
# plus DV is binary => LOGIT is better to compare 



############################
# Let's talk about STEP models and why they are dangerous
# these can be used in any regression

m.OLS.KS <- lm(exit ~ ., data=dt)
model.step.OLS <- step(m.OLS.KS)

stargazer(m.OLS.KS,model.step.OLS,type="text")

# Half_day_leaves explain exits ?!  Why? How?
# careful about such spurious outputs.
# step function is very useful, but use with caution


m.logit.KS <- glm(exit ~ ., data=dt,family = "binomial")
model.step.Logit <- step(m.logit.KS)
stargazer(m.logit.KS,model.step.Logit,type="text")

# compare OLS and Logit

stargazer(model.step.OLS, model.step.Logit,type="text")

# again, half day leaves predicting exit?!

############################
