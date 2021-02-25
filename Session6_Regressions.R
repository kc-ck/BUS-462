############################################################
# BUS 462 | Spring 2021 | Session 6
# OLS  REGRESSIONS on Chimera Data
# CK 
# 24 FEB 2021 
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
set.seed(42) # Set a seed to ensure repeatable random samples

# libraries
require(data.table)
require(stargazer)
require(PerformanceAnalytics)
require(Hmisc)
require(corrr)
require(dplyr)
#LOAD DATA  ###
dt <- fread("C:/Users/kalig/Dropbox/SFU TEACHING/BUS 462/datasets and cases/chimeraCSV_full.csv")

str(dt) # check the structure
head(dt)

## Plot correlation matrix - Pairwise correlation
#https://www.datanovia.com/en/blog/easy-correlation-matrix-analysis-in-r-using-corrr-package/
tab1 <-rcorr(as.matrix(dt))
round(tab1$r,2)
View(tab1$r) # look at the correlations
View(tab1$P)

# this is too busy. Can we do something? Why, yes
library(corrr)
res.cor <- correlate(dt)
res.cor

# Let's focus on job satisfaction
tab2 <- res.cor %>%   focus(job_satisfaction)
View(tab2)

# If you want to round it to 2 digits:
#tab2 <-as.data.table(tab2)
#tab2$job_satisfaction  <- round(tab2$job_satisfaction,2)
#tab2

# look at your variable of interest -- job_satisfaction Anything jump out?
# check p value
# View(tab1$P)

#################################################
## Multiple Regressions on CHIMERA DATA ###########
#################################################7

# Let's model job_satisfaction

# Kitchen Sink Model
Model.KS <- lm(job_satisfaction ~ .  , data=dt)
summary(Model1)
stargazer(Model.KS,type="text",column.sep.width = "1pt",omit.stat=c("f"))

# CAN YOU IMPROVE THIS MODEL ?- 
# USE THEORY and Correlation Matrix to guide you. 
# top 4 correlations to job_satisfaction

# remember confounds and controls.
# do you want to add anything else?

# team_size
# years_since_promotion
# kpi_performance
# salary 
#

