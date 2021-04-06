############################################################
# BUS 462 | Spring 2021 | Assignment 3
# CK 
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
set.seed(42) # Set a seed to ensure repeatable random samples

# libraries
require(data.table)
require(pastecs)
require(stargazer)
require(PerformanceAnalytics)
library(pscl)

#LOAD DATA  
dt <- fread("C:/Users/kalig/Dropbox/SFU TEACHING/BUS 462/datasets and cases/chimeraCSV_full.csv")

# Data Wrangling
# Let's create 2 subsets - one for exits only and one for non-exits
dt.exit <- dt[dt$exit==1] # create a separate data table for exits only
dt.stay <- dt[dt$exit==0] # create a separate data table for non-exits only


### Q1 ########
# Remember that Odds or Odds Ratio is simply # of favourable events / # of unfavourable events
# Comapratively, probability is # of favourable events / # of total events  

  #Q1.1
  Odds_exit <-nrow(dt.exit)/nrow(dt.stay)
  # Ans: Odds / Odds Ratio for Exit:Stay is 2457/15675 or .1567

  #Q1.2
  Odds_avg <- 9/91 # 9% exit => 91% stay => 9/91 is the odds ratio
  # Ans: 9/91  or 0.098
  
  #Q1.3
  Odds_delta <- Odds_exit-Odds_avg
  # Chimera needs to bring their odds down by 0.0578 to meet industry average

### Q2 ########

# Let's subset our 4 variables of interest
dt1 <- dt[,c("exit", "boss_survey", "job_satisfaction","gender", "rank")]
  
## Q2.1 ######
  # We can use a combination of domain knowledge and correlation chart to determine possible interactions
  # From Assignment 2, we saw that boss survey and job satisfaction likely indicate exits
  # we also hypothesized that these 2 variables are interlinked and potentially interact
  
  # To consider other possible interactions, we can use a correlation chart
  chart.Correlation(dt1, histogram=TRUE, pch=19) # get's busy 

  #Notice that:
  # a) boss survey and job satistfaction are significantly correlated  (0.43),
  # b) as are gender and rank (0.44)
  # While job satisfaction and rank are also significantly correlated, the size if the effect is smaller (.23)
  # We needn't worry about Exits here as that is our DV (and we want to explore interaction within IVs)
  # realize that rank and gender are good potential controls to have or boss survey and job satisfaction
  
  # Realize that interaction a) make sense  from a domain perspective
    #- job satisfaction and boss survey are potentially cyclically related
  # similarly there is an underlying dynamic in the business world (unfortunately) in regards to b) 
    # - rank and gender aren't equally distributed
  table(dt1$rank,dt1$gender) # shows these differences - 
  # almost all employees in the top 2 ranks at Chimera (ranks 4 and 5) are men!
  # this potentially hints at a structural issue at Chimera, either related or unrelated to Exits (let's see what analysis says)
  
  
  # From a and b we can come up with the following interaction terms:
  dt1$boss_x_jobSatisfaction <- dt1$job_satisfaction * dt1$boss_survey
  dt1$gender_x_rank <-  dt1$gender * dt1$rank
  
  #Note: doing more interaction terms is okay, but i don't want to see all possible interactions 

## Q2.2 ######
  
  # summary stats for all variables (including the interaction variables we created)
  stargazer(dt1,type="text",summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))
  
  #Corr chart
  chart.Correlation(dt1, histogram=TRUE, pch=19) # get's busy, but still useful

## Q2.3 ######
  
  # OLS MODELS
  ModelA.OLS <- lm(exit ~ boss_survey + job_satisfaction, data=dt1)
  ModelB.OLS <- lm(exit ~ boss_survey + job_satisfaction + rank + gender, data=dt1)
  ModelC.OLS <- lm(exit ~ ., data=dt1)
  # Let's compare the summary results from these OLS regression models
  stargazer(ModelA.OLS,ModelB.OLS,ModelC.OLS,type="text")

  # LOGIT MODELS
  # Remember in LOGIT, our DV (Exit) is being treated as a binomial value of 0 or 1
  ModelA.Logit <- glm(exit ~ boss_survey + job_satisfaction, data=dt1,family = "binomial")
  ModelB.Logit <- glm(exit ~ boss_survey + job_satisfaction + rank + gender, data=dt1,family = "binomial")
  ModelC.Logit <- glm(exit ~ ., data=dt1,family = "binomial")
  # Let's compare the summary results from these regression models
  stargazer(ModelA.Logit,ModelB.Logit,ModelC.Logit,type="text")

#2.3.1  Best Performing Models
  #ANS From results of lines 94 and 102, Notice that Model C wins out in both OLS and LOGIT
  # also check that F-stats are significant and the model diagnostics check out
  
  # Look at Adj R-sq for OLS (.186 for C vs .16 for A and B) and AIC for LOGIT (11096 vs 11100+)
  # we also know from our model construction that Model C is pretty decent from a theoretical / domain knowledge perspective
  
  #BEST MODELS ARE:
  stargazer(ModelC.Logit,ModelC.OLS,type="text")

#2.3.2  #compare psuedo-r-sq and adj r-sq

  pR2(ModelC.Logit) 
  # ANS: McFadden's Psuedo R-sq for Logit Model C is 0.229
  summary(ModelC.OLS)$adj.r.squared #0.186
  #ANS: Adj R-sq for Model C OLS is 0.186 

#2.3.3 #compare Model C  OLS and LOGIT AIC
AIC(ModelC.OLS,ModelC.Logit)
 # OLS is actually better! even though LOGIT is superior coz of the binary DV treatment

## Q2.4 ######
  # Model C LOGIt is the best - Mainly because Exit is Binary DV + all underlying dynamics are accounted for
  # It's fine to drop gender x rank 
  summary(ModelC.Logit)
  
  # Interpretation of Boss survey on Exit:
  # we see from line 128 that the interaction of boss x job_satisfaction is significant
  # This implies simply that  the effect of boss_survey on exit DEPENDS on the value of job_satisfaction as well!
  
  # remember we our DV in LOGIT is log-odds of exit (the log of odds of exit)
  # remember LOGIT model interpretation - the logit formula we keep seeing
  
  # From that formula, if we consider the effect of boss survey (and ignore intercept), we get this relationship
    
  # ANS: log(Exit Odds) = -8.47 * boss_survey + 2.09*boss_survey*job_satisfaction
  # => log(Exit Odds) = boss_survey(-8.47 + 2.09*job_satisfaction)

  # it's okay if you considered intercept or any other significant interaction
  # -0.847 comes from the coefficient value for boss_survey => any increase in  value of boss survey REDUCES log(Exit Odds) by 8.47 (better boss surveys => less exits)
  # however, we also have that interaction term that considers boss survey value
  # 2.09 is the effect of this interaction term: for every increase in the interaction term value, log-odds of exit go up
  
  # main thing to realize here is: the interaction makes boss survey's relation to exit dependent on job satisfaction

## Q2.5 ######
  # the effect of boss_survey on exit DEPENDS on the value of job_satisfaction. 
  # Increase in job_satisfaction will make the magnitude of boss_survey, the size of the effect, smaller. 
  ####### Big Takeaway
  # The more satisfied I am with my job, the less having a good manager matters in my decision to exit.


  #  This is an open question but here are my top 3 recommendations
  #1/ Work to increase job satisfaction among employees
  #2/ For employees or groups of employees with low job satisfaction, make sure managers are trained
  #3/ Gender plays a significant role in exits - Model C Logit shows significant relationship
    # Along with our rank and gender table above, Chimera needs to address gender equity in their workplace

### Q3 ########

# let's create a new data table, where we now have half day leaves

dt2 <- dt[,c("half_day_leaves", "boss_survey", "job_satisfaction","gender", "rank")]

#corr chart to determine possible interactions
chart.Correlation(dt2, histogram=TRUE, pch=19) # we see job satisfaction has a strong relationship with half day leaves

# let's consider our usual interaction suspects
dt2$boss_x_jobSatisfaction <- dt2$job_satisfaction * dt2$boss_survey
dt2$gender_x_rank <-  dt2$gender * dt2$rank

# summary stats
stargazer(dt2,type="text",summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))


# let's build our regression model - we can start with OLS, maybe move to probit 
Model3 <- lm(half_day_leaves~ ., data=dt2)
stargazer(Model3,type="text")

# Indicative that job satisfaction, rank and gender*rank are the main culprits
# Adj-Rsq is pretty low - we need to go on to logit/probit models, or consider other variables

# so takeaway is  there is a difference in underlying dynamics
