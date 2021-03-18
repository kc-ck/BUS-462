############################################################
# BUS 462 | Spring 2021 | Session 7
# INTRO TO LOGIT REGRESSIONS (compared to OLS) 
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
  require(pastecs)
  library(stargazer)
  library(PerformanceAnalytics)
  
# PROB DEFINITION:
  # We want to understand (and predict) Admits to a grad program, based on GRE, GPA and prestige (Rank)
  # of undergrad school
  # The response variable, admit/don't admit, is a binary variable.
  
#LOAD DATA  ###
# Ref: https://stats.idre.ucla.edu/r/dae/logit-regression/

mydata <- fread("https://stats.idre.ucla.edu/stat/data/binary.csv")
  
#################################
#### PRELIMINARY ANALYSIS

  # summary stats
  stargazer(mydata,type="text",summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))
  stat.desc(mydata)

  # check if analyzable -- any missing data for admit's?
  table(mydata$admit,mydata$rank)
  xtabs(~admit + rank, data = mydata) # alternate way of seeing it / with col and row names
  
  #scatterplot and correlation matrix
  chart.Correlation(mydata, histogram=TRUE, pch=19) # get's busy
  stat.desc(mydata)
  
  # What do you think the model should be?
  # higher GRE, GPA and a higher ranked school should lead to higher p(admit)
  
#let's start with OLS and then bring up logit
  # OLS
  mOLS.KS <- lm(formula = admit~gre+gpa+rank,data=mydata)
  mOLS.1 <- lm(formula = admit~rank,data=mydata)
  mOLS.2 <- lm(formula = admit~gpa+rank,data=mydata)
  
  stargazer(mOLS.1,mOLS.2,mOLS.KS,type="text")
  # F Stat-s all look good and significant
  # rank seems to ahve a higher significance level
  # KS model is clearly leading, and fits with our hypothesis
  
  #model Diagnostic
  par(mfrow = c(2, 2)) # r outputs 4 graphs - this puts them in 1 plot
  plot(mOLS.KS, main = "Model KitchenSink")
  par(mfrow = c(1,1)) # setting normal output
  
  
  # BUT, how to interpret coeffecients here? since DV is binary?!
  # Hence LOGIT
  
###########################  
# LOGIT
  
  # first convert to factors
  mydata$admit <- as.factor(mydata$admit)
  mydata$rank <- as.factor(mydata$rank)
  
  # run logit - using generalized linear models
  mLOGIT <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
  summary(mLOGIT)
  
  # interpret:
  # remember Null us coefficients are 0
  
  # Null Deviance: Shows how well response is predicted with just an intercept
  # Residual Intercept : Shows how well the model is when predictors are included
  #   note: degrees of freedom = no. of observations - no. of predictors
  
  # Residual deviance decreases with reduced degrees of freedom => fit is okay
  
  # Remember:
  # If your Null Deviance is really small, it means that the Null Model explains the data pretty well. 
  # Likewise with your Residual Deviance - smaller is better

  ##################  
#->  # rule of thumb:: Residual deviance / ( deg of freedom) >>1 is okay
  ###############
  
  #now compare to OLS
 stargazer(mOLS.KS,mLOGIT,type="text",header = FALSE)
  
  # What difference do you notice?
  
  #--> we now have coeffecient for each factor of rank!
  # --> This allows for more powerful prediction
  
  # 
  plot(mLOGIT)
  
  # compare AIC
  AIC(mOLS.KS)
  AIC(mLOGIT)
  
  
  ## INTERPRETATION!
  stargazer(mLOGIT,type="text")
  ## 
  
  #For every one unit change in gre, the log odds of admission (versus non-admission) increases by 0.002.
  #For a one unit increase in gpa, the log odds of being admitted to graduate school increases by 0.804.
  # The indicator variables for rank have a slightly different interpretation. 
  # For example, having attended an undergraduate institution with rank of 2, 
  # versus an institution with a rank of 1, changes the log odds of admission by -0.675.
  
  # looking at the odds ratio makes for easirt interpretation
  ## odds ratios and 95% CI
  exp(cbind(OR = coef(mLOGIT), confint(mLOGIT)))

  # Interpretation:
  # For one unit increase in gpa, the odds of being admitted to graduate school increase by a factor of 2.23 (prob of ~.69). 
  # refer: https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
  # OR for intercept doesn't mean anything
  
################################################  
# Sensitivity Analysis using PREDICTION MODEL - to predict probabilities (and understand the model)
#  a bit out of scope, but useful to know.
#################################################

#1:   predicted probability of admission at each value of rank, holding gre and gpa constant at their means. 
  
  # create a new data table with IV values are means of our data
  # Check out the ?with description
    newdata1 <- with(mydata, data.table(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
    newdata1
  
  # predict probability for each row using n 
    newdata1$Pred.Prob <- predict(mLOGIT, newdata = newdata1, type = "response")
    newdata1

  # let's plot the predicted probability against undergrad institution prestige (rank)
    plot(newdata1$rank,newdata1$Pred.Prob,xlab="Prestige of Undergrad Institution", ylab="Prob(Admit)", main="GRE and GPA constant")
    
  #Interpretation
  # -> predicted prob of being accepted into a graduate program is 0.52 for students from rank 1 (highest prestige undergraduate institutions)
  # ->  0.18 for students from the lowest ranked institutions (rank=4), holding gre and gpa constant at their means
    
  
#2:  predicted probability of admission  for different gre a while holding gpa and rank constant
    
  newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),4), 
                                      gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
  head(newdata2) 
  tail(newdata2)
    
  # predictions for each row (Set of values)
  # slight addition to before: we want Std  Errors since this is not a categorical variables
  # we calculate the  quantile and then the prob of that quantile from the prob. density function
  
  #install.packages("aod")
  library(aod)
  
  newdata3 <- cbind(newdata2, predict(mLOGIT, newdata = newdata2, type = "link",se = TRUE))
  
    newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  # we also want to transform predicted values and 
  
 head(newdata3)
  
 # Sensitivity Table of predictions across GRE scores, by rank
 library(ggplot2)
 ggplot(newdata3, aes(x = gre, y = PredictedProb)) + 
   geom_line(aes(colour = rank),size = 1)
 
 # let's add the Confidence Intervals
 ggplot(newdata3, aes(x = gre, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) + 
   geom_line(aes(colour = rank),size = 1)
 