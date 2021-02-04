############################################################
# BUS 462 | Spring 2021 | Session 4
# Inferential Stats: T-tests and other commonly used statistical tests
# CK 
# 04 FEB 2021 
### REFERENCES
#(1) http://r-statistics.co/Statistical-Tests-in-R.html  
#(2) probability plots: https://www.statmethods.net/advgraphs/probability.html 
#(3) https://suinotes.wordpress.com/2009/11/30/understanding-t-test-in-r/
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
set.seed(42) # Set a seed to ensure repeatable random samples

## REMEMBER 
# remember remember: p-value is the prob. of wrongly rejecting null and is always compared to signif. level
#  small p-values < significance level => null is unlikely to be true => reject null
# large p values > significance level => failure to reject null => null hyp likely true
# commonly used significance levels: 0.005, 0.01, 0.05, 0.1


#1: One Sample t-Test to test mean of a sample #####
# Used to test if mean of a sample from a normal distribution could reasonably be a specific value.

  x <- rnorm(50,mean=10,sd=0.5) # create a random normal sample of mean and sd
  # check distribution
  hist(x) # see histogram
  
  # test if mean of x could be some value mu: i.e.
  # H0: true mean of x is mu
  # H1: true mean of x is not Mu
  t.test(x,mu=10)
  # How to interpret:
    # Note the significance level is 0.05 (1-.95 from the CI) 
    # Look at the p value -> 0.08275! This is greater than the significance level 
      # => Null cannot be rejected (failure to reject) i.e likely to be true
    # look at the 95% CI - 10 falls within this range
    # THUS, it is okay to say mean of random variable  x is 10 within the 0.05 significance level
    # caveat here is we can say this since x is normally distributed (we made it like that!)
  
  # Let's play around
  # use another mu
  t.test(x,mu=10.2)
  # Look at the p - value. It is smaller than the significance level of 0.05. Therefore, you should reject null
  
  # Use conf.level argument to adjust the confidence level.
  t.test(x,mu=12,conf.level = .9)
  
  
#2: Two-sample t-Test to compare 2 means  #####
  # testing if 2 samples are coming from the same (assumed) normal distribution
  # H0: mean of 2 samples is equal
  # H1: difference of means of 2 samples is not 0
  
  # remember remember: p-value is the prob. of wrongly rejectying null and is always compared to signif. level
  # small p-values < significance level => null is unlikely to be true => reject null
  # let's use the sleep data
  ?sleep
  
  # let's test 2 groups of 10 samples of sleep data measuring # of hrs of extra sleep 
  plot(extra~group,data=sleep) # bar plot showing values of samples of sleep hrs across 2 groups
  
  # we want to test if there is a statistically significant overlap in means, i.e. are the means equal?
  # can we say group 2 is sleeping more than group 1?!
  
  # let's do a t.test!
  
  t.test(extra~group,data=sleep)
  
  # remember, we are testing the difference between means of group 1 and 2
  # look at the p-value: 0.07939, > 0.05 significance level
  # => Ho cannot be rejected , i.e. Null likely to be true!
  
  # Look at the 95% CI of the differences between means: -3.36 to 0.2
  # that is a wide range. Look at the means of group 1 and 2, and calculate the difference
  # 2.33-0.75 = 1.58 which falls within the CI interval.
  
  # Thus, assuming normal distribution of the 2 samples, we can't conclude that the means are different!
  # for 2 groups to have differencces, the delta of means must be greater
  
  # this is WIDELY used from druig trials, to AB testing!
  
  # Extra note: Here we use welch's t-test for 2 samples with unequal variances.
  # look up other types of t-tests and play around!
  
  # include paired=TRUE and see what happens?
  t.test(extra~group,data=sleep, paired=TRUE)
  # paired tests are used to compare before-after treatment conditions
  # look at the p-value! 0.0028
  # => reject null! => H1 is true 
  
  
#3: Wilcox Signed Rank Test for non-normal sample assumption  #####
  # alternative to t-test when you don't want to assume normal distribution
  # non-parametric method used to test if an estimate is different from its true value.
  
  x <- c(0.80, 0.83, 1.89, 1.04, 1.45)
  y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
  # is the mean of this data == 20?
  wilcox.test(x,y, paired=TRUE)
  
#4: Shapiro Test to check for normality  #####
  ## test if a sample is normally distributed!
  # Null H0: sample is nnormally distributed
  # Alt H1: sample is NOT normally distributed
  set.seed(42)
  test <- rnorm(100, mean=5, sd=1) # generate a normal distribution
  shapiro.test(test)  # the shapiro test.
  # look at the p-value ===> > significance => failure to reject null
  
  # test the sleep data
  shapiro.test(sleep$extra)
  shapiro.test(sleep$extra[sleep$group==1]) # this subsets group 1 of sleep data
  shapiro.test(sleep$extra[sleep$group==2])# this subsets group 2 of sleep data
  

#5:  Kolmogorov And Smirnov Test to check if 2 samples have (are drawn from) the same distribution #####
  
  # let's generate 2 samples
  x <- rnorm(50) # random normal distr
  y <- runif(50) # random distr
  hist(x)
  hist(y)
  
  # H0:x and y are drawn from same distribution
  # H1:x and y are drawn from different distribution
  ks.test(x,y)
  # p value is < significance level =>  reject null
  
  # let's play around
  x <- rnorm(50)
  y <- rnorm(30)
  ks.test(x,y)

      
#6:  Fisher's F-Test to check if 2 samples have the same variance #####
  # let's generate 2 samples again
  x <- rnorm(50) # random normal distr
  y <- runif(50) # random distr
  hist(x)
  hist(y)

  var.test(x,y)
  #H0: same variance, ie. ratio of variance ==1 
  #H1: Different variance ie. ratio of variance neq 1 
  # we see p < \alpha => reject null
  
  # let's play around with 2 random normal distr.
  x <- rnorm(100, mean=5.5, sd=1) # generate a normal distribution
  y <- rnorm(100, mean=5.25, sd=1) # generate a normal distribution
  hist(x)
  hist(y)
  var.test(x,y)
  # we see p > \alpha => null is likely true
  # nort surprising given we drew from same std. devs even though means are different
  
#7:  Correlation Significance Test (Pearsons) #####
  ?cars # remember this?
  cor(cars$speed,cars$dist) # we used this last time
  
  # we can test dependence using pearson's / spearman / kendall test -- look this up using ?cor.test
  # Here H0: Correlation is 0, i.e. not statistically related
  # H1: they are statistically related
  cor.test(cars$speed,cars$dist)
  # p value is < significance level =>  reject null 
  
####################
### END OF CODE ####
####################