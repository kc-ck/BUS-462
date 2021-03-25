# BUS 462 | Spring 2021 | Assignment 2
# CK 
# 25 Mar 2021
# AB Testing example
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
set.seed(42) # Set a seed to ensure repeatable random samples

# libraries
require(data.table)
library(tidyverse)

#LOAD DATA  
click_data <- fread("C:/Users/kalig/Downloads/click_data.csv")


# Preliminary Analysis #########
  range(click_data$visit_date) # years worth of baseline data

  # Calculating Baseline Rates
  
  # overall conversion rate
  click_data %>%   summarize(conversion_rate = mean(clicked_adopt_today))
  ## This shows that roughly 27.7 % of users clicked on adopt today over 2017
  
  # Account for seasonality
  click_data %>% group_by(month(visit_date)) %>% summarize(conversion_rate = mean(clicked_adopt_today))
  ## This clearly shows some seasonal variation
  
  # we can also check for trends by day of week and  week #
  
  # Calculate the mean conversion rate by day of the week
  click_data %>% 
    group_by(wday(visit_date)) %>% 
    summarize(conversion_rate = mean(clicked_adopt_today))
  
  # Calculate the mean conversion rate by week of the year
  click_data %>%
    group_by(week(visit_date)) %>%
    summarize(conversion_rate = mean(clicked_adopt_today))
  
  
  
  # Let's plot this conversion rate showing seasonality
  
  # Compute conversion rate by week of the year
  click_data_sum <- click_data %>%
    group_by(month(visit_date)) %>%
    summarize(conversion_rate = mean(clicked_adopt_today))
  
  # Build plot showing Monthly seasonality
  ggplot(click_data_sum, 
         aes(x = `month(visit_date)`,y = conversion_rate*100)) +
    geom_point() +  geom_line() +  scale_y_continuous(limits = c(0, 100))
  ## Summer and Winter Peak
  
  # weekly trent
  click_data_sum_weekly <- click_data %>%
    group_by(week(visit_date)) %>%
    summarize(conversion_rate = mean(clicked_adopt_today))
  
  ggplot(click_data_sum_weekly, 
         aes(x = `week(visit_date)`,y = conversion_rate*100)) +
    geom_point() +  geom_line() +  scale_y_continuous(limits = c(0, 100))
  
  
# Calculating Power and # of experimental observations required #########
  #install.packages("powerMediation")
  #installed.packages("broom")
  library(powerMediation)
  library(broom)
  total_Sample_size <- SSizeLogisticBin(p1= 0.54 ,# conversion rate in aug
                                      p2= 0.64, # you want to detect a 10% increase
                                      B= 0.5, # proportion of sample for test
                                      alpha= .05,
                                      power= .8)
## This means you need 758 experimental observations to detect a 10% increase in experiment sample
  
    # Now calculate for August, but 5% increase (p2)
    total_Sample_size <- SSizeLogisticBin(p1= 0.54 ,# conversion rate in aug
                                        p2= 0.59, # you want to detect a 10% increase
                                        B= 0.5, # proportion of sample for test
                                        alpha= .05,
                                        power= .8)
## This means you need 3085 experimental observations to detect a 10% increase in experiment sample  

    # Experiment Analysis #########
  
  exp_data <- fread("C:/Users/kalig/Downloads/experiment_data.csv")
  
  min(exp_data$visit_date)
  max(exp_data$visit_date)
  # run for JAn - control and test
  
  # Results peek 
  exp_data %>%
    group_by(condition) %>%
    summarize(conversion_rate = mean(clicked_adopt_today))


  # Group and summarize data
  experiment_data_sum <- exp_data %>%
    group_by(visit_date, condition) %>%
    summarize(conversion_rate = mean(clicked_adopt_today))
  
  # Make plot of conversion rates over time
    ggplot(experiment_data_sum,
           aes(x = visit_date,
               y = conversion_rate,
               color = condition,
               group = condition)) +
      geom_point(size=2) +
      geom_line()

  # Box Plots are awesome too! but don't show temporal trends unless you play around
    ggplot(experiment_data_sum,
         aes(y = conversion_rate,
             color = condition,
             group = condition)) +
    geom_boxplot()
  
  # histograms arem't useful in this case
    experiment_data_sum <- as.data.table(experiment_data_sum)

    ggplot(experiment_data_sum, aes(x=conversion_rate, fill=condition)) +
      geom_histogram(bins = 100, position="identity")+
      stat_density(geom = "line", aes(colour = condition))
      
  # STatistical testing!
  
  dt.control <- experiment_data_sum[condition=="control"] 
  dt.exp <- experiment_data_sum[condition=="test"] 
  
  # T-test
  t.test(conversion_rate~condition,data=experiment_data_sum, paired=TRUE)
  
  # H0: mean of 2 samples is equal
  # H1: difference of means of 2 samples is not 0
  # remember remember: p-value is the prob. of wrongly rejectying null and is always compared to signif. level
  # small p-values < significance level => null is unlikely to be true => reject null
  
  # paired tests are used to compare before-after treatment conditions
  # look at the p-value! almost 0
  # => reject null! => H1 is true 

  
  
# is there a better way? given conversion rates are calculated and not observed directly?

  # YES -  Run a logit regression!
  # in this case, you are running a logistic regression with specification shown
  
  library(stargazer)
  experiment_results <- glm(clicked_adopt_today ~ condition,
                            family = "binomial",
                            data = exp_data)
  stargazer(experiment_results, type="text") 
  
# look at p-value, and direction of the coefficient => condition test did improve log-odds of clicking the button
  
# another way to see results in console
  experiment_results <- glm(clicked_adopt_today ~ condition,
                            family = "binomial",
                            data = exp_data) %>% 
    tidy
  
  experiment_results
  
  
  # p-values are almost 0. You can reject the null => cat with hat is indeed better!
  
   