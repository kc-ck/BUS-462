############################################################
# BUS 462 | Spring 2021 | Session 5
# OLS REGRESSIONS - Single and Multiple
# OLS Regression Diagnostics
# CK 
# 11 FEB 2021 
### REFERENCES
# TBD
############################################################


#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
set.seed(42) # Set a seed to ensure repeatable random samples

# libraries
require(data.table)
require(pastecs)


#################################################
## SIMPLE SINGLE LINEAR MODEL
#################################################

## http://r-statistics.co/Linear-Regression.html 

?cars

# we start with some basic analysis and graphical analysis

stat.desc(cars) # summary desc stats

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot
# =>  suggestive of linear relationship

# check for outliers
  par(mfrow=c(1, 2))  # divide graph area in 2 columns
  boxplot(cars$speed, main="Speed")  # box plot for 'speed'
  boxplot(cars$dist, main="Distance")  # box plot for 'distance'
  #Generally, any datapoint that lies outside the 1.5 * interquartile-range  is considered an outlier, 
  #where, IQR is calculated as the distance between the 25th percentile and 75th percentile values 
  # => one outlier in distance

# check for normality 
  par(mfrow=c(1, 2))  # divide graph area in 2 columns
  # density plot for 'speed'
  hist(cars$speed, freq = FALSE,breaks = 10, col='red') + lines(density(cars$speed))
  # density plot for 'dist'
  hist(cars$dist, freq = FALSE,breaks = 10, col = "blue") + lines(density(cars$dist))
  #if not normal - we need to transform
  #TODO: we'll get to transformation later

#Correlation
  cor(cars$dist,cars$speed)

# linear model
Model1 <- lm(dist~speed,data=cars,) # we trying to figure out stopping distance y as function of x
summary(Model1)
# => dist = −17.579 + 3.932∗speed

# see p value:
# remember in OLS:  Null Hypothesis is that the coefficients associated with the variables is equal to zero
# alt h1:  coefficients are not equal to zero => statistically significant relationship between the y and x
# small p value => reject null and accept h1

# t value:
# heuristic: larger t value => less likely coeffecient \beta neq 0 by chance
# Pr(>|t|) or p-value is the probability that you get a t-value as high or higher than the observed value 
# when the Null Hypothesis (the \beta coefficient is equal to zero or that there is no relationship) is true.
# So if the Pr(>|t|) is low, the coefficients are significant (significantly different from zero). 
# If the Pr(>|t|) is high, the coefficients are not significant.

# if you want a different significance level, just see the p value and signif codes. 

# NOTE on AIC AND BIC
# The Akaike’s information criterion - AIC (Akaike, 1974) and the Bayesian information criterion - BIC (Schwarz, 1978) 
#measures of the goodness of fit of an estimated statistical model and can also be used for model selection. 
# Both criteria depend on the maximized value of the likelihood function L for the estimated model.
AIC(Model1)
BIC(Model1)


# MODEL DIAGNOSTICS:

par(mfrow = c(2, 2)) # r outputs 4 graphs - this puts them in 1 plot
plot(Model1, main = "Model 1")
# recall 
par(mfrow = c(1, 1)) # remember to change it back!
# see what happens without

#################################################
## Multiple Regressions on CHIMERA DATA ###########
#################################################7

#LOAD DATA  
dt <- fread("C:/Users/kalig/Dropbox/SFU TEACHING/BUS 462/datasets and cases/chimeraCSV_full.csv")

# Let's model Boss

Model1 <- lm(job_satisfaction ~ .  , data=dt)
summary(Model1)

Model2 <- lm(exit ~ ., data=dt)

summary(Model2)
names(Model1)


