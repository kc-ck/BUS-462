############################################################
# BUS 462 | Spring 2021 | Session 5
# Anscombe's Quartet Class Example
# CK 
# 11 FEB 2021 
# session 5 starts line 53
### REFERENCES
# https://en.wikipedia.org/wiki/Anscombe's_quartet
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()
require(data.table)

#### LOAD DATA
## ,colClasses=c("numeric") forces fread to read the data in as numeric
# You'll run into trouble otherwise
aq1 <- fread("C:/Users/kalig/Dropbox/lecture slides/CK's version/DataSets/AQ_1.csv",colClasses=c("numeric"))
aq2 <- fread("C:/Users/kalig/Dropbox/lecture slides/CK's version/DataSets/AQ_2.csv",colClasses=c("numeric"))
aq3 <- fread("C:/Users/kalig/Dropbox/lecture slides/CK's version/DataSets/AQ_3.csv",colClasses=c("numeric"))
aq4 <- fread("C:/Users/kalig/Dropbox/lecture slides/CK's version/DataSets/AQ_4.csv",colClasses=c("numeric"))

# we perform repeated calculations - let's do a function
fn1 <- function(dt){
  x_mean <- mean(dt$x)
  y_mean <- mean(dt$y)
  x_var <- var(dt$x)
  y_var <- var(dt$y)
  corr_xy<-cor(dt$x,dt$y)
  ans <- cbind(x_mean,x_var,y_mean,y_var,corr_xy)
  return(ans)
}

fn1(aq1)
fn1(aq2)
fn1(aq3)
fn1(aq4)
    
# scatter plots
par(mfrow=c(2,2)) #  function to plot multiple plots on one page 
plot(aq1)
plot(aq2)
plot(aq3)
plot(aq4)

### TAKEAWAYS:
#1:  descriptive statistics is inadequate,
#2:  looking at a set of data graphically before starting to analyze 


#################################################
## Regressing AQ DAta - SEssion 5 ###########
#################################################7
# refer: https://rstudio-pubs-static.s3.amazonaws.com/52381_36ec82827e4b476fb968d9143aec7c4f.html 


# let's build 4 models
Model1 <- lm(aq1$y~aq1$x) # aq 1
Model2 <- lm(aq2$y~aq2$x) # aq 2
Model3 <- lm(aq3$y~aq3$x) # aq3
Model4 <- lm(aq4$y~aq4$x) #aq 4

# sumamaries of Model
summary(Model1)
summary(Model2)
summary(Model3)
summary(Model4)

# see RSE, RSquared, Adj R., and F-stat!
# everything looks the same! WTF!

# Notice residuals are different.

# let's do diagnostics

#1. first look at scatter plots!
  library(ggplot2) # let's use ggplot!
  # ggplot is awesome!
  
  # ggplot does not work with par(mfrow=c(2,2))
  ggplot(data=aq1,aes(x=x,y=y)) + geom_point() + geom_smooth(formula = y~x, method=lm)
  ggplot(data=aq2,aes(x=x,y=y)) + geom_point() + geom_smooth(formula = y~x, method=lm)
  ggplot(data=aq3,aes(x=x,y=y)) + geom_point() + geom_smooth(formula = y~x, method=lm)
  ggplot(data=aq4,aes(x=x,y=y)) + geom_point() + geom_smooth(formula = y~x, method=lm)
  # Outliers are an issue
  
#2.  Let's do model diagnostics and see residuals
  par(mfrow = c(2, 2))
  plot(Model1, main = "Model 1")
  plot(Model2, main = "Model 2")
  plot(Model3, main = "Model 3")
  plot(Model4, main = "Model 4")
  
  # look at residuals vs. Fitted and normal q-q plots!
  
  # what is the best model?!
  
  # dataset 1!
#See  https://towardsdatascience.com/importance-of-data-visualization-anscombes-quartet-way-a325148b9fd2 
  
  
