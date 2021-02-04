############################################################
# BUS 462 | Spring 2021 | Session 4
# Anscombe's Quartet Class Example
# CK 
# 04 FEB 2021 
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

# Next week -  we see the effects of regressions / predictions if we don't account for different distributions

### TAKEAWAYS:
#1:  descriptive statistics is inadequate,
#2:  looking at a set of data graphically before starting to analyze 
# (next week) #3: type of relationships are important
















