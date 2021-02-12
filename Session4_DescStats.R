############################################################
# BUS 462 | Spring 2021 | Session 4
# DEscriptive Stats 
# CK 
# 04 FEB 2021 
### REFERENCES - PLEAS GO THROUGH
#(1) http://www.sthda.com/english/wiki/descriptive-statistics-and-graphics 
#(2) https://statsandr.com/blog/descriptive-statistics-in-r/

# MISC NOTES:
# There are four major types of descriptive statistics:
# Measures of Central Tendency. * Mean, Median, and Mode. ...
# Measures of Dispersion or Variation. * Range, Variance, Standard Deviation. ...
# Measures of Frequency: * Count, Percent, Frequency. ...
# Measures of Position. * Percentile Ranks, Quartile Ranks.
############################################################


#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()

#### DESC STATS FUNCTIONS IN R ###### 
# Mean 	mean()
# Standard deviation 	sd()
# Variance 	var()
# Minimum 	min()
# Maximum 	maximum()
# Median 	median()
# Range of values (minimum and maximum) 	range()
# Sample quantiles 	quantile()
# Generic function 	summary()
# Interquartile range 	IQR()
############################################ 

# let's use another embedded  dataset
?iris

dt <- iris # assign dataset to object dt


#1 Measure of central tendency: mean, median, mode #####
  mean(dt$Sepal.Length)

  ## NOTE!!!!
  #  mean() function will return NA if even only one value is missing in a vector.
  # avoid this by using  na.rm = TRUE, which tells to the function to remove any NAs before calculations
  # e.g.  LEt's introduce NA here.
  dt[151,] <- dt[150,] # enter a new row that simply copies over the previous row
  dt$Sepal.Length[151] <- "" # let's delete this element column in the last row to introduce NA
  # notice how R forces this into a character vector
  # let's force this to a numeric column
  dt$Sepal.Length <- as.numeric(dt$Sepal.Length)  
  dt$Sepal.Length[151] # check that the last line is NA
  
  # calculate mean
  mean(dt$Sepal.Length) # the NAs force mean function to fail
  # fix using na.rm
  mean(dt$Sepal.Length,na.rm = TRUE)
  # Yes, magic! i know right!
  
  # let's get back on track
  dt <- iris
  
  mean(dt$Sepal.Length)
  
# how to deal with outliers:
  # you can trim your mean by cutting a fraction of outliers
  # look up trim in mean"
  ?mean
  # trim - the fraction (0 to 0.5) of observations to be trimmed from each end of x before the mean is computed. 
  plot(dt$Sepal.Length)
  mean(dt$Sepal.Length, trim = 0.05) # trim 5% outliers
  
  median(dt$Sepal.Length)
  # you can also use quantile function to find value of x percentile of your data
  quantile(dt$Sepal.Length,.5)
  
  # mode isn't readily available. We'll get to it alter.
  
  
#2 Measure of variablity #####
  # 2,1 Range
  range(dt$Sepal.Length)
  
  # 2,2 IQR
  #The interquartile range (i.e., the difference between the first and third quartile)
  IQR(dt$Sepal.Length)
  # or you can calculate differences manually
  quantile(dt$Sepal.Length,.95) - quantile(dt$Sepal.Length,.05)
  
  # 2,3 Var and STD DEV
  var(dt$Sepal.Length)
  sd(dt$Sepal.Length)
  
  
  # Note on measures
  #  Range. It's not often used because it's very sensitive to outliers.
  # Interquartile range. It's pretty robust to outliers. It's used a lot in combination with the median.
  # Variance. It's completely uninterpretable because it doesn't use the same units as the data. It's almost never used except as a mathematical tool
  # Standard deviation. This is the square root of the variance. It's expressed in the same units as the data. The standard deviation is often used in the situation where the mean is the measure of central tendency.

  ########################s
  # VERY IMPORTANT NOTE:
  # standard deviation and the variance are different whether we compute it for a sample or a population 
  # see https://statsandr.com/blog/what-is-the-difference-between-population-and-sample/ 
  # In R, the standard deviation and the variance are computed as if the data represent a sample 
  # (so the denominator is n-1, where n is the number of observations).
  ########################s

#3  Summary functionality #####

  # Summary of a single variable : mean, median, 25th and 75th quartiles, min and max :
  summary(dt$Sepal.Length)
  
  # sumamry of a data frame or data table: applied to each column
  summary(dt)

  
#4 Misc. Short cut functions  #####
  # 4.1 sappply to to apply a particular function over a list or vector. 
  ## i.e. Use it, to compute a function for each column in a data frame, like mean, sd, var, min, quantile, â¦
  # row  output
  sapply(dt[, -5], mean)
  
  # 4.2 lapply does the same, but in column output
  lapply(dt[,-5], mean)
  
# 5 EASY PACKAGE(S) stat.desc function in pastecs package #############
  
#install.packages("pastecs") # install package
library(pastecs)    # call package
# reference: https://cran.r-project.org/web/packages/pastecs/pastecs.pdf 

stat.desc(dt)

# you can save this easily as a table or data frame
res <- stat.desc(dt[,-5]) # since col 5 is qualitative, we skip it 
res <- round(res, 2) # round digits to 2
print(res)

# this is easy to save -  either use write.csv or anything else - google /  help function ftw!
# write.csv(res,"C:/Users/kalig/Dropbox/lecture slides/CK's version/DataSets/res.csv")
# look up and figure out how to export as a excel sheet!

# Anyways...... back to res.

res

# this package tells you # of nulls, missing vbalues, and a bunch more, so pretty cool


# using dplyr. we can compute desc stats by group

# install.packages("dplyr")
library(dplyr)

# The function %>% is used to chain operation in dplyr
group_by(dt,Species) %>% summarise(count = n(),
                                   mean = mean(Sepal.Length, na.rm = TRUE), 
                                   sd = sd(Sepal.Length, na.rm = TRUE))
  


# 6 Some Data Wrangling #########

# creating Contingency table
# table() introduced above can also be used on two qualitative variables to create a contingency table. 
# The dataset iris has only one qualitative variable so we create a new qualitative variable 
# We create the variable size which corresponds to small if the length of the petal is smaller than the median of all flowers, big otherwise:
# this is very useful general trick

dt$size <- ifelse(dt$Sepal.Length < median(dt$Sepal.Length),"small", "big") # ifelse function

table(dt$size)

table(dt$size,dt$Species)
# alternate
xtabs(~dt$size+dt$Species)

# useful to see subgroups in your data

# relative frequencies (i.e., proportions) in each subgroup instead of frequencies (count)
prop.table(table(dt$size,dt$Species))


# some plotting
mosaicplot(table(dt$Species, dt$size),
           color = TRUE,
           xlab = "Species", # label for x-axis
           ylab = "Size" # label for y-axis
)

# box plots / distributions by group
boxplot(dt$Sepal.Length ~ dt$Species) # + dt$size)


#### LET's stop here

####################
### END OF CODE ####
####################