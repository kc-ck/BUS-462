###########################################################
# BUS 462 | Spring 2021 | Session 7
# Titanic Data - CART Classification using TREES
# CK 
# 18 MAR 2021 
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
library(dplyr)

#ref: https://www.guru99.com/r-decision-trees.html 
#Step 1: Import Data
#path <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv"
#dt <- fread(path)  

titanic <-fread("C:/Users/kalig/Dropbox/SFU TEACHING/BUS 462/datasets and cases/titanic.csv")
  #ref:https://data.world/nrippner/titanic-disaster-dataset 
  head(titanic)
  tail(titanic)
  # 

#Step 2: Clean the Data
  stat.desc(titanic)  # There's a bunch of NAs
  
  # let's keep only those factors of interest
  dt <- titanic[,c("survived","sex","age","sibsp")]
  
  # summ stats
  stat.desc(dt)
  # notice NA and age and sex are chr
  dt$age <- as.integer(dt$age)
  dt$sex <- as.factor(dt$sex)
  dt$sex <- as.numeric(dt$sex)
  
  # omit NA's
  dt <- na.omit(dt)
  
  #summ stats
  stargazer(dt,type="text")
  
  # correlation plot
  chart.Correlation(dt) 
  
  # shuffle the data - randomize rows to prep for splitting into test and train parts
  shuffle_index <- sample(1:nrow(dt))
  dt <- dt[shuffle_index, ]
  head(dt)
  
#Step 3:Create train / Test Data
  
    # create test and train data
  # simply take the top 80% for train and bottim 20% for test
  n_cut <- round(nrow(dt)*.8,0)
  data_train <- dt[1:n_cut]
  data_test <- dt[(n_cut+1):nrow(dt)]
  
  #check dim
  dim(data_train)
  dim(data_test)
  
  # test distribution of survivors
  prop.table(table(data_train$survived))
  prop.table(table(data_test$survived))
  
# Step 4:  BUILD THE MODEL
  #install.packages("rpart.plot")	

  library(rpart)
  library(rpart.plot)
  fit <- rpart(survived~sex+age+sibsp, data = data_train, method = 'class') # Model for CART
  rpart.plot(fit, extra = 101) # tree plotting
  
#step 5 Prediction
  
  predict_unseen <-predict(fit, data_test, type = 'class')
  
  # Testing the passenger who didn't make it and those who did.
  # this is the confusion matrix!
  table_mat <- table(data_test$survived, predict_unseen)
  table_mat
  # Model correctly predicted 128 deaths but classified 21 survivors who actually died
  
#step 6: PErformance MEasure
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  print(paste('Accuracy for test', accuracy_Test))
  