############################################################
# BUS 462 | Spring 2021 | Session 2
# First Steps - Loading and observing Data + some data Cleaning
# In session 2, we'll just load and observe the data,
# in session 3, we will proceed with making sense and cleaning it (into usable information)
# CK 
# 20 JAN 2021 

############################################################

# any line starting with a "#" is a comment - R does not consider that as runnable code.

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()

## Loading Libraries / Packages ####
# if you haven't instlled these packages, run
# install.packages("data.table","dplyr")
require(data.table)
require(dplyr)

## LOADING DATA ####
# for this example, we load data from the internet directly
url <- "https://www.dropbox.com/s/9bykqkzuw2i9qih/nashville_airbnb.csv?dl=1"

#dt is an R object called a data table 
## Other ways to read data (read_csv, etc...)
dt <- fread(url) # Data.table command to read in data
#  if you want to load from your own computer, command is
dt <- fread("C:/Users/kalig/Downloads/nashville_airbnb.csv") # remember - / and not \

## Looking at the data ####
head(dt) # First 5 entries
tail(dt) # LAst 5 entries
dim(dt) # Num of rows and column

# view entire data
View(dt)
# also see details in the global enviroment


### THIS IS WHAT what most raw/ scapped data looks like!


# Summarizing the data:
ls(dt) # lists all column names
objects(dt) # Same as above 

summary(dt) # produces summar stats for numerical / int data.


## It's too large -  let's go by individual colums:
# To call columns in data frames or data tables, format is <data.table.name>$<column name>
# column names should autofill

summary(dt$bathrooms)

summary(dt$bedrooms)

# check individual columns
class(dt$name)
attributes(dt$price)

# we can visualize numeric / factor columns
hist(dt$bedrooms)
plot(dt$bathrooms,dt$bedrooms)



## Let's get back to some basic structures and ideas with data wrangling in R with clean data before getting back to raw / messy data



##########################
##### COMMAND LISTS ######
##########################
##1/ EXPLORATION

# for some data table / data frame object called mydata, these are some commands and desc. 
summary(mydata)  # Provides basic descriptive statistics and frequencies. 
edit(mydata)     # Open data editor
str(mydata)      # Provides the structure of the dataset
names(mydata)    # Lists variables in the dataset

head(mydata)     # First 6 rows of dataset
head(mydata, n=10)# First 10 rows of dataset
head(mydata, n= -10)  # All rows but the last 10

tail(mydata)     # Last 6 rows
tail(mydata, n=10)    # Last 10 rows
tail(mydata, n= -10)  # All rows but the first 10
mydata[1:10, ]   # First 10 rows
mydata[1:10,1:3] # First 10 rows of data of the first 3 variables
