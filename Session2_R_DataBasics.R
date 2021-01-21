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

#dt is an R object called a data tablehttps://www.dropbox.com/s/9bykqkzuw2i9qih/nashville_airbnb.csv?dl=1
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

## PLAY AROUND 

# look at individual columns
class(dt$name)

attributes(dt$price)
