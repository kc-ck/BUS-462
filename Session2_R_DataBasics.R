############################################################
# BUS 462 | Spring 2021 | Session 2
# First Steps - Loading and observing Data + some data Cleaning
# CK 
# 20 JAN 2021 
############################################################

#### PREAMBLE ####
## DATA CLEANING

## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()

## Loading Libraries / Packages ####
require(data.table)
require(dplyr)

## LOADING DATA ####
  # for this example, we load data from the internet directly
  url <- "https://www.dropbox.com/s/9bykqkzuw2i9qih/nashville_airbnb.csv?dl=1"

#dt is an R object called a data tablehttps://www.dropbox.com/s/9bykqkzuw2i9qih/nashville_airbnb.csv?dl=1
  dt <- fread(url) # Data.table command to read in data -- it gets tr
  #  if you want to load from your own computer, command is
  ## dt <- fread("directory link")

## Looking at the data ####
head(dt) # First 5 entries
tail(dt) # LAst 5 entries
dim(dt) # Num of rows and column