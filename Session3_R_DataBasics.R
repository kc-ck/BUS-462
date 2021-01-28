############################################################
# BUS 462 | Spring 2021 | Session 3
# Managing Data aka Data WRangling Basics
# CK 
# 28 JAN 2021 
### REFERENCES
# R Data import tutorial -  https://www.datacamp.com/community/tutorials/r-data-import-tutorial
# R Style GUIDE: http://adv-r.had.co.nz/Style.html

## attribution: adopted from  PhilChodrow @ MIT https://philchodrow.github.io/cos_2017/ 
############################################################


#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()

require(data.table)
#data  
#Download data from  https://philchodrow.github.io/cos_2017/1_terminal_and_git/taxi_data.csv 
# courtesy of of the New York City Taxi and Limousine Commission and PhilChodrow @ MIT

############################
## WORKING WITH CSV FILES ##
############################

# - Let's open the taxi data. It is found in
#   /my/path/to/cos_2017/1_terminal_and_git/taxi_data.csv
# - Let's find the path to that file on our own computer.
# -- (Mac) /.../OR-software-tools-2016/data/2013-05-14.csv
# -- (Windows) E:/.../OR-software-tools-2016/data/2013-05-14.csv
#              where "E" is whichever drive we mounted athena using
#              win-sshfs
# Load csv files using read.csv
# header = TRUE is usually ASSUMED, so not strictly necessary

# traditional
# dt <- read_csv(file="C:/Users/kalig/Downloads/taxi_data.csv", header = TRUE)

# data table alternate - fast and effecient!
taxi_data <- fread("C:/Users/kalig/Downloads/taxi_data.csv")

# Use names() to extract column names
names(taxi_data)

# Use str to look at details of columns
str(taxi_data)

# Use head() to look at the first several rows
head(taxi_data)

# Use the $ command to look at specific columns
taxi_data$vendor_id
taxi_data$rate_code

####################################################
## BASIC STATISTICS, PLOTTING, AND SUMMARY TABLES ##
####################################################

# Calculate the mean, standard deviation, and other statistics
mean(taxi_data$passenger_count)
sd(taxi_data$passenger_count)
summary(taxi_data$passenger_count)

# Plot fare amount vs trip distance
plot(taxi_data$trip_distance, taxi_data$fare_amount)

# Plot with a title, x- and y-axis labels
plot(taxi_data$trip_distance, taxi_data$fare_amount, main="Fare Amount vs. Trip Distance", xlab = "Trip Distance [mi]", ylab = "Fare Amount [$]")

# For other plots and information about the defaul graphics package
library(help = "graphics")

# We'll use ggplot2 soon enough - that is really really COOL!

# Create a table to summarize the data
# Here, we look at mean trip distance, based on the number of passengers in the taxi
tapply(taxi_data$trip_distance, taxi_data$passenger_count, mean)

# We can also create a table to look at counts 
table(taxi_data$passenger_count, taxi_data$payment_type)

# we can save these as R data objects!


## Try out a few other basic statistics and graphing functions

min(taxi_data$trip_distance)
median(taxi_data$trip_distance)
max(taxi_data$trip_distance)
summary(taxi_data$trip_distance)
hist(taxi_data$trip_distance)

sum(taxi_data$total_amount)

hist(taxi_data$total_amount)
boxplot(taxi_data$total_amount)


