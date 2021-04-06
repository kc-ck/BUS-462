############################################################
# BUS 462 | Spring 2021 | Assignment 2 SOLUTION
# CK 
# 06 APR  2021 
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
  cat("\014")  # Clear Console
  rm(list = ls(all.names = TRUE))# clear all
  gc()
  set.seed(42) # Set a seed to ensure repeatable random samples

  # libraries
  require(data.table)
  require(pastecs)
  require(stargazer)
  
  #LOAD DATA  
  dt <- fread("C:/Users/kalig/Dropbox/SFU TEACHING/BUS 462/datasets and cases/chimeraCSV_full.csv")
  
#General Notes: 
#1. Naming your data objects with dt or df is much easier than chimeraCSV_full or longer names
#2. Once you are familiar with R, the next step is efficient code:
    # Only creating objects you need
    # Use of functions
    # Reduced lines of code
    # efficient commenting
  # Efficient coding is something you will need to continuously improve on
  
############# 
#Q1##########
  
  # Ideally, you would do the following steps
  
  #1. See the data
  head(dt)
  tail(dt)
  View(dt)
  
  #2. Check the structure of the data - are all columns properly coded (numeric, categorical,...)
  str(dt)  #to show information about the structure of the data table
  
  #3. Check for NAs and missing variables
  colSums(is.na(dt)) # check if any columns have missing data
  dt <- na.omit(dt) # omits rows with NAs
  
# All these steps will tell you the data is analyzable
  
############# 
#Q2##########
  #2A
  nrow(dt[dt$gender==1])/nrow(dt[dt$gender==0]) # of rows with gender M / # of rows with gender F
  # Alt:
  table(dt$gender) #and then calculate ratio of men:women manually
  11400/6732 # ANS 1.69 Males to Females
  
  #2B
  range(dt$age)
  #ANS 19-56
  
  #2C
  #easiest way to do this is by using breaks in the data
  hist(dt$age,breaks=5) 
  # alternate - define break points in histogram
  hist(dt$age, xlim =c(1,60), breaks = c(10,20,30,40,50,60), main ="Decadal Age Distribution of Employees", xlab = "Decadal Range")
  
  #2D
  prop.table(table(dt$local)) # Recall from data desc 0 => local and 1 =. foriegn 
  # Ans ~39.9% of employees are foreign 
  # Note i didn't ask ratio here
  
  #2E
  length(unique(dt$city_size)) #shows the number of unique data values in city_size column
  #Assumption: the same city size means the same city
  
  #2F
  # Either plot tables or histograms for variables of interest
  # Some examples below - here is where you use domain knowledge to explore the data
  table(dt$education)
  table(dt$part_time)
  
  # You can also use chart correlations to identity variables of interest under the employee demographic tree (as opposed to Org Climate tree )

#############
#Q3##########
  # here is where efficiency comes in!
  
  #dt describes all employees at the beginning of the year
  # create dt.e to describe employees who exited
  dt.e <- dt[dt$exit==1]
  # create dt.s to describe employees who stayed
  dt.s <- dt[dt$exit==0]
  
  # we have 3 objects: dt -> all employees; dt.e -> employees who exited and dt.s -> employees who stayed
  
  #  Now  let's subset variables of interest - in this case demographic profiles for each of these 3 data objects
  dt.dp <- dt[,c("gender","age","local","city_size","education","part_time")]
  dt.e.dp <- dt.e[,c("gender","age","local","city_size","education","part_time")]
  dt.s.dp <- dt.s[,c("gender","age","local","city_size","education","part_time")]
  # Now we can use EITHER summary function, or stat.desc from pastecs package, or stargazer package
  # Whatever you use, pay attention to means, std dev, Median in particular as you will use these to compare 
  
  #3A
  summary(dt.dp) # base package
  # Other packages/functions to use
   # round(stat.desc(dt.dp),2) # stat.desc from pastecs
    # stargazer(dt.dp,type="text") # summ stats from stargazer
  
  #3B
  # use the same package/function you use in #3A. I only show the base package here
  summary(dt.e.dp)   # you now see Summary Descriptive Statistics for all employees who left
  
  #3C
  # use the same package/function you use in #3A. I only show the base package here
  summary(dt.s.dp)  # you now see Summary Descriptive Statistics for all employees who stayed
  
  #3D
  # you are comparing these together:
  summary(dt.dp)
  summary(dt.e.dp)
  summary(dt.d.dp)
  # Note that the base package doesn't have SD / Variance.
  
  # Hence, I prefer stargazer
  stargazer(dt.dp,dt.e.dp,dt.s.dp,type="text")
  
# What i wanted you to Note:
# Comparing Summary Stats indicates that demographic data between employees who exited and stayed show NO major differences
# if you highlighted some small differences, that is okay.  
  
#############
#Q4##########
# Note that "distribution" when used alone could be a histogram, box plot, summary stats table, anything that points to the moments
# additional note: "probability distribution" is a lot more specific - that implies if it is normal, or uniform, or....
  
  #4A
  hist(dt$job_satisfaction) # shows the distribution/histogram of values
  boxplot(dt$job_satisfaction) # shows outliers and IQR (75th percentile - 25th percentile)
  
  #4B
  hist(dt$kpi_performance) # shows the distribution/histogram of values
  boxplot(dt$kpi_performance) # shows outliers and IQR (75th percentile - 25th percentile)
  
  #4C
  hist(dt$boss_survey) # shows the distribution/histogram of values
  boxplot(dt$boss_survey) # shows outliers and IQR (75th percentile - 25th percentile)
  
  #4D
  # like in Q2, inclued any variables you feel relevant from domain knowledge. E.g., 
  hist(dt$years_since_promotion)
  hist(dt$salary)
  
# Note that these indicate data seems to be normalized within company metrics, so it's comparable
  
#############
#Q5##########
  
  # Remember we already created subsets of dt.e (exits) and dt.s (Stays) from dt(all employees)
  
  # like in Q3, let's extract the Employmen Climate variables of interest from dt.e dt.s and dt
  dt.c <- dt[,c("job_satisfaction","kpi_performance","boss_survey","years_since_promotion","salary")]
  dt.e.c <- dt.e[,c("job_satisfaction","kpi_performance","boss_survey","years_since_promotion","salary")]
  dt.s.c <- dt.s[,c("job_satisfaction","kpi_performance","boss_survey","years_since_promotion","salary")]
  
  # Like in Q3, we can use EITHER summary function, or stat.desc from pastecs package, or stargazer package
  # Whatever you use, pay attention to means, std dev, Median in particular as you will use these to compare 
  # I will use stargazer in this example
  
  #5A
  stargazer(dt.c,type="text") # shows summary statistics of climate variables for all employees
  
  #5B
  stargazer(dt.e.c,type="text") # shows summary statistics of climate variables for all employees
  
  #5C
  stargazer(dt.s.c,type="text") # shows summary statistics of climate variables for all employees
  
  #5D
  # Comparing the 3 tables, I wanted you to notice these points:
  # A. There is a big difference in boss_survey between those who left and who stayed - look at the means(sd) -- .299(.174) for exits vs .532(.185)
  # B. There is a small difference in job satisfaction between the groups - mean(s.d) of .498(0.196) for exits vs .616 (.196) for remain
  # C. There is a small difference in KPI performance between groups -. mean(s.d) of 0.677(0.097) for exits vs 0.704 (.1) for remain
#NOTE: If the differences in means is greater than 1 or 2 Standard deviations, it is likely significant
  # Realize this makes intuitive sense - a bad manager could lead to lower satisfaction 
  # These 2 could potentially explain exit - something to check using regressions!
  # There are potential interactions between these 3, i.e., one could lead to the other. Something to check
  
  # Extra points if you compared salary, team sizes, etc... - 
  #Just remember to tie it into a hypotheses to check for using regressions 
    ##(for instance half day leaves, cannot explain exits!)
  
  # you can make some nice plots here comparing these variables
  par(mfrow=c(1,1))    # set the plotting area into a 3*2 array
  boxplot(dt.e.c$boss_survey,dt.s.c$boss_survey,xlab="Boss Survey", names = c("Exit", "Remain"))
  boxplot(dt.e.c$job_satisfaction,dt.s.c$job_satisfaction,xlab="job Satisfaction", names = c("Exit", "Remain"))
  # additional plots to check intuition
  boxplot(dt.e.c$kpi_performance,dt.s.c$kpi_performance,xlab="Performance", names = c("Exit", "Remain"))
  boxplot(dt.e.c$salary,dt.s.c$salary,xlab="Salary", names = c("Exit", "Remain"))
  boxplot(dt.e.c$years_since_promotion,dt.s.c$years_since_promotion,xlab="years_since_promotion", names = c("Exit", "Remain"))
  
  #############
  #Q6##########
  
  prop.table(table(dt$exit)) # gives overall % of stays and exit
  # Ans: OVerall, 13.55% of employees left
  
  #6A
  # we want to calculate # of core employees who exited / # of core employees at the beginning
  
  #There are many ways to do this - here is a one line example
  sum(dt$exit[dt$core==1])/sum(dt$core) 
  # First term indicates # of exits among core employees - since exit is categorical and coded as 1, we can just sum
  # second term indicates total # of core employees at the beginning - again summing our categorical variable for core coded as 1
  # Ans - 13.72 % of core employees left. Compared to 13.55% of overall exit rate, this isn't a red flag
  
  #6B
  # again, a one line example to do this:
  sum(dt$exit[dt$high_potential==1])/sum(dt$high_potential) 
  #ans = 14.29%; a tad bit higher than normal. Any intervention could be aimed to stop this bleeding of high potentials
  
  
  #############
  #Q7##########
  # this is an open question.
  # Main points i wanted to see:
  # What the data is telling us:
  # 1. Employee demographic profiles are likely not indicating exit
  # 2. Employment Climate however is indicating differences between exits and remains:
    # particularly boss survey, and job satisfaction
    # potential interaction or relationship between these
  # Therefore next steps:
  # A deeper analysis using correlation tests and regression modeling to focus on
  # testing hypothesis that Employment Climate is causing exits
  
  #############
  # PArt B ##########
   # open questions
  
  # generally you folks were quite good!
  # we'll discuss some examples in class

