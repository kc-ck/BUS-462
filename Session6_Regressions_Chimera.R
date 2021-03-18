# Foldover notes

## Class Exercise -- Tell me what performs best

Model1 <- lm(job_satisfaction ~ team_size, data=dt)

Model2 <- lm(job_satisfaction ~ team_size + years_since_promotion, data=dt)

Model3 <- lm(job_satisfaction ~ team_size + years_since_promotion + salary, data=dt)

Model4 <- lm(job_satisfaction ~ team_size + years_since_promotion + salary + kpi_performance , data=dt)

stargazer(Model1,Model2,Model3,Model4,type="text",column.sep.width = "1pt",omit.stat=c("f"))

stargazer(Model.KS,Model4,type="text")


# LEt's try some interactions 
dt$perfxsal <- dt$salary * dt$kpi_performance
Model4A <- lm(job_satisfaction ~ team_size + years_since_promotion + salary + kpi_performance + perfxsal , data=dt)

stargazer(Model4A,Model4,type="text", single.row = TRUE)

stargazer(Model.KS,Model4, Model4A,type="text")

## IS MODEL BETTEr:
# Look at AIC values



# Regression Models

# DV: Exit
# IV: Boss Survey

Model0 <- lm(exit ~ 1, data=dt)
Model1A <- lm(exit ~ boss_survey -1, data=dt) # -1 here removes the intercept
Model1 <- lm(exit ~ boss_survey, data=dt)
summary(Model0)
summary(Model1A)
summary(Model1)


# Take a note of how much of the variation of "exit" are we capturing with just boss_survey.
# look at adj r-squared
# Is this model of single variable significantly different from a model with just a constant?
#  look at f-statistic
# Does the boss_survey variable have a meaningful effect on the dv?
#  look at the coefficient size. Is it non zero?
#  look at the p-value, is it significant?

#  Starting with variables of interest helps you avoid overfitting a model with irrelevant data

## CONTROLING FOR CONFOUNDS

# step 1: Plot correlation matrix - Pairwise correlation

round(cor(dt),2)

cor.test(dt)


# step 2: RE-define the model: Add new variables
Model2 <- lm(exit ~ boss_survey+job_satisfaction, data=dt)

summary(Model2)
# Did the addition of the new variable make a significant contribution?

# Did Adj R-squared improve?
#  Do the newly added variables have a significant effect on the dv? (check coefficient size and p-value)
# Do the newly added variables have a significant effect on the initial variables of interest? (check to see if coefficient size and p-values of the intial variables are different in this regression vs the one in step 3.2

# You can keep new variables that add value to the model (increase in adj r-sqd or significant impact on dv)
# , and "throw out" any of the variables that do not add value. When throwing out variables, do it one by one and make sure that they indeed have zero effect on the model. We "throw out" variables that are not relevant (no informational value) because we want to keep our model lean and prevent overfitting.

#step 3 Add more variables that can control for bias that's inherent to the data. 
