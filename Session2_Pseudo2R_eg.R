############################################################
# BUS 462 | Spring 2021 | Session 2
# ### PSEUDOCODE TO R CODE EXAMPLE in CLASS
# CK 
# 20 JAN 2021 
############################################################


##PSEUDOCODE to find average of 10 individual grades
#1 Set total to zero
#2 Set grade counter to one
#3 While grade counter is less than or equal to ten
#a Input the next grade
#b Add the grade into the total 
#4 Set the class average to the total divided by ten
#5 Print the class average

## R CODE
Total <- as.integer(0)
GradeCounter <-0
for(GradeCounter in 1:10) {
  Total <- as.integer(readline(prompt="Enter Grade: ")) + Total
  GradeCounter <- GradeCounter + 1
}
x <- Total/10
print(paste("Class Grade average is",x))

