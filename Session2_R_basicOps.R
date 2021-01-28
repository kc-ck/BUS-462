
############################################################
# BUS 462 | Spring 2021 | Session 2
# Getting Started with R - Data Types and Data Manipulations
# CK 
# 20 JAN 2021 
## REFERENCES / Attribution: 
# (1) https://data.princeton.edu/R/gettingStarted ,
# (2) https://bookdown.org/animestina/R_Manchester/basic-principles-of-r-programming.html 
############################################################


###################################0
# 0 . Comments - anything starting with "#" means a comment ####
###################################0

# remember to hit CTRL-ENTER to run code in editor
# hit enter to run code on console

#### PREAMBLE : ## Clearing mem buffers ####
rm(list = ls(all.names = TRUE))# clear all files
gc() # garbage collector i.e.,  dump buffers and show available mem space
cat("\014")  # Clear Console


###################################0
# 1. Expressions and Assignments ####
###################################0

### Arithmetic:: R works like a calculator, you type an expression and get the answer:
  1 + 2
  
  
  # The standard arithmetic operators are +, -, *, /, and ^ for add, subtract, multiply, divide, and  exp
  #These operators have the standard BODMAS precedence 
  2^3
  
  
  # You can use standard mathematical functions, such as sqrt(), exp(), and log(). For example
  log(0.3/(1 - 0.3))
  
   
  
### Assignments :: Assigning values to variables are done by <- or =
  
  x <- 1    # creating an object x and assigning value of 1
  # x is now a vector object of class (data type) numeric with a single element with value 1
  # you will see the x object on your environment screen (top right quadrant)
  
  class(x) # see the class of x
  is.vector(x) # checks if x is a vector class object
  is.character(x)
  
  # you can now operate on x, # just like excel
  x+1
  x <- x+1 # operating value of x
  x
  View(x)
  
  y = 2 # Assign value 2 to another object
  
  z <- x + y # create a new object that is the sum of vectors x and y
  
  # is z a vector?
  is.vector(z) #  YES - [1] TRUE 
  
  #console print
  z
  
### LOGIC Operations ::   < > <= >= == != |-OR  &-and  
  x>y
  x>=y
  x<y
  z==y # note that == is different than =


###################################0
# 2. DATA STRUCTURES/ OBJECTS ####
###################################0

  #Think of objects as boxes. The names of the objects are only labels. 
  #Just like with boxes, it is convenient to label boxes in a way that is indicative of their contents, 
  # but the label itself does not determine the content.
  
 # R provides many functions to examine features of vectors and other objects, for example
  
    #class() - what kind of object is it (high-level)?
    #typeof() - what is the object's data type (low-level)?
    #length() - how long is it? What about two dimensional objects?
    #attributes() - does it have any metadata?
    
    
# Create a vector.
  apple <- c('red','green',"yellow")
  class(apple)

# Examining Vectors
  # The functions typeof(), length(), class() and str() provide useful information about your vectors and R objects in general.
  
  
  ## coercion: You can also control how vectors are coerced explicitly using the as.<class_name>() functions:
  #vectors of different classes
  xx <- c(1.7, "a")
  class(xx) #R implicitly coerces them to class or data type character
  xx <- as.integer(xx) # R explicitly coerces them to class type integar, introducing NAs
  xx # "a" becomes NA --- USEFUL to know when dealing with data sets 

# let's create another vector
  x <- c(1, 3, 5, 7) # if you already have another object called x, this will rewrite the old x!
  class(x)
  length(x) # how many elements of x?
  str(x) # structure of x
  
  # you can use : to generate a sequence of numbers
  x <- 1:10
  print(x) #or just x
  
  # seq can be used to create sequences
  x <- seq(0, 1, 0.05)
  x  

  # let's work with bigger vectors
  x <- seq(1, 100, 1)

  # you can use logical operations to check elements of x. e.g.:
  y <- x<5 # returns TRUE / FALSE for each element of x
  
  y <- x[x!=5] # outputs values of x that are leq 5 
  # these can now be assigned another object! ==> Very useful in analytics
  
  
  # you can find individual elements of vector x, i.e. x[i]
  x[3]  
  x[5]
  x[5] - x[99]
   
# let's create a matrix
  M = matrix(1:12, 3, 4) # the colon implies sequences
  M
  # like vector, you can call specific elements in M[i,j]
  M[1,3]
  
  # you can call indiviual rows or columns (to assign to other objects)
  M[3,] # row 3 of M
  M[,2] # inverted column 2
  
  
#### FACTORS (These are a bit weird)
  #They are used mainly for telling R that a vector represents a categorical variable.
  
  # create a vector of 15 "control"s and 15 "treatment"s
  # rep stands for 'repeat', which is exactly what the function does, similar to sequence
  x <- rep(c("control", "treatment"), each = 15)
  x
  
  # let's convert or coerce x into a factor
  x <- as.factor(x)
  x 
  # The first thing to notice is the line under the last printout that says "Levels: control treatment". 
  # This informs you that x is now a factor with two levels (or, a categorical variable with two categories).
  typeof(x) # x is stored as an int!
  class(x) # but the object is of class/data structure factor!

  
  # we can coerce factors to numeric to strip labels "control and treatment"
  x <- as.numeric(x)
  x # all controls became 1 and treatment became 2
  # you can coerce them back! Try it! look up how to assign new labels!
  x <- as.factor(x)
  x
  
### TREAT ALL CATEGORICAL VARS IN YOUR DATA as factors
  
  # let's play around some more
  # let's reassign x
  x <- rep(c("control", "treatment"), each = 15)
  x <- as.factor(x)
  attributes(x) # check the structure
  x
  
  # Thing to remember with factors:
  # The fact that factors in R are represented as labelled integers has interesting implications 
  # First, certain functions will coerce factors into numeric vectors which can shake things up. 
  # This happened when you used cbind() i.e. func to combine rows or columns  on a factor with levels 0 and 1:
  cbind(x[1:15], x[16:30])
  # cbind() binds the vectors you provide into the columns of a matrix. 
  #since matrices can only contain logical, numeric, and character elements, 
  # the cbind() function coerces the elements of the x into numeric, stripping the labels and leaving only 1s and 2s like we saw before
  
  # back to x 
  attributes(x) # check the structure
  # notice the labels or levels are separate from elements  
  # => even if you delete some of the numbers, the labels stay the same
  # let's demonstrate - plotting factors is easy! more on that later
  plot(x) 
  x <- x[1:20] # removing the last 10 obs
  plot(x)  # labels stay!
  x <- x[1:15] # removing all "Treatments"
  x    
  plot(x)  # labels stay! even if no values for treatment
  
  
  
  ### NOTE # Special cases:   
  #1:   NA, stands for "not applicable" and is used for missing data. 
  # Unlike other kinds of elements, it can be bound into a vector along with elements of any class.
  
  #2: NaN, stands for "not a number". 
  # It is technically of class numeric but only occurs as the output of invalid mathematical operations, 
  # such as dividing zero by zero or taking a square root of a negative number:
###############################
  
  
####  E100 STOP POINT 21 Jan 2020
  
###################################0
# 3. INTRO TO FUNCTIONS ####
###################################0
  # let's create a vector of numbers the mean of which I want to calculate
  vec <- c(103, 1, 1, 6, 3, 43, 2, 23, 7, 1)
  
  # see what's inside
  vec
  
  # let's get the mean
  # mean is the sum of all values divided by the number of values
  sum(vec)/length(vec)
  
  # good, now let's create a function that calculates the mean of whatever we ask it to
  # let's make this function as an obhject
  fn1 <- function(x) {sum(x)/length(x)} # stored in environment!
  # the code inside the object fn1 is reusable
  
  
  # to run function:
  fn1(vec) # returns mean of vec!
  # we can assign this to another object
  mean.vec <- fn1(vec)
  
### this is VERY USEFUL in analytics

  
  ###Let this be your mantra: "If I want to keep it for later, I need to put it in an object so that is doesn't go off." 

#### D100 STOP POINT 21 Jan 2020
  
  
###################################0
# 4. simple graphing ####
  ###################################0
# simple graphing
  
  z <- seq(-3, 3, 0.1)
  plot(z)  
  d<- dnorm(z)  #call the function dnorm() to calculate the standard normal density evaluated at each point in z
  plot(d) # plot the std normal density
  # standard plotting  -- look up plotting help!
  plot(z, d, type="l") 
  title("The Standard Normal Density", col.main="#3366cc") # add a title
  
  z2  <- rep(1,32)
  pie(z2, col = rainbow(32))
  # let's play with this
  z2[3] <- 5
  z2[6] <-5
  pie(z2, col = rainbow(32)) # showing you the relative sizes of element 3 and 6 compared to all else in z2
  barplot(z2)
  plot(z2)

  
  ###################################0
  # 5. A preview to Clean Data (Data Frames) and plotting ####
  # Note - A LOT MORE TO COME
  ###################################0  

  # For this example we'll use a very simple dataset. 
  # The cars data comes with the default installation of R. 
  # to see all pre-loaded datasets, simply type data()
  
  # to see a desc of the data
  ?cars
  # also try  for mtcars dataset 
  
  # To see the first few columns of the data, just type head(cars).
  head(cars)
  
  # let's create a data frame object and assign it to the default cars data
  dt <- cars 
  
  #check the structure of dt
  str(dt)
  
  View(dt) # let's see this data
  # we have obs for speed and stopping distance for cars in 1920s

  
  # let's calculate  average and std dev of speed and distance
  avg.speed <- mean(dt$speed) # notice the $ to point the general function mean to a specific column!
  avg.distance <- mean(dt$dist)
  
  sd.speed <- sqrt(var(dt$speed))
  sd.distance <- sqrt(var(dt$dist))
  
  # let's visualize the data
  hist(dt$dist) # histogram of distance
  hist(dt$speed)
  plot(dt$speed)
  plot(dt$dist)
  # let's see relationships
  cor(dt) # simplistic correlation matrix
  plot(dt$speed,dt$dist) # simple scatter plot
  
  # Q: Change the axis to the plot above!
  ####
  ###
  ###
  ####
  ###
  #######
  ###
  #######
  ###
  #######
  ###
  #######
  ###
  #######
  ###
  #######
  ###
  ###
  plot(dt$dist,dt$speed)
  
  
  ## Going forward, we shall use other packages and the like to explore data and make plots
  
  
  ### END OF CODE #####