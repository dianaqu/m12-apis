### Exercise 3 ###

# libraries
# install.packages('dplyr')
library(dplyr)

# We'll be using the mtcars dataset -- adding rowname as a column
# vehicle <- add_rownames(mtcars, var='car')
mtcars$car <- rownames(mtcars)
vehicle <- mtcars

######## buggy ######### ----------------------------------------------

# Function to debug:
# Given a number of forward gears and a number of cylinders, what is the 
# Name of the car with the best mpg?
BestGearsCyl <- functon(gears, cylinders) {
  ret <- vehicles %>%  
        filter(gear == gears, cyl == cylinders) %>% 
        filter(mgp = max(mpg)) %>% 
        select(car)
}

# Get the best mpg car for 6 cylinder cars with 4 gears
answer <- BestGearsCyl(6, 4)
