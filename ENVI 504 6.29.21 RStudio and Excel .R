#T, 6/29/21
library(e1071)
library(readr)

#read data, perform basic statistics and visualization
setwd("/Users/annavietmeier/Downloads") #set working directory

data <- read_csv("2622217.csv") #reads file using loaded readr
data <- read.csv("2622217.csv") #reads file using RStudio pre-loaded code, not as nice

data$TOBS # $ sign lets you select a specific column 

m <- mean(data$TOBS, na.rm = TRUE) #adding na.rm = TRUE is the default function in Excel
  #This removes the "NA" from the file. NA is a gap in the data. 
s <- sd(data$TOBS, na.rm = TRUE) #standard deviation of the column TOBS. 

sk <- skewness(data$TOBS, na.rm = TRUE) #we have these functions from loading e1071
kt <- kurtosis(data$TOBS, na.rm = TRUE) #we have this function from loading e1071

h <- hist(data$TOBS, breaks = c(-30, -20, -10, 0, 10, 20, 30)) #plot data in a histogram 
  #the c is short for concatnat, meaning to stack on on top of another. 
  #this stacks a bunch of numbers into the same vector


