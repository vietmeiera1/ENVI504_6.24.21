#DATA PROJECT
#Using data collected from a growth curve done on the plate reader with AV1 

install.packages("growthcurver")

library(readr) 
library(dplyr)
library(ggplot2)
library(growthcurver)

gc <- read_csv("AV_85.columns.csv", skip = 17, col_names = TRUE) #load in datafile. Skip 1st 17 rows. Column names listed
#BEFORE loading file, manually change time to hour
# ^^UNLESS I CAN FIND A WAY TO DO THIS IN R??
#Make sure Hour column is general format and loads as a number


# For reference on RStudio growth curves https://rpubs.com/angelov/growthcurver 


#want to treat each column as an independent growth curve. 
ggplot(gc, aes(x = Hour, y = A1)) + geom_point(alpha=0.7)  
ggplot(gc, aes(x = Hour, y = A2)) + geom_point(alpha=2.0)
#the alpha # tell you how dark to make the point.
#the greater the number, the darker the point. 





# gc <- gc %>% filter(Hour<=24) #use this code if you want to filter to a MAX number of hours 

model.A1 <- SummarizeGrowth(gc$Hour, gc$AV1)
# ^^ This code is not working, it gives me an error =/



model.A1$vals
predict(model.AV1$model)

growth.values.plate <- SummarizeGrowthByPlate(gc)
