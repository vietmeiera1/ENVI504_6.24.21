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

#Can either manually rename each column -OR- tear apart using strsplit command 
# strsplit for reference https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strsplit 


# For reference on RStudio growth curves https://rpubs.com/angelov/growthcurver 
   


#want to treat each column as an independent growth curve. 
ggplot(gc, aes(x = Hour, y = A1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()
#the alpha # tell you how dark to make the point - the greater the number, the darker the point. 
# labs = labels for each axis 
# ggtitle() = title of the graph
# to make log 10 scale, scale_y_log10()

# can I loop this to do each column?

ggplot(gc, aes(x = Hour, y = A2)) + geom_point(alpha=2.0)






# gc <- gc %>% filter(Hour<=24) #use this code if you want to filter to a MAX number of hours 

model.A1 <- SummarizeGrowth(gc$Hour, gc$AV1)
# ^^ This code is not working, it gives me an error =/



model.A1$vals
predict(model.AV1$model)

growth.values.plate <- SummarizeGrowthByPlate(gc)
