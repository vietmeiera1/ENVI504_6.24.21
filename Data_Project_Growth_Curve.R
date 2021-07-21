#DATA PROJECT
#Using data collected from a growth curve done on the plate reader with AV1 

install.packages("growthcurver")
install.packages("purrr")

library(readr) 
library(dplyr)
library(ggplot2)
library(growthcurver)
library(purrr)

setwd("~/Documents/Duquesne/Year_2_F20_S21_U21/Summer 2021/ENVI 404:504 Computer Tools for Scientists Excel & R/RStudio/ENVI504_6.24.21")

gc <- read_csv("AV_85.columns.csv", locale = locale(encoding = "Latin1"), skip = 17, col_names = TRUE) #load in datafile. Skip 1st 17 rows. Column names listed
#BEFORE loading file, manually change time to hour
# ^^UNLESS I CAN FIND A WAY TO DO THIS IN R??
#Make sure Hour column is general format and loads as a number
#Latin1 was added, as I was getting an error code later in the doc
#Must also delete the original time column since it was being read in as a chr and not num which interfered with the predict(model.wt$model) code
# The error was due to the file being loaded in and the letters being recognized in greek 

#Can either manually rename each column -OR- tear apart using strsplit command 
# strsplit for reference https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strsplit 





## GROWTH CONDITION: AV1 in R2A pH 4.0 30C 1:25 subculture
#want to treat each column as an independent growth curve. 
ggplot(gc, aes(x = Time, y = A1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "plain", size = 12))
  scale_y_log10() 
#the alpha # tell you how dark to make the point - the greater the number, the darker the point. 
# labs = labels for each axis 
# ggtitle() = title of the graph
# to make log 10 scale, scale_y_log10()
# can I loop this to do each column?
# theme(aspect.ratio = 1) ...creates a square sized plit
#  theme(panel.background = element_rect(fill = "white", colour = "black")) + ...makes background white

ggplot(gc, aes(x = Hour, y = B1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Hour, y = C1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Hour, y = D1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Hour, y = E1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Hour, y = F1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Hour, y = G1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Hour, y = H1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()


# x <- mean(cbind(gc$A1, gc$B1, gc$C1, gc$D1, gc$E1, gc$F1, gc$G1, gc$H1), na.rm = TRUE)
# ##This does not work, it thinks the second item should be TRIM
# 
# A1 <- gc$A1
# B1 <- gc$B1
# mean("AV1", "B1")
# # This also does not work, it says it is not logical or numeric 

gc_1 <- gc %>%
  rowwise() %>%
  mutate(
    m = mean(c(A1,B1,C1,D1,E1,F1,G1,H1))
    )
#Will add an additional column named "m" to your df that takes the average of A1, B1...

library(matrixStats)
gc_1 <- gc_1 %>%
  rowwise() %>%
  mutate(
    sd = sd(c(A1,B1,C1,D1,E1,F1,G1,H1))
  )
#This will calculate the standard deviation of the replicates

ggplot(gc_1, aes(x = Time, y = m)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10() +
  geom_errorbar(aes(ymin=m-sd, ymax=m+sd), width=.1,
                position=position_dodge(.9)) 
#This graphs the mean & standard deviation of the 8 replicates 












ggplot(gc, aes(x = Hour, y = A2)) + geom_point(alpha=2.0)


#libary(STRINGR)
#filter(STR-Detect(Column_Name, "letter of interest"))


########################################

# For reference on RStudio growth curves https://rpubs.com/angelov/growthcurver 
# based on the Growthcurver package by Sprouffske et. al.
# https://www.ncbi.nlm.nih.gov/pubmed/27094401
model.wt <- SummarizeGrowth(gc$Time, gc$A1)
model.wt$vals
##This gives you all the values, growth rate, etc. 

predict(model.wt$model)
#gives you the predicted OD values according to the model
#this will be needed for the trendline on the scatter plot 


growth.values.plate <- SummarizeGrowthByPlate(gc) 
#Growthcurver package has a function the can run the fit for many samples
# This code gives an error because their are 

# gc <- gc %>% filter(Hour<=24) #use this code if you want to filter to a MAX number of hours 

model.A1 <- SummarizeGrowth(gc$Hour, gc$AV1)
# ^^ This code is not working, it gives me an error =/

model.A1$vals
predict(model.AV1$model)

growth.values.plate <- SummarizeGrowthByPlate(gc)
