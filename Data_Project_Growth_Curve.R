#DATA PROJECT
#Using data collected from a growth curve done on the plate reader with AV1 

install.packages("growthcurver")
install.packages("purrr")

library(readr) 
library(dplyr)
library(ggplot2)
library(growthcurver)
library(purrr)
library(matrixStats)
library(reshape2)

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


###############################################################################
## Condition 1

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

ggplot(gc, aes(x = Time, y = B1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Time, y = C1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Time, y = D1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Time, y = E1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Time, y = F1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Time, y = G1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()

ggplot(gc, aes(x = Time, y = H1)) + geom_point(alpha=0.7) +
  labs(x = "Hours", y = "OD600 - Log 10") +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  scale_y_log10()
####

##Combine replicates onto the same scatter plot 
ggplot(gc, aes(x = Time)) + 
  geom_point(aes(y=A1, color="A1")) +
  geom_point(aes(y=B1, color="B1")) +
  geom_point(aes(y=C1, color="C1")) +
  geom_point(aes(y=D1, color="D1")) +
  geom_point(aes(y=E1, color="E1")) +
  geom_point(aes(y=F1, color="F1")) +
  geom_point(aes(y=G1, color="G1")) +
  geom_point(aes(y=H1, color="H1")) +
  labs(x = "Hours", y = "OD600 - Log 10") 
#This would allow for a quick visualization of how the growth curves compare
#prior to taking the mean and may identify potential outliers
# Determine if their is significant differences between replicates


##To get the mean & standard deviation of replicates
gc_1 <- gc %>%
  rowwise() %>%
  mutate(
    m = mean(c(A1,B1,C1,D1,E1,F1,G1,H1)),
    sd = sd(c(A1,B1,C1,D1,E1,F1,G1,H1))
    )
#Will add an additional column named "m" to your df that takes the average of A1, B1...
#sd will calculate the standard deviation of the replicates


##To plot the average & sd of the 8 replicates
ggplot(gc_1, aes(x = Time, y = m)) + geom_point(alpha=0.7, color = "blue") +
  
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_y_log10() +
  
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +

  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_line(colour = "grey85"),
    ) +
  
  geom_errorbar(aes(ymin=m-sd, ymax=m+sd), width=.1,
                position=position_dodge(.9))
#This graphs the mean & standard deviation of the 8 replicates 
#  grids(linetype = "dashed")









##Zoom in on exponential phase of growth to get doubling time
ex1 <- gc_1[c(5:26),c(1,99)]
#creates a dataset with just time & mean of condition 1
#c(1:105)...rows 1:105
#c(1,99)...columns 1&99

install.packages("ggpubr")
library(ggpubr)
install.packages("Hmisc")
install.packages("broom")
library(broom) 
library(lattice)
library(Hmisc)
install.packages("ggpmisc")
library(ggpmisc)


#Plot just the exponential phase of the growth curve
ggplot(ex1, aes(x = Time, y = m)) + geom_point(alpha=0.7, color = "blue") +
  
  scale_y_log10() +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  
  ggtitle("Exponential Phase AV1 R2A pH 4.0 30C 1:25 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  
  
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_line(colour = "grey85"),
  ) +
  
  stat_smooth(method = 'nls', 
              method.args = list(start = c(a=1, b=1)), 
              formula = y~a*exp(b*x), 
              se = FALSE) +
  
  stat_regline_equation(label.x = 1, aes(label = ..rr.label..)) +

  stat_regline_equation(
      label.x = 1, 
      label.y = log(.545), 
      aes(label = ..eq.label..))
  
 


#Plot just the exponential phase of the growth curve
x <- ex1$Time
y <- ex1$m

df <- data.frame(x, y)

exp.mod    <- lm(log(y) ~ x, df)

new_x      <- seq(min(x), max(x), 0.01)
prediction <- exp(predict(exp.mod, newdata = list(x = new_x)))
exp_line   <- data.frame(x = new_x, y = prediction)

eq <- paste0('paste(y, " = ", italic(e^{',  round(exp.mod$coefficients[2], 2), 
             "*x ~~+~~ ", round(exp.mod$coefficients[1], 2),
             '}), ~~~~~~~~R^2~ "="~', round(summary(exp.mod)$r.squared, 2), ")")

ex1_plot <- ggplot(data = df, mapping = aes(x, y)) + 
  geom_point(alpha=0.7, color = "blue") +
  
  scale_y_log10() +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  
  ggtitle("Exponential Phase AV1 R2A pH 4.0 30C 1:25 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_line(colour = "grey85"),
  ) +
  
  geom_line(data = exp_line) +
  geom_text(aes(x = min(x) + 0.1 * diff(range(x)), 
                y = min(y) + 0.9 * diff(range(y)), label = eq), 
            parse = TRUE, size = 5, check_overlap = TRUE, hjust = 0)

ex1_plot


  


 






# From the graph, estimate the approximate exponential phase and select that time window
# approx. time 1 hr - 10 hr 
#can i use an equation to tell me when my doubling time is occuring?
# N1 = cells at time 1
# n2 = cells at time 2
# doubling time is when N2/N1 is equal to 2 
#stat_smooth(method = "lm") .... adds an exponential trend line
#this will appear straight since its on a log scale y axis. 












###############################################################################
## Condition 2 

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
