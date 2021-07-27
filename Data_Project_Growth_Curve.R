#DATA PROJECT
#Using data collected from a growth curve done on the plate reader with AV1 
install.packages("ggpubr")
library(ggpubr)
install.packages("Hmisc")
install.packages("broom")
library(broom) 
library(lattice)
library(Hmisc)
install.packages("ggpmisc")
library(ggpmisc)

library(readr) 
library(dplyr)
library(ggplot2)
library(growthcurver)
library(purrr)
library(matrixStats)
library(reshape2)


library(ggpubr)
library(broom) 
library(lattice)
library(Hmisc)
library(ggpmisc)


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
## Control:Blank ################################################################

##Combine replicates onto the same scatter plot 
ggplot(gc, aes(x = Time)) + 
  geom_point(aes(y=A8, color="A8")) +
  geom_point(aes(y=B8, color="B1")) +
  geom_point(aes(y=C8, color="C1")) +
  geom_point(aes(y=D8, color="D1")) +
  geom_point(aes(y=E8, color="E1")) +
  geom_point(aes(y=F8, color="F1")) +
  geom_point(aes(y=G8, color="G1")) +
  geom_point(aes(y=H8, color="H1")) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  labs(x = "Hours", y = "OD600 (Log 10)") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  ggtitle("R2A pH 4.0 30C") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  theme(axis.title = element_text(size = 15)) 


gc_R2A <- gc %>%
  rowwise() %>%
  mutate(
    m.R2A = mean(c(A8,B8,C8,D8,E8,F8,G8,H8)),
    sd.R2A = sd(c(A8,B8,C8,D8,E8,F8,G8,H8))
  )
#Will add an additional column named "m" to your df that takes the average of A1, B1...
#sd will calculate the standard deviation of the replicates

##To plot the average & sd of the 8 replicates
ggplot(gc_R2A, aes(x = Time, y = m.R2A)) + geom_point(alpha=0.7, color = "blue") +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_y_log10() +
  ggtitle("R2A pH 4.0 30C") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  geom_errorbar(aes(ymin=m.R2A-sd.R2A, ymax=m.R2A+sd.R2A), width=.1,
                position=position_dodge(.9))


###############################################################################
## Condition 1 ################################################################

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
  geom_point(aes(y=(A1-A8), color="A1")) +
  geom_point(aes(y=(B1-B8), color="B1")) +
  geom_point(aes(y=(C1-C8), color="C1")) +
  geom_point(aes(y=(D1-D8), color="D1")) +
  geom_point(aes(y=(E1-E8), color="E1")) +
  geom_point(aes(y=(F1-F8), color="F1")) +
  geom_point(aes(y=(G1-G8), color="G1")) +
  geom_point(aes(y=(H1-H8), color="H1")) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  labs(x = "Hours", y = "OD600 (Log 10)") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),  ) +
  ggtitle("AV1 R2A pH 4.0 30C 1:25 subculture replicates") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  theme(axis.title = element_text(size = 15)) 
#This allows for a quick visualization of how the growth curves compare
#May identify potential outliers


##To get the mean & standard deviation of replicates - the blank
gc_1 <- gc %>%
  rowwise() %>%
  mutate(
    m.R2A = (mean(c(A1,B1,C1,D1,E1,F1,G1,H1)) - mean(c(A8,B8,C8,D8,E8,F8,G8,H8))),
    sd = sd(c(A1,B1,C1,D1,E1,F1,G1,H1))
    )
#Will add an additional column named "m" to your df that takes the average of A1, B1...
#sd will calculate the standard deviation of the replicates


##To plot the average & sd of the 8 replicates
ggplot(gc_1, aes(x = Time, y = m.R2A)) + geom_point(alpha=0.7, color = "blue") +
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
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
    ) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  geom_errorbar(aes(ymin=m.R2A-sd, ymax=m.R2A+sd), width=.1,
                position=position_dodge(.9))
#This graphs the mean & standard deviation of the 8 replicates 


########## EXPONENTIAL GRAPH ###################
##Zoom in on exponential phase of growth to get doubling time
ex1 <- gc_1[c(5:26),c(1,99)]
#creates a dataset with just time & mean of condition 1
#c(1:105)...rows 1:105
#c(1,99)...columns 1&99



#Plot just the exponential phase of the growth curve
x <- ex1$Time
y <- ex1$m.R2A

df <- data.frame(x, y)

exp.mod    <- lm(log(y) ~ x, df)

new_x      <- seq(min(x), max(x), 0.01)
prediction <- exp(predict(exp.mod, newdata = list(x = new_x)))
exp_line   <- data.frame(x = new_x, y = prediction)

# eq <- paste0('paste(y, " = "', round(exp(exp.mod$coefficients[1], 2)), 'italic(e^{',  round(exp.mod$coefficients[2], 2), 
#              "*x ~~+~~ ", 
#              '}), ~~~~~~~~R^2~ "="~', round(summary(exp.mod)$r.squared, 2), ")")
# eq <- expression("y = ", round(exp(exp.mod$coefficients[1]), 2), "e"^{round(exp.mod$coefficients[2], 2)})
# 
# 
# eq <- paste0('paste(y, " = ", italic(e{',  round(exp.mod$coefficients[2], 2),
#              "*x ~~+~~ ", round(exp.mod$coefficients[1], 2),
#              '}), ~~~~~~~~R^2~ "="~', round(summary(exp.mod)$r.squared, 2), ")")

# eq <- paste0('paste(y, " = ", italic(e^{',  round(exp.mod$coefficients[2], 2),
#             "*x ~~+~~ ", round(exp.mod$coefficients[1], 2),
#             '}), ~~~~~~~~R^2~ "="~', round(summary(exp.mod)$r.squared, 2), ")")

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
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  
  geom_line(data = exp_line)
  # #geom_text(aes(x = min(x) + 0.1 * diff(range(x)),
  #               y = min(y) + 0.9 * diff(range(y)), label = eq),
  #           parse = TRUE, size = 5, check_overlap = TRUE, hjust = 0)

ex1_plot

summary(exp.mod)
 
R_sq <- round(summary(exp.mod)$r.squared, 2)
print(R_sq)

B <- as.numeric(round(exp.mod$coefficients[2], 2))
print(B)

dt <- log(2)/B
print(dt)


###############################################################################
## Condition 2 ################################################################
## GROWTH CONDITION: AV1 in R2A pH 4.0 30C 1:50 subculture
#want to treat each column as an independent growth curve. 
##Combine replicates onto the same scatter plot 
ggplot(gc, aes(x = Time)) + 
  geom_point(aes(y=(A2-A8), color="A2")) +
  geom_point(aes(y=(B2-A8), color="B2")) +
  geom_point(aes(y=(C2-C8), color="C2")) +
  geom_point(aes(y=(D2-D8), color="D2")) +
  geom_point(aes(y=(E2-E8), color="E2")) +
  geom_point(aes(y=(F2-F8), color="F2")) +
  geom_point(aes(y=(G2-G8), color="G2")) +
  geom_point(aes(y=(H2-H8), color="H2")) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  labs(x = "Hours", y = "OD600 (Log 10)") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  ggtitle("AV1 R2A pH 4.0 30C 1:50 subculture replicates") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  theme(axis.title = element_text(size = 15)) 
#This allows for a quick visualization of how the growth curves compare
#May identify potential outliers


##To get the mean & standard deviation of replicates
gc_2 <- gc %>%
  rowwise() %>%
  mutate(
    m2.R2A = (mean(c(A2,B2,C2,D2,E2,F2,G2,H2)) - mean(c(A8,B8,C8,D8,E8,F8,G8,H8))),
    sd = sd(c(A2,B2,C2,D2,E2,F2,G2,H2))
  )
#Will add an additional column named "m" to your df that takes the average of A1, B1...
#sd will calculate the standard deviation of the replicates

##To plot the average & sd of the 8 replicates
ggplot(gc_2, aes(x = Time, y = m2.R2A)) + geom_point(alpha=0.7, color = "blue") +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_y_log10() +
  ggtitle("AV1 R2A pH 4.0 30C 1:50 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  geom_errorbar(aes(ymin=m2.R2A-sd, ymax=m2.R2A+sd), width=.1,
                position=position_dodge(.9))
#This graphs the mean & standard deviation of the 8 replicates 

########## EXPONENTIAL GRAPH ###################
##Zoom in on exponential phase of growth to get doubling time
ex2 <- gc_2[c(5:26),c(1,99)]
#creates a dataset with just time & mean of condition 1
#c(1:105)...rows 1:105
#c(1,99)...columns 1&99

#Plot just the exponential phase of the growth curve
x.2 <- ex2$Time
y.2 <- ex2$m2.R2A

df2 <- data.frame(x.2, y.2)

exp.mod2    <- lm(log(y.2) ~ x.2, df2)

new_x2      <- seq(min(x.2), max(x.2), 0.01)
prediction2 <- exp(predict(exp.mod2, newdata = list(x.2 = new_x2)))
exp_line2   <- data.frame(x.2 = new_x2, y.2 = prediction2)

ex2_plot <- ggplot(data = df2, mapping = aes(x.2, y.2)) + 
  geom_point(alpha=0.7, color = "blue") +
  
  scale_y_log10() +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  
  ggtitle("Exponential Phase AV1 R2A pH 4.0 30C 1:50 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  
  geom_line(data = exp_line2)

ex2_plot

summary(exp.mod2)

R_sq.2 <- round(summary(exp.mod2)$r.squared, 2)
print(R_sq.2)

B.2 <- as.numeric(round(exp.mod2$coefficients[2], 2))
print(B.2)

dt.2 <- log(2)/B.2
print(dt.2)


###############################################################################
## Condition 3 ################################################################
## GROWTH CONDITION: AV1 in R2A pH 4.0 30C 1:100 subculture

##Combine replicates onto the same scatter plot 
ggplot(gc, aes(x = Time)) + 
  geom_point(aes(y=A3-A8, color="A3")) +
  geom_point(aes(y=B3-B8, color="B3")) +
  geom_point(aes(y=C3-C8, color="C3")) +
  geom_point(aes(y=D3-D8, color="D3")) +
  geom_point(aes(y=E3-E8, color="E3")) +
  geom_point(aes(y=F3-F8, color="F3")) +
  geom_point(aes(y=G3-G8, color="G3")) +
  geom_point(aes(y=H3-H8, color="H3")) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  labs(x = "Hours", y = "OD600 (Log 10)") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  ggtitle("AV1 R2A pH 4.0 30C 1:100 subculture replicates") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  theme(axis.title = element_text(size = 15)) 
#This allows for a quick visualization of how the growth curves compare
#May identify potential outliers
# Determine if their is significant differences between replicates

##To get the mean & standard deviation of replicates
gc_3.R2A <- gc %>%
  rowwise() %>%
  mutate(
    m3.R2A = (mean(c(A3,B3,C3,D3,E3,F3,G3,H3 - mean(c(A8,B8,C8,D8,E8,F8,G8,H8))))),
    sd3.R2A = sd(c(A3,B3,C3,D3,E3,F3,G3,H3))
  )
#Will add an additional column named "m" to your df that takes the average of A1, B1...
#sd will calculate the standard deviation of the replicates

##To plot the average & sd of the 8 replicates
ggplot(gc_3.R2A, aes(x = Time, y = m3.R2A)) + geom_point(alpha=0.7, color = "blue") +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_y_log10() +
  ggtitle("AV1 R2A pH 4.0 30C 1:100 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  geom_errorbar(aes(ymin=m3.R2A-sd3.R2A, ymax=m3.R2A+sd3.R2A), width=.1,
                position=position_dodge(.9))
#This graphs the mean & standard deviation of the 8 replicates 

########## EXPONENTIAL GRAPH ###################
##Zoom in on exponential phase of growth to get doubling time
ex3 <- gc_3.R2A[c(9:36),c(1,99)]
#creates a dataset with just time & mean of condition 1
#c(1:105)...rows 1:105
#c(1,99)...columns 1&99

#Plot just the exponential phase of the growth curve
x.3 <- ex3$Time
y.3 <- ex3$m3.R2A
df3 <- data.frame(x.3, y.3)
exp.mod3    <- lm(log(y.3) ~ x.3, df3)
new_x3      <- seq(min(x.3), max(x.3), 0.01)
prediction3 <- exp(predict(exp.mod3, newdata = list(x.3 = new_x3)))
exp_line3   <- data.frame(x.3 = new_x3, y.3 = prediction3)

ex3_plot <- ggplot(data = df3, mapping = aes(x.3, y.3)) + 
  geom_point(alpha=0.7, color = "blue") +
  scale_y_log10() +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  ggtitle("Exponential Phase AV1 R2A pH 4.0 30C 1:100 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  geom_line(data = exp_line3)
# #geom_text(aes(x = min(x) + 0.1 * diff(range(x)),
#               y = min(y) + 0.9 * diff(range(y)), label = eq),
#           parse = TRUE, size = 5, check_overlap = TRUE, hjust = 0)

ex3_plot

summary(exp.mod3)

R_sq.3 <- round(summary(exp.mod3)$r.squared, 2)
print(R_sq.3)

B.3 <- as.numeric(round(exp.mod3$coefficients[2], 2))
print(B.3)

dt.3 <- log(2)/B.3
print(dt.3)

###############################################################################
## Condition 4 ################################################################
## GROWTH CONDITION: AV1 in R2A pH 4.0 30C 1:200 subculture
#want to treat each column as an independent growth curve. 
##Combine replicates onto the same scatter plot 
ggplot(gc, aes(x = Time)) + 
  geom_point(aes(y=A4-A8, color="A4")) +
  geom_point(aes(y=B4-B8, color="B4")) +
  geom_point(aes(y=C4-C8, color="C4")) +
  geom_point(aes(y=D4-D8, color="D4")) +
  geom_point(aes(y=E4-E8, color="E4")) +
  geom_point(aes(y=F4-F8, color="F4")) +
  geom_point(aes(y=G4-G8, color="G4")) +
  geom_point(aes(y=H4-H8, color="H4")) +
  ggtitle("AV1 R2A pH 4.0 30C 1:200 subculture replicates") +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  labs(x = "Hours", y = "OD600 (Log 10)") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  theme(axis.title = element_text(size = 15)) 
#This allows for a quick visualization of how the growth curves compare
#May identify potential outliers
# Determine if their is significant differences between replicates


##To get the mean & standard deviation of replicates
gc_4.R2A <- gc %>%
  rowwise() %>%
  mutate(
    m4.R2A = (mean(c(A4,B4,C4,D4,E4,F4,G4,H4)) - - mean(c(A8,B8,C8,D8,E8,F8,G8,H8))),
    sd = sd(c(A4,B4,C4,D4,E4,F4,G4,H4))
  )
#Will add an additional column named "m" to your df that takes the average of A1, B1...
#sd will calculate the standard deviation of the replicates


##To plot the average & sd of the 8 replicates
ggplot(gc_4.R2A, aes(x = Time, y = m4.R2A)) + geom_point(alpha=0.7, color = "blue") +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_y_log10() +
  ggtitle("AV1 R2A pH 4.0 30C 1:200 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  geom_errorbar(aes(ymin=m4.R2A-sd, ymax=m4.R2A+sd), width=.1,
                position=position_dodge(.9))
#This graphs the mean & standard deviation of the 8 replicates 

########## EXPONENTIAL GRAPH ###################
##Zoom in on exponential phase of growth to get doubling time
ex4 <- gc_4.R2A[c(12:36),c(1,99)]
#creates a dataset with just time & mean of condition 1
#c(1:105)...rows 1:105
#c(1,99)...columns 1&99

#Plot just the exponential phase of the growth curve
x.4 <- ex4$Time
y.4 <- ex4$m4.R2A
df4 <- data.frame(x.4, y.4)
exp.mod4    <- lm(log(y.4) ~ x.4, df4)
new_x4      <- seq(min(x.4), max(x.4), 0.01)
prediction4 <- exp(predict(exp.mod4, newdata = list(x.4 = new_x4)))
exp_line4   <- data.frame(x.4 = new_x4, y.4 = prediction4)
ex4_plot <- ggplot(data = df4, mapping = aes(x.4, y.4)) + 
  geom_point(alpha=0.7, color = "blue") +
  scale_y_log10() +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  ggtitle("Exponential Phase AV1 R2A pH 4.0 30C 1:200 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  geom_line(data = exp_line4)
ex4_plot
summary(exp.mod4)

R_sq.4 <- round(summary(exp.mod4)$r.squared, 2)
print(R_sq.4)

B.4 <- as.numeric(round(exp.mod4$coefficients[2], 2))
print(B.4)

dt.4 <- log(2)/B.4
print(dt.4)

###############################################################################
## Condition 5 ################################################################
## GROWTH CONDITION: AV1 in R2A pH 4.0 30C 1:400 subculture
#want to treat each column as an independent growth curve. 
##Combine replicates onto the same scatter plot 
ggplot(gc, aes(x = Time)) + 
  geom_point(aes(y=A5-A8, color="A5")) +
  geom_point(aes(y=B5-B8, color="B5")) +
  geom_point(aes(y=C5-C8, color="C5")) +
  geom_point(aes(y=D5-D8, color="D5")) +
  geom_point(aes(y=E5-E8, color="E5")) +
  geom_point(aes(y=F5-F8, color="F5")) +
  geom_point(aes(y=G5-G8, color="G5")) +
  geom_point(aes(y=H5-H8, color="H5")) +
  ggtitle("AV1 R2A pH 4.0 30C 1:400 subculture replicates") +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  labs(x = "Hours", y = "OD600 (Log 10)") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  theme(axis.title = element_text(size = 15)) 
#This allows for a quick visualization of how the growth curves compare
#May identify potential outliers
# Determine if their is significant differences between replicates

##To get the mean & standard deviation of replicates
gc_5.R2A <- gc %>%
  rowwise() %>%
  mutate(
    m5 = (mean(c(A5,B5,C5,D5,E5,F5,G5,H5)) - mean(c(A8,B8,C8,D8,E8,F8,G8,H8))),
    sd = sd(c(A5,B5,C5,D5,E5,F5,G5,H5))
  )
#Will add an additional column named "m" to your df that takes the average of A1, B1...
#sd will calculate the standard deviation of the replicates

##To plot the average & sd of the 8 replicates
ggplot(gc_5.R2A, aes(x = Time, y = m5)) + geom_point(alpha=0.7, color = "blue") +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_y_log10() +
  ggtitle("AV1 R2A pH 4.0 30C 1:400 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  geom_errorbar(aes(ymin=m5-sd, ymax=m5+sd), width=.1,
                position=position_dodge(.9))
#This graphs the mean & standard deviation of the 8 replicates 

########## EXPONENTIAL GRAPH ###################
##Zoom in on exponential phase of growth to get doubling time
ex5 <- gc_5.R2A[c(5:26),c(1,99)]
#creates a dataset with just time & mean of condition 1
#c(1:105)...rows 1:105
#c(1,99)...columns 1&99

#Plot just the exponential phase of the growth curve
x.5 <- ex5$Time
y.5 <- ex5$m5

df5 <- data.frame(x.5, y.5)

exp.mod5    <- lm(log(y.5) ~ x.5, df5)

new_x5      <- seq(min(x.5), max(x.5), 0.01)
prediction5 <- exp(predict(exp.mod5, newdata = list(x.5 = new_x5)))
exp_line5   <- data.frame(x.5 = new_x5, y.5 = prediction5)

ex5_plot <- ggplot(data = df5, mapping = aes(x.5, y.5)) + 
  geom_point(alpha=0.7, color = "blue") +
  scale_y_log10() +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  ggtitle("Exponential Phase AV1 R2A pH 4.0 30C 1:400 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  geom_line(data = exp_line5)

ex5_plot

summary(exp.mod5)

R_sq.5 <- round(summary(exp.mod5)$r.squared, 2)
print(R_sq.5)

B.5 <- as.numeric(round(exp.mod5$coefficients[2], 2))
print(B.5)

dt.5 <- log(2)/B.5
print(dt.5)

###############################################################################
## Condition 6 ################################################################
## GROWTH CONDITION: AV1 in M63+L 30C 1:10 subculture
#want to treat each column as an independent growth curve. 
##Combine replicates onto the same scatter plot 
ggplot(gc, aes(x = Time)) + 
  geom_point(aes(y=A6-A9, color="A6")) +
  geom_point(aes(y=B6-B9, color="B6")) +
  geom_point(aes(y=C6-C9, color="C6")) +
  geom_point(aes(y=D6-D9, color="D6")) +
  geom_point(aes(y=E6-E9, color="E6")) +
  geom_point(aes(y=F6-F9, color="F6")) +
  geom_point(aes(y=G6-G9, color="G6")) +
  geom_point(aes(y=H6-H9, color="H6")) +
  ggtitle("AV1 in M63+L 30C 1:10 subculture replicates") +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  labs(x = "Hours", y = "OD600 (Log 10)") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  theme(axis.title = element_text(size = 15)) 
#This allows for a quick visualization of how the growth curves compare
#May identify potential outliers
# Determine if their is significant differences between replicates

##To get the mean & standard deviation of replicates
gc_6.M <- gc %>%
  rowwise() %>%
  mutate(
    m6 = (mean(c(A6,B6,C6,D6,E6,F6,G6,H6)) - mean(c(A9,B9,C9,D9,E9,F9,G9,H9))),
    sd = sd(c(A6,B6,C6,D6,E6,F6,G6,H6))
  )
#Will add an additional column named "m" to your df that takes the average of A1, B1...
#sd will calculate the standard deviation of the replicates

##To plot the average & sd of the 8 replicates
ggplot(gc_6.M, aes(x = Time, y = m6)) + geom_point(alpha=0.7, color = "blue") +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_y_log10() +
  ggtitle("AV1 in M63+L 30C 1:10 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  geom_errorbar(aes(ymin=m6-sd, ymax=m6+sd), width=.1,
                position=position_dodge(.9))
#This graphs the mean & standard deviation of the 8 replicates 

########## EXPONENTIAL GRAPH ###################
##Zoom in on exponential phase of growth to get doubling time
ex6 <- gc_6.M[c(6:36),c(1,99)]
#creates a dataset with just time & mean of condition 1
#c(1:105)...rows 1:105
#c(1,99)...columns 1&99

#Plot just the exponential phase of the growth curve
x.6 <- ex6$Time
y.6 <- ex6$m6

df6 <- data.frame(x.6, y.6)

exp.mod6    <- lm(log(y.6) ~ x.6, df6)

new_x6      <- seq(min(x.6), max(x.6), 0.01)
prediction6 <- exp(predict(exp.mod6, newdata = list(x.6 = new_x6)))
exp_line6   <- data.frame(x.6 = new_x6, y.6 = prediction6)

ex6_plot <- ggplot(data = df6, mapping = aes(x.6, y.6)) + 
  geom_point(alpha=0.7, color = "blue") +
  scale_y_log10() +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  ggtitle("Exponential Phase AV1 in M63+L 30C 1:10 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  geom_line(data = exp_line6)

ex6_plot

summary(exp.mod6)

R_sq.6 <- round(summary(exp.mod6)$r.squared, 2)
print(R_sq.6)

B.6 <- as.numeric(round(exp.mod6$coefficients[2], 2))
print(B.6)

dt.6 <- log(2)/B.6
print(dt.6)

###############################################################################
## Condition 7 ################################################################
## GROWTH CONDITION: AV1 in M63+A 30C 1:10 subculture
#want to treat each column as an independent growth curve. 
##Combine replicates onto the same scatter plot 
ggplot(gc, aes(x = Time)) + 
  geom_point(aes(y=A7-A10, color="A7")) +
  geom_point(aes(y=B7-B10, color="B7")) +
  geom_point(aes(y=C7-C10, color="C7")) +
  geom_point(aes(y=D7-D10, color="D7")) +
  geom_point(aes(y=E7-E10, color="E7")) +
  geom_point(aes(y=F7-F10, color="F7")) +
  geom_point(aes(y=G7-G10, color="G7")) +
  geom_point(aes(y=H7-H10, color="H7")) +
  ggtitle("AV1 in M63+A 30C 1:10 subculture replicates") +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  labs(x = "Hours", y = "OD600 (Log 10)") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  theme(axis.title = element_text(size = 15)) 
#This allows for a quick visualization of how the growth curves compare
#May identify potential outliers
# Determine if their is significant differences between replicates

##To get the mean & standard deviation of replicates
gc_7M <- gc %>%
  rowwise() %>%
  mutate(
    m7 = (mean(c(A7,B7,C7,D7,E7,F7,G7,H7)) - mean(c(A10,B10,C10,D10,E10,F10,G10,H10))),
    sd = sd(c(A7,B7,C7,D7,E7,F7,G7,H7))
  )
#Will add an additional column named "m" to your df that takes the average of A1, B1...
#sd will calculate the standard deviation of the replicates

##To plot the average & sd of the 8 replicates
ggplot(gc_7M, aes(x = Time, y = m7)) + geom_point(alpha=0.7, color = "blue") +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_y_log10() +
  ggtitle("AV1 in M63+A 30C 1:10 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  geom_errorbar(aes(ymin=m7-sd, ymax=m7+sd), width=.1,
                position=position_dodge(.9))
#This graphs the mean & standard deviation of the 8 replicates 

########## EXPONENTIAL GRAPH ###################
##Zoom in on exponential phase of growth to get doubling time
ex7 <- gc_7M[c(5:50),c(1,99)]
#creates a dataset with just time & mean of condition 1
#c(1:105)...rows 1:105
#c(1,99)...columns 1&99

#Plot just the exponential phase of the growth curve
x.7 <- ex7$Time
y.7 <- ex7$m7
df7 <- data.frame(x.7, y.7)
exp.mod7    <- lm(log(y.7) ~ x.7, df7)
new_x7      <- seq(min(x.7), max(x.7), 0.01)
prediction7 <- exp(predict(exp.mod7, newdata = list(x.7 = new_x7)))
exp_line7   <- data.frame(x.7 = new_x7, y.7 = prediction7)

ex7_plot <- ggplot(data = df7, mapping = aes(x.7, y.7)) + 
  geom_point(alpha=0.7, color = "blue") +
  scale_y_log10() +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  ggtitle("Exponential Phase AV1 in M63+A 30C 1:10 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  geom_line(data = exp_line7)
ex7_plot

summary(exp.mod7)

R_sq.7 <- round(summary(exp.mod7)$r.squared, 2)
print(R_sq.7)

B.7 <- as.numeric(round(exp.mod7$coefficients[2], 2))
print(B.7)

dt.7 <- log(2)/B.7
print(dt.7)

###############################################################################
## Condition 8 ################################################################
## GROWTH CONDITION: AV1 in M63+A 30C 1:10 subculture
#want to treat each column as an independent growth curve. 
##Combine replicates onto the same scatter plot 
ggplot(gc, aes(x = Time)) + 
  geom_point(aes(y=A7, color="A7")) +
  geom_point(aes(y=B7, color="B7")) +
  geom_point(aes(y=C7, color="C7")) +
  geom_point(aes(y=D7, color="D7")) +
  geom_point(aes(y=E7, color="E7")) +
  geom_point(aes(y=F7, color="F7")) +
  geom_point(aes(y=G7, color="G7")) +
  geom_point(aes(y=H7, color="H7")) +
  ggtitle("AV1 in M63+A 30C 1:10 subculture replicates") +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  labs(x = "Hours", y = "OD600 (Log 10)") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  theme(axis.title = element_text(size = 15)) 
#This allows for a quick visualization of how the growth curves compare
#May identify potential outliers
# Determine if their is significant differences between replicates

##To get the mean & standard deviation of replicates
gc_7 <- gc %>%
  rowwise() %>%
  mutate(
    m = mean(c(A7,B7,C7,D7,E7,F7,G7,H7)),
    sd = sd(c(A7,B7,C7,D7,E7,F7,G7,H7))
  )
#Will add an additional column named "m" to your df that takes the average of A1, B1...
#sd will calculate the standard deviation of the replicates

##To plot the average & sd of the 8 replicates
ggplot(gc_7, aes(x = Time, y = m)) + geom_point(alpha=0.7, color = "blue") +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_y_log10() +
  ggtitle("AV1 in M63+A 30C 1:10 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  theme(axis.text = element_text(face = "plain", size = 12)) + 
  geom_errorbar(aes(ymin=m-sd, ymax=m+sd), width=.1,
                position=position_dodge(.9))
#This graphs the mean & standard deviation of the 8 replicates 

########## EXPONENTIAL GRAPH ###################
##Zoom in on exponential phase of growth to get doubling time
ex7 <- gc_7[c(5:26),c(1,99)]
#creates a dataset with just time & mean of condition 1
#c(1:105)...rows 1:105
#c(1,99)...columns 1&99

#Plot just the exponential phase of the growth curve
x.7 <- ex7$Time
y.7 <- ex7$m
df7 <- data.frame(x.7, y.7)
exp.mod7    <- lm(log(y.7) ~ x.7, df7)
new_x7      <- seq(min(x.7), max(x.7), 0.01)
prediction7 <- exp(predict(exp.mod7, newdata = list(x.7 = new_x7)))
exp_line7   <- data.frame(x.7 = new_x7, y.7 = prediction7)

ex7_plot <- ggplot(data = df7, mapping = aes(x.7, y.7)) + 
  geom_point(alpha=0.7, color = "blue") +
  scale_y_log10() +
  labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
  theme(axis.title = element_text(size = 15)) +
  ggtitle("Exponential Phase AV1 in M63+A 30C 1:10 subculture") +
  theme(plot.title = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = .5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor.x = element_line(colour = "grey85", linetype = "dashed"),
  ) +
  geom_line(data = exp_line7)
ex7_plot

summary(exp.mod7)

R_sq.7 <- round(summary(exp.mod7)$r.squared, 2)
print(R_sq.7)

B.7 <- as.numeric(round(exp.mod7$coefficients[2], 2))
print(B.7)

dt.7 <- log(2)/B.7
print(dt.7)


##### WRITE DOUBLE TIME TABLE#####
#table(R_sq, B, dt)
#hydrology > disaster data set 
# Table data:
# All growth conditions & doubling times & R_sq values:

# All.con <- data.frame("R2A pH 4.0 1:25", "R2A pH 4.0 1:50", "R2A pH 4.0 1:100", 
#                       "R2A pH 4.0 1:200", "R2A pH 4.0 1:400", "M63+Lactate 1:10", 
#                       "M63+Acetate 1:10")

library(gt) 


Cons2 <- data.frame("Growth_Condition" =c("R2A pH 4.0 1:25", "R2A pH 4.0 1:50", "R2A pH 4.0 1:100", 
                                   "R2A pH 4.0 1:200", "R2A pH 4.0 1:400", "M63+Lactate 1:10", 
                                                   "M63+Acetate 1:10"),
                   "Doubling_Time" =c(dt, dt.2, dt.3, dt.4, dt.5, dt.6, dt.7),
                   "R_squared" = c(R_sq, R_sq.2, R_sq.3, R_sq.4, R_sq.5, R_sq.6, R_sq.7)
                   )
print(Cons)


Cons2 %>%
  gt() %>%
  tab_header( title = "Isolate AV1", subtitle = "30C") %>%
  fmt_number(columns = vars(R_squared), decimals = 2) %>%
  fmt_number(columns = vars(Doubling_Time), decimals = 2) #changes that number of decimals in the table to the hundredths place






# #Method gives linear line not exponetial line equation
# #Plot just the exponential phase of the growth curve
# ggplot(ex1, aes(x = Time, y = m)) + geom_point(alpha=0.7, color = "blue") +
# 
#   scale_y_log10() +
#   labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
#   theme(axis.title = element_text(size = 15)) +
# 
#   ggtitle("Exponential Phase AV1 R2A pH 4.0 30C 1:25 subculture") +
#   theme(plot.title = element_text(size = 16)) +
#   theme(plot.title = element_text(hjust = .5)) +
# 
#   theme(panel.background = element_rect(fill = "white", colour = "black")) +
#   theme(aspect.ratio = 1) +
# 
# 
#   theme(
#     panel.grid.major.x = element_line(color = "grey80"),
#     panel.grid.minor.x = element_line(colour = "grey85"),
#   ) +
# 
#   stat_smooth(method = 'nls',
#               method.args = list(start = c(a=1, b=1)),
#               formula = y~a*exp(b*x),
#               se = FALSE) +
# 
#   stat_regline_equation(label.x = 1, aes(label = ..rr.label..)) +
# 
#   stat_regline_equation(
#     formula = y~a*exp(b*x),
#     label.x.npc = "left",
#     label.y.npc = "top",
#     label.x = NULL,
#     label.y = NULL,
#     output.type = "expression",
#     geom = "text",
#     position = "identity",
#     na.rm = FALSE,
#     show.legend = NA,
#     inherit.aes = TRUE,
#   )
# 
# 
# 
# 
# 
# 
# ggplot(ex1, aes(x = Time, y = m)) + geom_point(alpha=0.7, color = "blue") +
#   
#   scale_y_log10() +
#   labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
#   theme(axis.title = element_text(size = 15)) +
#   
#   ggtitle("Exponential Phase AV1 R2A pH 4.0 30C 1:25 subculture") +
#   theme(plot.title = element_text(size = 16)) +
#   theme(plot.title = element_text(hjust = .5)) +
#   
#   theme(panel.background = element_rect(fill = "white", colour = "black")) +
#   theme(aspect.ratio = 1) +
#   
#   
#   theme(
#     panel.grid.major.x = element_line(color = "grey80"),
#     panel.grid.minor.x = element_line(colour = "grey85"),
#   ) +
#   
#   stat_smooth(method = 'nls',
#               method.args = list(start = c(a=1, b=1)),
#               formula = y~a*exp(b*x),
#               se = FALSE) +
#   
#   stat_regline_equation(label.x = 1, aes(label = ..rr.label..)) +
#   
#   stat_regline_equation(
#       label.x = 1,
#       label.y = log(.545),
#       aes(label = ..eq.label..))
#  
# 





# From the graph, estimate the approximate exponential phase and select that time window
# approx. time 1 hr - 10 hr 
#can i use an equation to tell me when my doubling time is occuring?
# N1 = cells at time 1
# n2 = cells at time 2
# doubling time is when N2/N1 is equal to 2 
#stat_smooth(method = "lm") .... adds an exponential trend line
#this will appear straight since its on a log scale y axis. 



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




# #Method gives linear line not exponetial line equation
# #Plot just the exponential phase of the growth curve
# ggplot(ex1, aes(x = Time, y = m)) + geom_point(alpha=0.7, color = "blue") +
#   
#   scale_y_log10() +
#   labs(x = "Time (Hours)", y = "OD600 (Log 10)") +
#   theme(axis.title = element_text(size = 15)) +
#   
#   ggtitle("Exponential Phase AV1 R2A pH 4.0 30C 1:25 subculture") +
#   theme(plot.title = element_text(size = 16)) +
#   theme(plot.title = element_text(hjust = .5)) +
#   
#   theme(panel.background = element_rect(fill = "white", colour = "black")) +
#   theme(aspect.ratio = 1) +
#   
#   
#   theme(
#     panel.grid.major.x = element_line(color = "grey80"),
#     panel.grid.minor.x = element_line(colour = "grey85"),
#   ) +
#   
#   stat_smooth(method = 'nls', 
#               method.args = list(start = c(a=1, b=1)), 
#               formula = y~a*exp(b*x), 
#               se = FALSE) +
#   
#   stat_regline_equation(label.x = 1, aes(label = ..rr.label..)) +
# 
#   stat_regline_equation(
#       label.x = 1, 
#       label.y = log(.545), 
#       aes(label = ..eq.label..))
