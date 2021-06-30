library(readr)
library(ggplot2)

wind <- read_csv("pgh_weather.csv")

dir <- hist(wind$`Wind Dir`)
br <- 10*(c(0:36))
dir <- hist(wind$`Wind Dir`, breaks = br)

# we need to pull the counts and mids from histogram data and put in a regular
# data frame for the next part to work
angle <- dir$mids
count <- dir$counts

y <- data.frame(angle, count)

ggplot(y, aes(x = angle, y = count)) + #AES = aesthetic
  geom_col(fill = 'steelblue', color = "steelblue") + 
  coord_polar(theta = "x", start = 0 ) #theta = angle 
  #This creats a wind rose
