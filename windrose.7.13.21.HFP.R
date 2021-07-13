## Wind data from Heinz Field
## Example of rose plot of wind direction
##Adjust bins and wind speeds to get the code functional. 
##Homework 7/13/21 due 7/15/21

library(tidyverse) # includes ggplot and readr commands
library(RColorBrewer) # used for wind rose with color code by speed
library(lubridate) # used for dates, specifically month separation

## Heinz Field (HFP)
hfp <- read_csv("https://duq.box.com/shared/static/2cs6xi81xtcmq4mmi46t0v0ev4f2mehs.csv")
hfp$date <- as_date(hfp$Timestamp)
hfp$month <- month(hfp$date)
hfp$dir <- hfp$`Wind Vane` # degrees
hfp$spd <- 0.44704 * hfp$Anemometer # converted to m/s from mph, per https://allegheny.weatherstem.com/pitt

# OLD METHOD - no speed binning
br <- 10*(c(0:36)) # This array constructs the bins in degrees
h <- hist(hfp$dir, breaks = br)
# Make rose plot, based on:
# https://stackoverflow.com/questions/39024758/how-to-use-r-package-circular-to-make-rose-plot-of-histogram-data-on-360/39025913
# https://stackoverflow.com/questions/50163352/plot-wind-rose-in-r
angle <- h$mids
count <- h$counts
y <- data.frame(angle, count)
ggplot(y, aes(x = angle, y = count)) +
  labs(caption = "Heinz Field") +
  geom_col(fill = "gold", color = "yellow") +
  coord_polar(theta = "x", start = 0) +
  scale_x_continuous(breaks = seq(0, 360, 45)) +
  theme_linedraw() +
  theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank())

## NEW METHOD - with speed binning
# divide by direction & speed 
speed.bins <- 6 
dir.bins <- 36

######How do I decide the speed.bins & dir.bins?







wind <- array(0, dim = c(speed.bins, dir.bins))
for (i in 1:nrow(pit)) {
  j <- ceiling(hfc$dir[i]/10)
  k <- ceiling(hfc$spd[i]/2)
  if(k >6) { #brute force correction for speeds over 12m/s
    k <- 6
  }
  wind[k,j] <- wind[k,j] + 1
}
#preallocate data via array, NA = not a number, 0 = zero
# 0 + 1 = 1, NA + 1 = NA
# dim = ...dimension, c = concatenate
# for() ...for loop (index variable in range AKA vector)



# ## Now, form long array rather than wide:
wind.long <- array(NA, dim = dir.bins*speed.bins)
speeds <- c(rep("0-2",dir.bins), rep("2-4",dir.bins), rep("4-6",dir.bins), rep("6-8",dir.bins), rep("8-10",dir.bins), rep("above 10",dir.bins)) # be sure to fill in as many as the wind bins in "wind" allocation
directions <- rep(5+10*(c(0:35)), speed.bins)
for (i in 1:speed.bins) {
  for (j in 1:dir.bins) {
    wind.long[(dir.bins*(i-1))+j] <- wind[i,j]
  }
}
#rep() .... repeat 

rose <- data.frame(directions, speeds, wind.long)
ggplot(rose, aes(fill = fct_rev(speeds), x = directions, y = wind.long)) +
  labs(caption = paste("Heinz Field")) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer("Speed (m/s)", palette = "Blues") +
  coord_polar(theta = "x", start = 0) +
  scale_x_continuous(breaks = seq(0, 360, 45)) +
  theme_linedraw() +
  theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) # NOTE: ylim used in export

# # caption = "Pittsburgh International Airport" + 
# # caption = "Heinz Field" + 





















## OLD METHOD - no speed binning
# br <- 10*(c(0:36)) # This array constructs the bins in degrees
# h <- hist(hfp.dir, breaks = br)
## Make rose plot, based on:
## https://stackoverflow.com/questions/39024758/how-to-use-r-package-circular-to-make-rose-plot-of-histogram-data-on-360/39025913
## https://stackoverflow.com/questions/50163352/plot-wind-rose-in-r
# angle <- h$mids
# count <- h$counts
# y <- data.frame(angle, count)
# ggplot(y, aes(x = angle, y = count)) + 
#       labs(caption = "Heinz Field") + 
#       geom_col(fill = "steelblue", color = "steelblue") +
#       coord_polar(theta = "x", start = 0) +
#       scale_x_continuous(breaks = seq(0, 360, 45)) +
#       theme_linedraw() +
#       theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) 

# ## NEW METHOD - with speed binning
# ## sort data:
# d <- x$dir
# s <- x$spd
# speed.bins <- 7 # this used for monthly data at HFP
# # speed.bins <- ceiling(max(s)) # HFP will work with this binning.
# # speed.bins <- 6 # PIT needs this binning
# wind <- array(0, dim = c(36,speed.bins))
# for (i in 1:(length(s))) {
#       speed.index <- ceiling(s[i]) # HFP will work with this binning.
#       ## for use when speed categories exceed colormap (>9)
#       # if (s[i] <= 2) {
#       #       speed.index <- 1
#       # } else if (s[i] <= 4) {
#       #       speed.index <- 2
#       # } else if (s[i] <= 6) {
#       #       speed.index <- 3
#       # } else if (s[i] <= 8) {
#       #       speed.index <- 4
#       # } else if (s[i] <= 10) {
#       #       speed.index <- 5
#       # } else {
#       #       speed.index <- 6
#       # }
#       wind[ceiling(d[i]/10),speed.index] <- wind[ceiling(d[i]/10),speed.index] + 1
# }
# ## Now, form long array rather than wide:
# wind.long <- array(NA, dim = 36*speed.bins)
# for (i in 1:speed.bins) {
#       for (j in 1:36) {
#             wind.long[(36*(i-1))+j] <- wind[j,i]
#       }
# }
# # speeds <- c(rep("0-2",36), rep("2-4",36), rep("4-6",36), rep("6-8",36), rep("8-10",36), rep("above 10",36)) # be sure to fill in as many as the wind bins in "wind" allocation
# speeds <- c(rep("0-1",36), rep("1-2",36), rep("2-3",36), rep("3-4",36), rep("4-5",36), rep("5-6",36), rep("6-7",36)) # for HFP
# directions <- rep(5+10*(c(0:35)), speed.bins)
# rose <- data.frame(directions, speeds, wind.long)
# 
# windHFP <- ggplot(rose, aes(fill = fct_rev(speeds), x = directions, y = wind.long)) + 
#       labs(caption = paste("Pittsburgh International Airport")) + 
#       geom_bar(position="stack", stat="identity") +
#       scale_fill_brewer("Speed (m/s)", palette = "Blues") +
#       coord_polar(theta = "x", start = 0) +
#       scale_x_continuous(breaks = seq(0, 360, 45)) +
#       theme_linedraw() +
#       theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) # NOTE: ylim used in export
# 
# # caption = "Pittsburgh International Airport" + 
# # caption = "Heinz Field" + 


## NEWer METHOD - with speed binning - even newer, with month separation
## sort data:
for (k in 1:12) {
  x <- pit %>%
    filter(month == k)
  d <- x$dir
  s <- x$spd
  #speed.bins <- 7 # this used for monthly data at HFP
  # speed.bins <- ceiling(max(s)) # HFP will work with this binning.
  speed.bins <- 6 # PIT needs this binning
  wind <- array(0, dim = c(36,speed.bins))
  for (i in 1:(length(s))) {
    #speed.index <- ceiling(s[i]) # HFP will work with this binning.
    ## for use when speed categories exceed colormap (>9)
    if (s[i] <= 2) {
      speed.index <- 1
    } else if (s[i] <= 4) {
      speed.index <- 2
    } else if (s[i] <= 6) {
      speed.index <- 3
    } else if (s[i] <= 8) {
      speed.index <- 4
    } else if (s[i] <= 10) {
      speed.index <- 5
    } else {
      speed.index <- 6
    }
    wind[ceiling(d[i]/10),speed.index] <- wind[ceiling(d[i]/10),speed.index] + 1
  }
  ## Now, form long array rather than wide:
  wind.long <- array(NA, dim = 36*speed.bins)
  for (i in 1:speed.bins) {
    for (j in 1:36) {
      wind.long[(36*(i-1))+j] <- wind[j,i]
    }
  }
  speeds <- c(rep("0-2",36), rep("2-4",36), rep("4-6",36), rep("6-8",36), rep("8-10",36), rep("above 10",36)) # be sure to fill in as many as the wind bins in "wind" allocation
  #speeds <- c(rep("0-1",36), rep("1-2",36), rep("2-3",36), rep("3-4",36), rep("4-5",36), rep("5-6",36), rep("6-7",36)) # for HFP
  directions <- rep(5+10*(c(0:35)), speed.bins)
  rose <- data.frame(directions, speeds, wind.long)
  
  windHFP <- ggplot(rose, aes(fill = fct_rev(speeds), x = directions, y = wind.long)) + 
    labs(caption = paste("month = ", k, sep = "")) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_brewer("Speed (m/s)", palette = "Blues") +
    coord_polar(theta = "x", start = 0) +
    scale_x_continuous(breaks = seq(0, 360, 45)) +
    ylim(0,15) + 
    theme_linedraw() +
    theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) # NOTE: ylim used in export
  
  # caption = "Pittsburgh International Airport" + 
  # caption = "Heinz Field" + 
  
  ## to save immediately:
  ggsave(paste("windPIT", k, ".png", sep = ""), plot = windHFP, device = "png")
  
}
