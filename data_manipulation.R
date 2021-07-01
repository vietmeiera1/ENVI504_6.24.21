#R, 7/1/21 
# basic stats on Beitbridge data

library(readr)
library(dplyr) #part of tidyverse D-pyler
  #warning objects are masked, d/t same name as stat package "filter" & "lag"
  # can call appropriately through STATS::filter  
library(tidyr)
library(lubridate)

file <- file.choose() #Kahler not a huge fan of this feature, but can be useful
#stores in the variable file whatever you chose. When you run this script
# it will open your files and you pick which one you want

beit <- read_csv(file, skip = 7, n_max = 28)
# gives you information on the data
#option n_max = number of rows to be read in
#option skip = number of lines to skip, e.g. headers
# we picked 7 because in Excel this is where the headers start

head(beit) #preview first several lines of the data
tail(beit) #preview the last several lines of data. 

beit <- beit[1:28,]
#this will reassign the beit and tell it to read to only row 28, otherwise we end
# up with junk at the end 
# [up and down, left and right]

mean(beit$`Flow (cumec)`)
sd(beit$`Flow (cumec)`) #this is the sample or population standard deviation ~ sample

m <- max(beit$`Flow (cumec)`) #max saved as variable m
i <- which(beit$`Flow (cumec)` == m) # double equal sign is a test of equality 
#^^ This gives the location on excel where the max is located, skipping the first 7 lines, 
# line 8 is the headers, then down to 21. 

beit[which.max(beit$`Flow (cumec)`), ] #coma and space is important OR else it will look for a square
beit[which.min(beit$`Flow (cumec)`), ]


#Types of brackets:
  # paraenthesis = argument of a function , input for functions, including mathematics ( )
  # square brackets are exclusively location values [ ]
  # curly brackets are in a loop, indicate where to loop thru { }
      # for (i in 1:10) {
      #   loop commands
      # }

n = 9 #single equal sign is saying something is equal

#logical operators: 
  # == equal
  # < less than
  # <= less than or equal to
  # > greater than
  # => greater than or equal to 
  # != not equal


#Variables: 
single.variables <- 7 #convention is to keep short, so this is a bad name
vectors_or_lists <- c(5+6, single.variables, 1:3)
# c = stack this one after another 
first <- c(1:3)
second <- c(4:8)
third <- cbind(first, second)
fourth <- rbind(first, second)
df <- data.frame("col1" = c("A", "B", "C", "D", "e"), 
                 "col2" = vectors_or_lists,
                 "col3" = c(1:5))
#data frame = an array with rows & columns. Names of columns. Same # of observations for all variables, excluding histograms


#manipulate some data! 

beit.f <- select(beit, Year,  'Flow (cumec)') #slimmed down version of dataframe 
      #select works for columns
y2k <- filter(beit, Year == 2000) #look at year 2000, which is a row 
#y2k <- filter(beit, Year == "M") #look at chatacter string M, which is a row 
#beit.f <- mutate(beit.f, flow_lps = 1000*'Flow (cumec)') #single quotation is wrong syntax
beit.f <- mutate(beit.f, flow_lps = 1000*`Flow (cumec)`)  #way to add column = new way, very useful
  #Note these are tick marks NOT single quotation
beit.f$flow_lps <-beit.f$`Flow (cumec)`*1000 #way #2 to add a column = old way

# wide vs long:
beit.l <- beit %>%   # %>% = pipe operator. Pipe operator also shown as |. This means not done. 
  mutate(flow_lps = 1000*`Flow (cumec)`) %>% #add this column
  select(Year, `Level (m)`, flow_lps) %>% #These are the variable (values) that are being stuck in new column
  pivot_longer(cols = c(`Level (m)`, flow_lps), 
               names_to = "Variable", 
               values_to = "Value")

beit.w <- beit.l %>%
  pivot_wider(names_from = Variable,
              values_from = Value)

#computer prefers long, wide is easier for us as humans

# Dates & huge data sets 
file <- file.choose()
mutale <- read_csv(file)
head(mutale)
str(mutale)

mutale1 <- mutale %>%
  mutate(datetime = ymd_hm(paste(YEAR, MONT, DAYN, HOUR, MINU))) %>% #year month date _ hours minutes
  na_if(-9999) %>% #remove nonlogical numbers 
  na_if(-8888) %>%
  na_if(-7777) %>%
  select(-YEAR, -MONT, -DAYN, -HOUR, -MINU) %>% # - sign used to remove things we don't want 
  rename(precip_mm = PRCP, 
         airtemp_c = TEMP, 
         humid_p = RHMD, 
         solRad_Wm2 = SRAD,
         AirPres_kPa = APRS, 
         Wind_sp = WSPD, 
         WindDir = WDIR, 
         RivStage_m = RIVS, 
         watertemp = WTMP,
         cond_us_cm = COND, 
         Tubidity = TRBD)

#Luberdate is a slightly better date system than RStudio has built in. Jan 1st 1970 starting date
# Standard in computing. R lets you pick an origin, but Lubridate uses 1/1/70.

file <- file.choose()
disasters <- read_csv(file, skip = 6, col_names = TRUE, col_types = "ciifffffcfffffccfffccnnfcccciiiiiiiiiiinnnn")

events <- disasters %>%
  filter(Year <= 2013) %>% #only year before 2013
  group_by(Continent) %>% #sort by continent
  summarize(events = length(Continent)) #summarize the data

deaths <- disasters %>%
  group_by(Continent) %>%
  summarize(deaths = sum(`Total Deaths`, na.rm = TRUE))