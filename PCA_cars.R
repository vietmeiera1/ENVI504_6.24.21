# Prinicpal Commone Analysis example
# https://www.datacamp.com/community/tutorials/pca-analysis-r 
library(dplyr)
library(tidyr)
library(devtools)
install_github("vqv/ggbiplot") #select 1 and yes
library(ggbiplot)

#this example uses the built-in dataset, mtcars, without the vargorical variable
data <- select(mtcars, -vs, -am)
head(data)
pca2 <- prcomp(data, center = FALSE, scale. = FALSE)
summary(pca2)
ggbiplot(pca2)
pca1 <- prcomp(data, center = TRUE, scale. = TRUE)
summary(pca1)
ggbiplot(pca1, labels = rownames(data))
country <- c(rep("Japan", 3), rep("US", 4), rep("Europe", 7), 
             rep("US", 3), "Europe", rep("Japan", 3),
             rep("US", 4), rep("Europe", 3), "US",
             rep("Europe", 3))
ggbiplot(pca1, ellipse = TRUE, labels = rownames(data), groups = country)
