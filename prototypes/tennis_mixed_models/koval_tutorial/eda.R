
library(ggplot2)
library(dplyr)


setwd("/Users/petertea/Documents/stat852_project")

project_data <- read.csv('Data2020.csv')

# |||| --- |||| --- |||| --- |||| 
# Exploratory Data Analysis #####
# |||| --- |||| --- |||| --- |||| 


project_data[!complete.cases(project_data),]

# Categorical Variable
ggplot(data = project_data, aes(x = Y, fill=X7))+
  geom_histogram(alpha = 0.5)

# GGpairs
library(GGally)
GGally::ggpairs(data = project_data %>%
            dplyr::select(Y:X6),
          upper = list(continuous = wrap("cor", size = 5, digits = 2)),
          title="") 


