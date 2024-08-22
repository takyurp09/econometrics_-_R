# Introduction to R
  
# Outline:
#   Setting up data
#   Exploring data
#   Editing data
#   Estimating regressions
#   Exporting regression output

# Introduction to R

# Data files: 
#   wage1.csv

# Setting up data ---------------------------------------------------------
rm(list = ls()) # Clear workspace

# Update and install packages
# update.packages()
# install.packages("tidyverse")
# install.packages("magrittr")
# install.packages("stargazer")

# Load necessary packages
library(tidyverse)
library(magrittr)
library(stargazer)

# Set working directory
directory <- "/Users/taky/econometrics_and_r/class1_introduction_to_r/"
getwd()
# setwd("/Users/taky/econometrics_and_r/class1_introduction_to_r")

# Load dataset
data <- read.csv(paste0(directory, "wage1.csv"))

# Exploring data ----------------------------------------------------------

# Display structure of data
str(data) 
data %>% select(wage, educ, exper) %>% str

# Preview first 10 rows of selected columns
data %>% select(wage, educ, exper) %>% head(10)

# Generate summary statistics
data %>% stargazer(type = "text")
data %>% select(wage, educ, exper) %>% stargazer(type = "text")

# Summary statistics by group (female)
data %>% select(female) %>% table
data %>% filter(female == 1) %>% stargazer(type = "text")
split(data, data$female) %>% walk(~ stargazer(., type = "text"))

# Editing data ------------------------------------------------------------

# Select and filter variables
# demonstrate %>%  versus %<>% 

data %<>%  select( wage, educ, exper, tenure, female, south, west)
data %<>% select(-tenure)
data %<>% filter(wage < 2)

# Create new variables
data %<>% mutate(logwage = log(wage), 
                 educsq = educ^2, 
                 southwest = south + west)
summary(data)
# Estimating regressions --------------------------------------------------

# Estimate simple regression model
linear.model.1 <- lm(wage ~ educ, data)
summary(linear.model.1)

# Export regression output as HTML
stargazer(linear.model.1, type = "html", out = "regtable.html")

# Estimate multiple regression model
linear.model.2 <- lm(wage ~ educ + exper, data)
summary(linear.model.2)

# Export regression outputs as Word document
stargazer(linear.model.1, linear.model.2,
          type = "html",
          out = "regtable.doc")


