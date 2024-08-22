# Econometrics and Economic Data

# Outline:
#   Cross-sectional data
#   Time series data
#   Pooled cross sections
#   Panel/longitudinal data

# Data files:
#   wage1.csv
#   prminwge.csv
#   hprice3.csv
#   wagepan.csv

# setup
rm(list = ls()) # clear the workspace
directory <- "/Users/taky/econometrics_and_r/class2_Econometrics and Economic Data/"


# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "moments")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Cross-sectional data -------------------------------------------------

# Cross-sectional data set on wages and other characteristics
# Observations are individuals

# Load the dataset
wage <- read.csv(paste0(directory, "wage1.csv"))

# Summarize the data
stargazer(wage, type = "text")

# Summarize the data for selected variables
wage %>% 
  select(wage, lwage, educ, exper, expersq, tenure, married, female) %>%
  stargazer(type = "text")
# `%>%` is the pipe operator that forwards lhs to rhs. 

# Keep only selected variables in the dataset
wage %<>% select(wage, lwage, educ, exper, expersq, tenure, married, female)
# 'lhs %<>% rhs' is equivalent to 'lhs <- lhs %>% rhs'.

# Summarize the data
str(wage)
stargazer(wage, type = "text")
head(wage, 10)

# Frequency tables
table(wage$female)


# Time series data -----------------------------------------------------

# Time series data on minimum wages and related variables

# Load the dataset and keep only select variables
prminwge <- read.csv(paste0(directory, "prminwge.csv"))
prminwge %<>% select(year, avgmin, avgcov, prunemp, prgnp)

# Describe and summarize data
str(prminwge)
stargazer(prminwge, type = "text")
table(prminwge$year)


# Pooled cross sections ------------------------------------------------
# Pooled cross sections on housing prices
# House prices for two years, 1978 and 1981 
# Houses are different in each year. 
# We do not have the price of the same house in both years. 

# Load the dataset and keep only select variables
hprice3 <- read.csv(paste0(directory, "hprice3.csv"))
hprice3 %<>% select(year, y81, price, lprice, rooms, baths)

# Describe and summarize the data
str(hprice3)
stargazer(hprice3, type = "text")
head(hprice3, 10)
table(hprice3$year)

# Summary statistics for price and year
hprice3 %>% 
  select(price, year) %>%
  stargazer(type = "text")

# Summary statistics for selected sample
# Before is year 1978, see average price before
hprice3 %>% 
  filter(year == 1978) %>%
  select(price) %>% 
  stargazer(type = "text")

# After is year 1981, see average price after
hprice3 %>% 
  filter(year == 1981) %>%
  select(price) %>% 
  stargazer(type = "text")


# Panel/longitudinal data ----------------------------------------------

# Panel data for wages of individuals across years 
# 8-year panel data on wages of individuals and other variables
# there are observations for the same individual for multiple years
# person identifier is nr
# time identifier is year

wagepan <- read.csv(paste0(directory, "wagepan.csv"))

wagepan %<>% select(nr, year, lwage, exper, educ, hours)

str(wagepan)
stargazer(wagepan, type = "text")
head(wagepan, 10)
table(wagepan$year)

