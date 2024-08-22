# Simple Regression Model

# Outline:
#   Simple regression
#   Prediction after regression
#   Goodness-of-fit measure (R-squared)
#   Log forms: log-log and log-linear forms

# Data files:
#   CEOSAL1.csv
#   wage1.csv

# Setup -----------------------------------------------------------------
# Clear the workspace
rm(list = ls()) 

# Set the directory path where data files are stored
directory <- "/Users/taky/econometrics_and_r/class3_Simple_Regression_Model/"

# Install and load necessary packages -----------------------------------
PackageNames <- c("tidyverse", "stargazer", "magrittr")
for(i in PackageNames){
    if(!require(i, character.only = TRUE)){
        install.packages(i, dependencies = TRUE)
        require(i, character.only = TRUE)
    }
}

# Simple Regression -----------------------------------------------------

# CEO salary example 
# Load and prepare the CEOSAL1 dataset
CEOSAL1 <- read.csv(paste0(directory, "CEOSAL1.csv"))

# Select relevant variables: salary and roe
CEOSAL1 %<>% select(salary, roe)
str(CEOSAL1)  # Check the structure of the data
stargazer(CEOSAL1, type = "text")  # Display descriptive statistics
head(CEOSAL1, 10)  # Show the first 10 rows of the dataset

# Explore data by calculating the correlation between variables
cor(CEOSAL1)
# Add a new variable for the average salary
CEOSAL1 %<>% mutate(avg_salary = mean(salary))

# Perform a simple linear regression: salary = beta0 + beta1*roe + u
model_CEOSAL1 <- lm(formula = salary ~ roe, data = CEOSAL1)
summary(model_CEOSAL1)  # Display regression results
model_CEOSAL1$coefficients['roe']  # Extract the coefficient for roe

# Plot the observations with a fitted regression line

plot(x = CEOSAL1$roe, y = CEOSAL1$salary)

abline(a = model_CEOSAL1$coefficients['(Intercept)'], 
       b = model_CEOSAL1$coefficients['roe'],
       col = 'red')

# Alternatively, use ggplot2 for a more customizable plot
ggplot(data = CEOSAL1, mapping = aes(x = roe, y = salary)) +
    theme_bw() +  # Set the theme of the plot
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

# Wage example
# Load and prepare the wage1 dataset
wage1 <- read.csv(paste0(directory, "wage1.csv"))
wage1 %<>% select(wage, educ)
str(wage1)  # Check the structure of the data
stargazer(wage1, type = "text")  # Display descriptive statistics
head(wage1, 10)  # Show the first 10 rows of the dataset

# Perform a simple linear regression: wage = beta0 + beta1*educ + u
model_wage1 <- lm(wage ~ educ, wage1)
summary(model_wage1)  # Display regression results

# Prediction after Regression ------------------------------------------

# CEO salary example
# Use the data 'CEOSAL1' and the model 'model_CEOSAL1'

# Predicted values for the dependent variable (salaryhat)
CEOSAL1 %<>% mutate(salaryhat = fitted(model_CEOSAL1))
stargazer(CEOSAL1, type = "text")  # Display data with predicted values

# Plot actual salary and predicted salary values against roe
ggplot(data = CEOSAL1, mapping = aes(x = roe)) +
    geom_point(mapping = aes(y = salary, color = 'Salary - actual value')) +
    geom_point(mapping = aes(y = salaryhat, color = 'Salaryhat - predicted value')) + 
    xlab('Return on equity')

# Calculate and plot residuals (uhat = actual - predicted)
CEOSAL1 %<>% mutate(uhat = residuals(model_CEOSAL1))
stargazer(CEOSAL1, type = "text")  # Display data with residuals
ggplot(CEOSAL1, aes(x = roe)) +
    geom_point(aes(y = salary, col = 'Salary - actual value')) +
    geom_point(aes(y = uhat, col = 'Residual uhat')) +
    xlab('Return on equity')

# Graph Actual and Predicted Values and Residuals together
ggplot(CEOSAL1, aes(x = roe)) +
    geom_point(aes(y = salary, color = 'Salary - actual value')) +
    geom_point(aes(y = salaryhat, color = 'Salaryhat - predicted value')) +
    geom_point(aes(y = uhat, color = 'Residual uhat')) +
    geom_smooth(aes(y = salary, color = 'Fitted line'), 
                method = "lm", se = FALSE) +
    xlab('Return on equity')

# Wage example
# Predict wage using the model 'model_wage1'
wage1 %<>% mutate(wagehat = fitted(model_wage1),
                  uhat = resid(model_wage1))

# Plot actual wage, predicted wage, and residuals against educ
ggplot(wage1, aes(x = educ)) +
    geom_point(aes(y = wage, color = 'Wage - actual value')) +
    geom_point(aes(y = wagehat, color = 'Wagehat - predicted value')) +
    geom_point(aes(y = uhat, color = 'Residual uhat')) +
    geom_smooth(aes(y = wage, color = 'Fitted line'), 
                method = "lm", se = FALSE)


# Goodness-of-fit measure (R-squared) ----------------------------------

# CEO salary example 
# Use 'str' function to inspect model details
str(model_CEOSAL1)
str(summary(model_CEOSAL1))

# Extract R-squared value from the model summary
summary(model_CEOSAL1)$r.squared

# Number of observations used in the model
nobs(model_CEOSAL1)

# Wage example
# Inspect model details
str(model_wage1)
str(summary(model_wage1))

# Extract R-squared value
summary(model_wage1)$r.squared
# Number of observations used in the model
nobs(model_wage1)


# Log forms: log-log and log-linear forms -------------------------------

# CEO salary example
# Load and inspect the dataset with logged variables
CEOSAL2 <- read.csv(paste0(directory, "CEOSAL1.csv"))
CEOSAL2 %>% select(salary, lsalary, sales, lsales) %>% head(10)

# Linear form regression: salary ~ sales
model_CEOSAL2 <- lm(salary ~ sales, CEOSAL2)
summary(model_CEOSAL2)  # Display regression results

# Plot salary vs sales with a fitted line
ggplot(CEOSAL2, aes(x = sales, y = salary)) +
    theme_bw() +
    geom_point() +
    geom_smooth(method = lm, se = FALSE)

# Log-log form regression: log(salary) ~ log(sales)
model_CEOSAL3 <- lm(lsalary ~ lsales, CEOSAL2)
summary(model_CEOSAL3)  # Display regression results

# Plot log(salary) vs log(sales) with a fitted line
ggplot(CEOSAL2, aes(x = lsales, y = lsalary)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE)

# Linear-log form regression: salary ~ log(sales)
model_CEOSAL4 <- lm(salary ~ lsales, CEOSAL2)
summary(model_CEOSAL4)  # Display regression results

# Plot salary vs log(sales) with a fitted line
ggplot(CEOSAL2, aes(x = lsales, y = salary)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE)

# Log-linear form regression: log(salary) ~ sales
model_CEOSAL5 <- lm(lsalary ~ sales, CEOSAL2)
summary(model_CEOSAL5)  # Display regression results

# Plot log(salary) vs sales with a fitted line
ggplot(CEOSAL2, aes(x = sales, y = lsalary)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE)

# Wage Example 
# Load and inspect the dataset with logged variables
wage2 <- read.csv(paste0(directory, "wage1.csv"))
select(wage2, wage, lwage, educ) %>% head(10)

# Linear form regression: wage ~ educ
model_wage2 <- lm(wage ~ educ, wage2)
summary(model_wage2)  # Display regression results

# Plot wage vs educ with a fitted line
ggplot(wage2, aes(educ, wage)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE)

# Log-linear form regression: log(wage) ~ educ
model_wage3 <- lm(lwage ~ educ, wage2)
summary(model_wage3)  # Display regression results

# Plot log(wage) vs educ with a fitted line
ggplot(wage2, aes(educ, lwage)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE)

