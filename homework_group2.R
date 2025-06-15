################################ Introduction #################################

###############################################################################

# Code produced by Alyssa Gurkas
# Data 624 - Predictive Analytics 
# 6/15/2025
# HW Problems KJ 3.1, 3.2 & HA 7.5, 7.6 (7.5 and 7.6 relate to each other and not 7.10) 

################################ Load Libraries ################################
library(fpp2)
library(tidyverse)
library(officer)
library(mlbench)

################################## KJ 3.1 ######################################
# The UC Irvine Machine Learning Repository6 contains a data set related
# to glass identification. The data consist of 214 glass samples labeled as one
# of seven class categories. There are nine predictors, including the refractive
# index and percentages of eight elements: Na, Mg, Al, Si, K, Ca, Ba, and Fe.
# The data can be accessed via:
data(Glass)
str(Glass)

# (a) Using visualizations, explore the predictor variables to understand their
# distributions as well as the relationships between predictors.

ggplot(iris2, aes(value)) +
  geom_histogram() +
  facet_wrap(~indicator)

# (b) Do there appear to be any outliers in the data? Are any predictors skewed?
# (c) Are there any relevant transformations of one or more predictors that
# might improve the classification model?



################################### KJ 3.2 #####################################
# 3.2. The soybean data can also be found at the UC Irvine Machine Learning
# Repository. Data were collected to predict disease in 683 soybeans. The 35
# predictors are mostly categorical and include information on the environmental
# conditions (e.g., temperature, precipitation) and plant conditions (e.g., left 
# spots, mold growth). The outcome labels consist of 19 distinct classes.

# The data can be loaded via:
# > library(mlbench)
# > data(Soybean)
# > ## See ?Soybean for details
#   (a) Investigate the frequency distributions for the categorical predictors. Are
# any of the distributions degenerate in the ways discussed earlier in this
# chapter?
#   (b) Roughly 18% of the data are missing. Are there particular predictors that
# are more likely to be missing? Is the pattern of missing data related to
# the classes?
#   (c) Develop a strategy for handling missing data, either by eliminating
# predictors or imputation.

###############################################################################
# HA 7.5
###############################################################################

# Data set books contains the daily sales of paperback and hardcover books at the 
# same store. The task is to forecast the next four days’ sales for paperback and 
# hardcover books.
# 
# Plot the series and discuss the main features of the data.
# Use the ses() function to forecast each series, and plot the forecasts.
# Compute the RMSE values for the training data in each case.

###############################################################################
# HA 7.6
###############################################################################

# We will continue with the daily sales of paperback and hardcover books in data 
# set books.
# 
# Apply Holt’s linear method to the paperback and hardback series and compute 
# four-day forecasts in each case.Compare the RMSE measures of Holt’s method for 
# the two series to those of simple exponential smoothing in the previous question. 
# (Remember that Holt’s method is using one more parameter than SES.) 
# 
# Discuss the merits of the two forecasting methods for these data sets. 
# Compare the forecasts for the two series using both methods. 
# Which do you think is best?
# 
# Calculate a 95% prediction interval for the first forecast for each series, 
# using the RMSE values and assuming normal errors. Compare your intervals with 
# those produced using ses and holt.