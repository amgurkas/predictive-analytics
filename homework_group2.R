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
library(gridExtra)
library(GGally)
library(caret)
library(stats)

################################## KJ 3.1 ######################################
# Problem Introduction
# The UC Irvine Machine Learning Repository6 contains a data set related
# to glass identification. The data consist of 214 glass samples labeled as one
# of seven class categories. There are nine predictors, including the refractive
# index and percentages of eight elements: Na, Mg, Al, Si, K, Ca, Ba, and Fe.
# The data can be accessed via:
data(Glass)
str(Glass)

# KJ 3.1 (a)
# Using visualizations, explore the predictor variables to understand their
# distributions as well as the relationships between predictors

# Approach KJ 3.1 (a)
# (1) Plot the data to see the distribution using a histogram. This can be done
# by using the ggplot facet_wrap() argument, or by creating individual plots.

# (2) Alternatively, the data can be visualized using the ggpairs() function
# from the GGally package. This provides a matrix of containing density plots,
# scatterplots, Pearson correlations, and boxplots among indicators.

glass2 <- Glass %>% 
  select(-Type) %>% 
  pivot_longer(everything(), names_to = "indicator", values_to = "value")

# facet wrapped indicators
ggplot(glass2, aes(value)) +
  geom_histogram(bins = 70, fill = "steelblue", color = "black") +
  facet_wrap(~indicator)

# density plots - doesn't show anything b/c values are not spread similarly
ggplot(glass2) +
  geom_density(aes(x=value,fill=indicator)) +
  labs(
    x="Value",
    y="Density"
  )
  ggtitle("Density Plots of Indicators") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

# Individual Indicators
plot_glass_distributions <- function(data) {
  numeric_data <- data[sapply(data, is.numeric)]
  
  for (colname in names(numeric_data)) {
    p <- ggplot(data, aes_string(x = colname)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      theme_minimal() +
      labs(title = paste("Distribution of", colname),
           x = colname,
           y = "Frequency")
    print(p)  # Print each plot individually
    readline(prompt = "Press [Enter] to show the next plot...")
  }
}

# running function, need to save each ggplot so that it can go into the word doc
# plots should be included in the Appendix.
plot_glass_distributions(Glass)

# plotting the indicators using the ggpairs() function
Glass %>% 
  select(-Type) %>% 
GGally::ggpairs()

# From these plots, it is apparent that not all of the predictors are normally 
# distributed, and that some have outliers, such as Ba, Al, and Na. Additionally,
# from the scatterplot matrix produced by the ggpairs() function, it is 
# apparent that certain predictors have stronger relationships than others. 

# KJ 3.1 (b) 
# Do there appear to be any outliers in the data? Are any predictors skewed?

# Approach KJ 3.1 (b)
# Outliers can be detected through visualizations like boxplots. By plotting the
# outliers in red, it is apparent how many there are, and if the data may need to
# be transformed. 

ggplot(glass2, aes(x = "", y = value)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  facet_wrap(~ indicator, scales = "free_y") +
  theme_minimal() +
  labs(title = "Boxplots of Predictors",
       y = "Value",
       x = "") +
  theme(strip.text = element_text(size = 8))

# From this plot, it is apparent that there are outliers in nearly all of the 
# predictors except for Mg. To handle skewnewss, the data can be transformed
# through Log or Box-Cox transformations. 

# KJ 3.1 (c) 
# Are there any relevant transformations of one or more predictors that might 
# improve the classification model?

# Approach KJ 3.1 (c)
# There are a few transformations or steps that can be taken to improve the 
# classification model: 
# Data Cleaning: Check missing values (Removal versus imputation)
# Data Transformation: Center the data or scale it 
# Data Transformation: For features that are skewed, can perform Log or Box-Cox 
# transformations to normalize distributions.
# Identify Problematic Predictors: Filter data for near-zero variance predictors
# (these are predictors where most of the values are the same), and highly 
# correlated predictors that may lead to collinearity issues and increase model 
# variance. 

# Check for missing values
na_counts <- map_dfc(Glass, ~ sum(is.na(.x)))
names(na_counts) <- paste0("NA_", names(Glass))

distinct_counts <- map_dfc(Glass, ~ n_distinct(.x, na.rm = TRUE))
names(distinct_counts) <- paste0("T_", names(Glass))

sum_missing <- bind_cols(na_counts, distinct_counts)

# In the summary of missing values dataframe, we can see that no indicators have
# missing values, which means imputations are not necessary. 

# Data Transformation: Centering and Scaling Data
# Note: cannot use the glass2 version because the PreProcess() function is 
# intended for wide datasets. When typing trans into the console, we can see that 
# nine indicators were center and scaled, and that one indicator was ignored. 
trans <- preProcess(Glass, method=c("center","scale"))

# Can add in the Box-Cox method as well by adding it into the method arugment
trans <- preProcess(Glass, method=c("center", "scale", "BoxCox"))

# To identify problematic predictors we can filter data for near-zero variance 
# predictors (these are predictors where most of the values are the same), and 
# highly correlated predictors that may lead to collinearity issues and increase 
# model variance. 

# if there are any near-zero variance predictors, they would be captured by the
# nearZeroVar function from the caret package.
nearZeroVar(Glass)

# for highly correlated indicators, the cor() function from the caret package 
# can be used. 
Glass <- Glass %>% 
  select(-Type)
corr <- stats::cor(Glass)
dim(corr)

################################### KJ 3.2 #####################################
# 3.2. The soybean data can also be found at the UC Irvine Machine Learning
# Repository. Data were collected to predict disease in 683 soybeans. The 35
# predictors are mostly categorical and include information on the environmental
# conditions (e.g., temperature, precipitation) and plant conditions (e.g., left 
# spots, mold growth). The outcome labels consist of 19 distinct classes.

data(Soybean)

#   (a) Investigate the frequency distributions for the categorical predictors. Are
# any of the distributions degenerate in the ways discussed earlier in this
# chapter?

nearZeroVar(Soybean)
colnames(Soybean[,c(19,26,28)])

ggplot(Soybean, aes(Class)) +
  geom_histogram(stat="count", fill = "steelblue", color = "black")

# The "leaf.mild", "mycelium", and "seclerotia" have near zero variance. 

# need to pivot ?? looking for categorical variables ??

ggplot()

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