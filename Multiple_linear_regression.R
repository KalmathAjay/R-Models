# Multiple Linear Regression

# install.packages('caTools')
library(caTools)

dataset = read.csv('/cloud/project/workingdir/Data/50_Startups.csv') # Importing the dataset

# Encoding categorical data
dataset$State = factor(dataset$State,levels = c('New York', 'California', 'Florida'))

# Splitting the dataset into the Training set and Test set
set.seed(150)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Inspect the structure of the training seta
str(training_set)

# Feature Scaling - scaling only numeric features
numeric_features = c('R.DSpend', 'Administration', 'MarketingSpend')
training_set[numeric_features] = scale(training_set[numeric_features])
test_set[numeric_features] = scale(test_set[numeric_features])

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Calculate accuracy metrics
mae = mean(abs(test_set$Profit - y_pred)) # Mean Absolute Error
mse = mean((test_set$Profit - y_pred)^2)  # Mean Squared Error
rmse = sqrt(mse)                           # Root Mean Squared Error
r_squared = 1 - (sum((test_set$Profit - y_pred)^2) / sum((test_set$Profit - mean(test_set$Profit))^2))

# Summary of the model
summary(regressor)
