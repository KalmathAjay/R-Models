# Random Forest Regression

# install.packages('randomForest')
# install.packages('caTools')
# install.packages('ggplot2')

library(randomForest)
library(caTools)
library(ggplot2)

# Importing the dataset
dataset = read.csv('/cloud/project/workingdir/Data/Position_Salaries.csv')
dataset = dataset[2:3]

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Regression to the training set
set.seed(150)
regressor = randomForest(x = training_set[-2],
                         y = training_set$Salary,
                         ntree = 500)

# Predicting results on the test set
y_pred = predict(regressor, newdata = test_set[-2])

# Calculate R², MAE, and MSE
rmse = sqrt(mean((y_pred - test_set$Salary)^2))
print(paste("RMSE:", round(rmse, 2)))

# R-squared
ss_total = sum((test_set$Salary - mean(test_set$Salary))^2)
ss_residual = sum((test_set$Salary - y_pred)^2)
r_squared = 1 - (ss_residual / ss_total)

# Mean Absolute Error (MAE)
mae = mean(abs(y_pred - test_set$Salary))

# Mean Squared Error (MSE)
mse = mean((y_pred - test_set$Salary)^2)

# Print the metrics
print(paste("R-squared:", round(r_squared, 4)))
print(paste("Mean Absolute Error (MAE):", round(mae, 2)))
print(paste("Mean Squared Error (MSE):", round(mse, 2)))


# Predicting a new result with Random Forest Regression
y_new_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the Random Forest Regression results (higher resolution)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Random Forest Regression)') +
  xlab('Level') +
  ylab('Salary')

# Print prediction for Level 6.5
print(paste("Predicted Salary for Level 6.5:", round(y_new_pred, 2)))
