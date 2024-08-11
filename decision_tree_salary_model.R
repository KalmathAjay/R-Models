# Decision Tree Regression

# install.packages('rpart')
library(rpart)
# install.packages('caTools')
library(caTools)
# install.packages('ggplot2')
library(ggplot2)
# install.packages('Metrics')
library(Metrics)

dataset = read.csv('/cloud/project/workingdir/Data/Position_Salaries.csv')# Importing the dataset
dataset = dataset[2:3]

# Splitting the dataset into the Training set and Test set
set.seed(150)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling (Not typically needed for decision trees)
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Decision Tree Regression to the dataset
regressor = rpart(formula = Salary ~ ., 
                  data = training_set, 
                  control = rpart.control(minsplit = 1))

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Calculate accuracy metrics
rmse_value = rmse(test_set$Salary, y_pred)
mae_value = mae(test_set$Salary, y_pred)
mse_value = mse(test_set$Salary, y_pred)
r2_value = 1 - (sum((test_set$Salary - y_pred) ^ 2) / sum((test_set$Salary - mean(test_set$Salary)) ^ 2))

# Print accuracy metrics
cat("RMSE: ", rmse_value, "\n")
cat("MAE: ", mae_value, "\n")
cat("MSE: ", mse_value, "\n")
cat("R²: ", r2_value, "\n")



# Visualising the Decision Tree Regression results (higher resolution)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')

# Plotting the tree
plot(regressor)
text(regressor)