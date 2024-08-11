# SVR

# install.packages('ggplot2')
library(ggplot2)
# install.packages('caTools')
library(caTools)
# install.packages('e1071')
library(e1071)

dataset = read.csv('/cloud/project/workingdir/Data/Position_Salaries.csv') # Importing the dataset
dataset = dataset[2:3]

# Splitting the dataset into the Training set and Test set
set.seed(150)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling-Scale Level and Salary, excluding Position
training_set[-c(1)] = scale(training_set[-c(1)])  
test_set[-c(1)] = scale(test_set[-c(1)])          

# Fitting SVR to the dataset
regressor = svm(formula = Salary ~ Level,
                data = training_set,
                type = 'eps-regression',
                kernel = 'radial')

# Predicting the test set results
y_pred_test = predict(regressor, newdata = test_set)

# Calculating MAE, MSE, RMSE, and R-squared
mae = mean(abs(y_pred_test - test_set$Salary))
mse = mean((y_pred_test - test_set$Salary)^2)
rmse = sqrt(mse)
r_squared = 1 - (sum((y_pred_test - test_set$Salary)^2) / sum((test_set$Salary - mean(test_set$Salary))^2))

# Print metrics
print(paste("Mean Absolute Error (MAE): ", round(mae, 2)))
print(paste("Mean Squared Error (MSE): ", round(mse, 2)))
print(paste("Root Mean Squared Error (RMSE): ", round(rmse, 2)))
print(paste("R-squared (R²): ", round(r_squared, 4)))

