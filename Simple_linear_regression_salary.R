# Simple Linear Regression

dataset = read.csv('/cloud/project/workingdir/Data/Salary_Data.csv') # Importing the dataset

# install.packages('caTools')
library(caTools)
library(ggplot2)

# Splitting the dataset into the Training set and Test set
set.seed(150)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling - using the mean and sd from the training set
train_mean = colMeans(training_set)
train_sd = apply(training_set, 2, sd)

training_set_scaled = scale(training_set, center = train_mean, scale = train_sd)
test_set_scaled = scale(test_set, center = train_mean, scale = train_sd)

# Convert scaled matrices back to data frames
training_set_scaled = as.data.frame(training_set_scaled)
test_set_scaled = as.data.frame(test_set_scaled)

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set_scaled)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set_scaled)

# Calculate accuracy metrics
mae = mean(abs(test_set_scaled$Salary - y_pred)) # Mean Absolute Error
mse = mean((test_set_scaled$Salary - y_pred)^2)  # Mean Squared Error
rmse = sqrt(mse)                                 # Root Mean Squared Error
r_squared = 1 - (sum((test_set_scaled$Salary - y_pred)^2) / sum((test_set_scaled$Salary - mean(test_set_scaled$Salary))^2))




# Visualising the Training set results
ggplot() +
  geom_point(aes(x = training_set_scaled$YearsExperience, y = training_set_scaled$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set_scaled$YearsExperience, y = predict(regressor, newdata = training_set_scaled)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') + xlab('Years of experience') + ylab('Salary')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = test_set_scaled$YearsExperience, y = test_set_scaled$Salary),
             colour = 'red') +
  geom_line(aes(x = test_set_scaled$YearsExperience, y = y_pred),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') + xlab('Years of experience') + ylab('Salary')
