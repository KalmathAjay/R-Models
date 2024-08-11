# Develop a program to read a csv file and analyze the data in the file in R

data <- read.csv("/cloud/project/workingdir/Data/people_scores.csv") # Read the CSV file

cat("Structure of the data frame:\n")
str(data)

cat("Summary statistics of numeric columns:\n")
summary(data)

mean_age <- mean(data$Age) # Calculate the mean age and score
mean_score <- mean(data$Score)

print(paste("Mean Age:", mean_age))
print(paste("Mean Score:", mean_score))

########################################################################

# Implement a program to find a list of even numbers from 1 to n using R-Loops.

find_even_numbers <- function(n) {
  even_numbers <- vector("numeric")
  
  for (i in 1:n) {
    if (i %% 2 == 0) {
      even_numbers <- c(even_numbers, i)
    }
  }
  
  return(even_numbers)
}

# Example usage:
n <- 20
even_numbers <- find_even_numbers(n)
print(even_numbers)

########################################################################

#Develop a program to join columns and rows in a data frame using cbind() and rbind() in R. 

# Create two data frames
df1 <- data.frame(A = c(1, 2, 3),
                  B = c(4, 5, 6))

df2 <- data.frame(A = c(7, 8, 9),
                  B = c(10, 11, 12))

# Print the original data frames
print("Original data frames:")
print(df1)
print(df2)

combined_df_columns <- cbind(df1, df2) # Use cbind() to join columns
print("Combined data frames using cbind():")
print(combined_df_columns)

combined_df_rows <- rbind(df1, df2) # Use rbind() to join rows
print("Combined data frames using rbind():")
print(combined_df_rows)

########################################################################

# Vector Operations
# Creating
numeric_vector <- c(1, 2, 3, 4, 5)
character_vector <- c("apple", "banana", "orange")

# Accessing
print(paste("First Element:", numeric_vector[1])) # Accessing elements by index
subset_vector <- numeric_vector[c(2, 4)] # Accessing multiple elements

# Updating
numeric_vector[3] <- 10 # Updating an element by index
numeric_vector[c(1, 4)] <- c(100, 200) # Updating multiple elements

# Appending
new_vector <- c(numeric_vector, 6) # Appending elements to the end
updated_vector <- append(numeric_vector, c(7, 8), after = 3) # Adding elements at specific positions

# Deleting Elements
shortened_vector <- numeric_vector[-3] # Removing elements by index
filtered_vector <- numeric_vector[numeric_vector != 3] # Removing specific values

# List Operations
# Creation
my_list <- list(name = "John", age = 30, scores = c(85, 90, 78)) # Creating a list

# Accessing Elements
name_value <- my_list$name # Accessing elements by name
age_value <- my_list[[2]] # Accessing elements by index
second_score <- my_list$scores[2] # Accessing nested elements

# Updating Elements
my_list$name <- "Alice" # Updating elements by name
my_list[[3]][1] <- 95 # Updating elements by index

# Adding Elements
my_list$grade <- "A" # Adding a new element
my_list <- append(my_list, list(city = "New York"), after = 2) # Adding elements at specific positions

# Deleting Elements
my_list$grade <- NULL # Removing elements by name
my_list[[4]] <- NULL # Removing elements by index

########################################################################

# Implementation and perform the various operations on data frames in R

# Creating a data frame
df <- data.frame(
  Name = c("John", "Alice", "Bob", "Emily"),
  Age = c(25, 30, 28, 35),
  Score = c(85, 92, 78, 90)
)

# Accessing elements
names <- df$Name # By column name
ages <- df$Age

first_row_first_col <- df[1, 1] # By row and column indices

# Adding elements
new_row <- data.frame(Name = "Sarah", Age = 32, Score = 88)# Adding a new row
df <- rbind(df, new_row)

new_column <- c(70, 80, 90, 85, 95) # Adding a new column
df$New_Score <- new_column

# Updating elements
df[2, 2] <- 33 # By row and column indices
df$Score <- df$Score + 5 # Updating entire column

# Removing elements
df <- df[-2, ] # Removing a row
df <- df[, -4] # Removing a column

# Filtering data
subset_df <- df[df$Age > 30, ] # Filtering based on condition
subset_df <- subset(df, Age > 30) # Filtering using subset function

# Sorting data
sorted_df <- df[order(df$Score), ] # Sorting by a column

# Summary statistics
summary_stats <- summary(df$Score) # Summary statistics of numeric columns

summary_stats

########################################################################

# Study and implementation of various control structures in R

# Conditional Statements (if, else, else if)
x <- 10
if (x > 5) {  # Example of if statement
  print("x is greater than 5")
}


y <- 20
if (y %% 2 == 0) {  # Example of if-else statement
  print("y is even")
} else {
  print("y is odd")
}


z <- 15
if (z > 10) {  # Example of if-else if-else statement
  print("z is greater than 10")
} else if (z == 10) {
  print("z is equal to 10")
} else {
  print("z is less than 10")
}

# Looping Constructs (for, while, repeat, break, next)
for (i in 1:5) { # for loop
  print(i)
}


j <- 1
while (j <= 5) {  # while loop
  print(j)
  j <- j + 1
}


k <- 1
repeat {  # repeat loop with break statement
  print(k)
  k <- k + 1
  if (k > 5) {
    break
  }
}


for (m in 1:5) { # Next statement to skip iteration
  if (m == 3) {
    next
  }
  print(m)
}

# Functions (function)
calculate_sum <- function(x, y) { # Defining a function
  return(x + y)
}

print(calculate_sum(5, 3))  # Calling a function

########################################################################

# Study and implementation of data transpose operations in R

matrix_data <- matrix(1:12, nrow = 3, ncol = 4) # Create a sample matrix

print("Original Matrix:")
print(matrix_data) # Print original matrix

transposed_matrix <- t(matrix_data) # Transpose the matrix

print("Transposed Matrix:")
print(transposed_matrix) # Print transposed matrix


data_frame <- data.frame(  # Create a sample data frame
  Name = c("John", "Alice", "Bob"),
  Age = c(25, 30, 28),
  Score = c(85, 92, 78)
)

print("Original Data Frame:")
print(data_frame) # Print original data frame

transposed_data_frame <- as.data.frame(t(data_frame)) # Transpose the data frame

print("Transposed Data Frame:")
print(transposed_data_frame) # Print transposed data frame

########################################################################

# Implement Linear and logistic Regression 

# Generate some sample data
set.seed(123)
x <- 1:100
y <- 2 * x + rnorm(100, mean = 0, sd = 10)

linear_model <- lm(y ~ x) # Fit linear regression model

summary(linear_model) # Print model summary

# Plot the data and regression line
plot(x, y)
abline(linear_model, col = "red")


# Load necessary libraries
library(ggplot2)

# Generate sample data for logistic regression
set.seed(123)
x <- 1:100
p <- 1 / (1 + exp(-(0.1 * x - 5)))
y <- rbinom(100, 1, p)


logistic_model <- glm(y ~ x, family = binomial) # Fit logistic regression model


summary(logistic_model) # Print model summary


data <- data.frame(x, y)
ggplot(data, aes(x, y)) + geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) # Plot the data and logistic regression curve

########################################################################

# Study and implementation of Data Visualization with ggplot2

# Load necessary library
library(ggplot2)

set.seed(123) 
scatter_data <- data.frame(x = rnorm(100), y = rnorm(100)) # Create sample data for scatter plot
line_data <- data.frame(x = 1:10, y = 1:10) # Create sample data for line plot
bar_data <- data.frame(category = letters[1:5], value = 1:5) # Create sample data for bar plot
histogram_data <- data.frame(x = rnorm(100)) # Create sample data for histogram
boxplot_data <- data.frame(group = rep(letters[1:3], each = 50), value = rnorm(150)) # Create sample data for box plot

# Scatter plot
ggplot(scatter_data, aes(x = x, y = y)) +
  geom_point() +
  ggtitle("Scatter Plot")

# Line plot
ggplot(line_data, aes(x = x, y = y)) +
  geom_line(color = "red") +
  ggtitle("Line Plot")

# Bar plot
ggplot(bar_data, aes(x = category, y = value)) +
  geom_bar(stat = "identity", fill = "green") +
  ggtitle("Bar Plot")

# Histogram
ggplot(histogram_data, aes(x = x)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  ggtitle("Histogram")

# Box plot
ggplot(boxplot_data, aes(x = group, y = value)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Box Plot")

########################################################################

# Create a data set and do statistical analysis on the data using R

# Generate sample data
set.seed(123)
height <- rnorm(100, mean = 170, sd = 10) # Heights in cm
weight <- rnorm(100, mean = 70, sd = 8)   # Weights in kg

# Create a dataframe
data <- data.frame(height = height, weight = weight)

# Display the first few rows of the dataset
head(data)

# Summary statistics
summary(data)

# Correlation between height and weight
cor(data$height, data$weight)

# Scatter plot of height vs weight
plot(data$height, data$weight, 
     xlab = "Height (cm)", ylab = "Weight (kg)",
     main = "Scatter Plot of Height vs Weight")

# Fit a linear regression model
linear_model <- lm(weight ~ height, data = data)
summary(linear_model)
