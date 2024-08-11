library(datasets)
data(iris)

View(iris) # View the data

# head() / tail()
head(iris, 5)
tail(iris, 5)

summary(iris)
summary(iris$Sepal.Length)

# Check to see if there are missing data?
sum(is.na(iris))

# install.packages("skimr")

library(skimr)

skim(iris) # Perform skim to display summary statistics

# Group data by Species then perform skim
iris %>% 
  dplyr::group_by(Species) %>% 
  skim() 


# Panel plots
plot(iris, col = "green")

# Scatter plot
plot(iris$Sepal.Width, iris$Sepal.Length, col = "red",     # Makes red circles + Adds x and y axis labels
     xlab = "Sepal width", ylab = "Sepal length")

# Histogram
hist(iris$Sepal.Width, col = "blue")   # Makes blue bars

# Feature plots
# https://www.machinelearningplus.com/machine-learning/caret-package/
featurePlot(x = iris[,1:4], 
            y = iris$Species, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

library(caret)

# To achieve reproducible model; set the random seed number
set.seed(150)

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set

# SVM model (polynomial kernel)

# Build Training model
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model
Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance, col = "red")
