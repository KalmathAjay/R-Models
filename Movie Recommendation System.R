library(recommenderlab)
library(ggplot2)                       
library(data.table)
library(reshape2)

# Read movie and rating data
movie_data <- read.csv("/cloud/project/Projects/Data/movies.csv", stringsAsFactors=FALSE)
rating_data <- read.csv("/cloud/project/Projects/Data/ratings.csv")

# Overview the summary 
summary(movie_data)
summary(rating_data)

# Convert to data.table
movie_data <- data.table(movie_data)
rating_data <- data.table(rating_data)

# Data Preprocessing: One-hot encoding for movie genres
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE) 
colnames(movie_genre2) <- c(1:10)

# List of genres
list_genre <- c("Action", "Adventure", "Animation", "Children","Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")

# Initialize genre matrix
genre_mat1 <- matrix(0, nrow(movie_genre2) + 1, 18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

# Populate genre matrix
for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) 
    genre_mat1[index+1,gen_col] <- 1
  }
}

# Convert to data frame and integer
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE)
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}

# Creating the search matrix
SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])

# Converting rating data into a matrix
ratingMatrix <- dcast(rating_data, userId ~ movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) # Remove userIds
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")

# Computing movie similarity
movie_similarity <- similarity(ratingMatrix[, 1:4], method ="cosine", which = "items")
# Visualizing movie similarity
image(as.matrix(movie_similarity), main = "Movies Similarity")

# Summarizing rating values
rating_values <- as.vector(ratingMatrix@data)
unique_ratings <- unique(rating_values)
rating_table <- table(rating_values)

# Most viewed movies visualization
movie_views <- colCounts(ratingMatrix) 
table_views <- data.frame(movie = names(movie_views), views = movie_views) 
table_views <- table_views[order(table_views$views, decreasing = TRUE), ] 
table_views$title <- NA
for (index in 1:10325){
  table_views[index,3] <- as.character(subset(movie_data, movie_data$movieId == table_views[index,1])$title)
}
table_views[1:6,]

# Visualize a bar plot for the total number of views of the top films
ggplot(table_views[1:6, ], aes(x = title, y = views)) + geom_bar(stat="identity", fill = 'steelblue') + geom_text(aes(label=views), vjust=-0.3, size=3.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Total Views of the Top Films")

# Visualize a heatmap of the movie ratings
image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")

# Data Preparation
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50, colCounts(ratingMatrix) > 50]

# Describing matrix of relevant users
minimum_movies <- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)

# Generating heatmap of the top users and movies
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,colCounts(movie_ratings) > minimum_users],main = "Heatmap of the top users and movies")

# Calculate average ratings per user
average_ratings <- rowMeans(movie_ratings)

# Visualize the distribution of average ratings per user
qplot(average_ratings, fill = I("steelblue"), col = I("red")) + ggtitle("Distribution of the Average Rating per User")

# Data Normalization
normalized_ratings <- normalize(movie_ratings)

# Calculate the number of users with non-zero average normalized ratings
sum(rowMeans(normalized_ratings) > 0.00001)

# Visualize the normalized ratings of the top users
image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies, colCounts(normalized_ratings) > minimum_users],main = "Normalized Ratings of the Top Users")

# Data Binarization
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)

movies_watched <- binarize(movie_ratings, minRating = 1)
good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,colCounts(movie_ratings) > binary_minimum_users],main = "Heatmap of the top users and movies")

# Collaborative Filtering System
# Splitting the dataset into training and test sets
sampled_data <- sample(x = c(TRUE, FALSE),size = nrow(movie_ratings),replace = TRUE,prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

# Building the Recommendation System
recommendation_system <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommen_model <- Recommender(data = training_data,method = "IBCF",parameter = list(k = 30))
recommen_model

# Exploring the recommendation system model
model_info <- getModel(recommen_model)

top_items <- 20
image(model_info$sim[1:top_items, 1:top_items], main = "Heatmap of the first rows and columns")

# Visualize sum of rows and columns with the similarity of the objects above 0
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill = I("steelblue"), col = I("red")) + ggtitle("Distribution of the column count")

# Number of items to recommend to each user
top_recommendations <- 10 
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

# Recommendations for the first user
user1 <- predicted_recommendations@items[[1]] 
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,movie_data$movieId == movies_user1[index])$title)
}
movies_user2

# Matrix with the recommendations for each user
recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) 
recommendation_matrix[,1:4]
