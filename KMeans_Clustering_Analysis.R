# K-Means Clustering

library(cluster)
library(fpc)

# Importing the dataset
dataset = read.csv('/cloud/project/workingdir/Data/mall.csv') 
X = dataset[4:5]

# Using the elbow method to find the optimal number of clusters
set.seed(150)
wcss = vector()  # Initialize a vector to store WCSS values
for (i in 1:10) {
  wcss[i] = sum(kmeans(X, i)$withinss)  # Calculate WCSS for each number of clusters
}

# Plotting the elbow method results
plot(x = 1:10,
     y = wcss,
     type = 'b',
     main = 'The Elbow Method',
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(30)
kmeans = kmeans(x = X,
                centers = 5,
                iter.max = 300,
                nstart = 10)

# Visualising the clusters
clusplot(x = X,
         clus = kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = 'Clusters of Customers',
         xlab = 'Annual Income',
         ylab = 'Spending Score')

# Calculate and plot the Silhouette Score
silhouette_score = silhouette(kmeans$cluster, dist(X))
plot(silhouette_score, main = "Silhouette Plot")
