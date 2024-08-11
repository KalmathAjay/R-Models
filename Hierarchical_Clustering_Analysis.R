# Hierarchical Clustering

library(cluster)

dataset = read.csv('/cloud/project/workingdir/Data/mall.csv') # Importing the dataset
X = dataset[4:5]

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(X, method = 'euclidean'),
                    method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(X, method = 'euclidean'),
            method = 'ward.D')
y_hc = cutree(hc, 5)

# Visualising the clusters
clusplot(x = X,
         clus = y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')

# Silhouette Score Calculation
silhouette_score = silhouette(y_hc, dist(X))
plot(silhouette_score, main = "Silhouette Plot")  
