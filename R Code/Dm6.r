library(ggplot2)
library(dbscan)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(class)
library(fpc)


iris <- read.csv("dirty_iris.csv", header = TRUE,sep =",")
for (i in 1:(ncol(iris)-1)) {
  iris[!is.finite(iris[, i]), i] <- NA
}
clean_iris = na.omit(iris)
clean_iris

iris_feat <- clean_iris[-5]
iris_feat

# Determine the optimal number of clusters using the elbow method
png("myplot.png", width = 800, height = 600)
fviz_nbclust(iris_feat, kmeans, method = "wss") + ggtitle("Elbow plot for Simple Kmeans clustering")
dev.off() # Save the plot and close the graphical device


# using kmeans
kmeansModel <- kmeans(iris_feat, 3)
kmeansModel

iris_feat$cluster <- factor(kmeansModel$cluster)
iris_feat$cluster
par(mar = c(2, 2, 2, 2))  # Set margin size to 2 for bottom, left, top, right

plot(iris_feat[1:4],col=kmeansModel$cluster)

# using dbscan
iris_feat <- clean_iris[-5]
iris_feat
kNNdistplot(iris_feat, k=3)

dbscanModel <- dbscan(iris_feat, .5, 1)
dbscanModel

iris_feat$cluster <- factor(dbscanModel$cluster)
iris_feat$cluster

plot(iris_feat[2:5],col=kmeansModel$cluster)

# using hierarchical clustering
iris_feat <- clean_iris[-5]
hcModel <- hclust(dist(iris_feat), method="average") 

plot(hcModel, hang = -1, labels=clean_iris$...5)

rect.hclust(hcModel, k=3)

iris_feat$cluster <- factor(cutree(hcModel, k=3))
iris_feat$cluster

plot(iris_feat[1:4],col=iris_feat$cluster)