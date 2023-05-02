library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(class)
library(caTools)

iris <- read.csv("dirty_iris.csv", header = TRUE,sep =",")
for (i in 1:(ncol(iris)-1)) {
  iris[!is.finite(iris[, i]), i] <- NA
}
clean_iris = na.omit(iris)
clean_iris

## standardize dataset

standardize = function(x) {
  z <- (x - mean(x)) / sd(x)
  return( z)
}

# Standardize the data
std_iris <- as.data.frame(lapply(clean_iris[, 1:4], standardize))
std_iris$Species <- clean_iris$Species

# Split data into training and testing set
set.seed(123)
split <- sample.split(clean_iris$Species, SplitRatio = 0.66)
train_set <- subset(clean_iris, split == TRUE)
test_set <- subset(clean_iris, split == FALSE)

# Train and test Naive Bayes model
classifier_naive <- naiveBayes(Species ~ ., data = train_set)
predicted_y <- predict(classifier_naive, newdata = test_set)
cm_nb <- table(test_set$Species, predicted_y)
nb_cm <- confusionMatrix(cm_nb)
nb_acc <- nb_cm$overall['Accuracy']

# Train and test K-Nearest Neighbors model
train_scale <- scale(train_set[, 1:4])
test_scale <- scale(test_set[, 1:4])
classifier_knn <- knn(train = train_scale[, c(1, 2, 3, 4)], test = test_scale[, c(1, 2, 3, 4)], cl = train_set$Species, k = 1)
cm_knn <- table(test_set$Species, classifier_knn)
knn_cm <- confusionMatrix(cm_knn)
knn_acc <- knn_cm$overall['Accuracy']

# Train and test Decision Tree model
dtm <- rpart(Species ~ ., train_set, method = "class")
p <- predict(dtm, test_set, type = "class")
cm_dt <- table(test_set$Species, p)
dt_cm <- confusionMatrix(cm_dt)
dt_acc <- dt_cm$overall['Accuracy']

# Print accuracy of each model
cat("Naive Bayes Accuracy:", nb_acc, "\n")
cat("K-Nearest Neighbors Accuracy:", knn_acc, "\n")
cat("Decision Tree Accuracy:", dt_acc, "\n")
