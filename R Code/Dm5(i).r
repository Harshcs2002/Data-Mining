library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(class)

iris <- read.csv("dirty_iris.csv", header = TRUE,sep =",")
for (i in 1:(ncol(iris)-1)) {
  iris[!is.finite(iris[, i]), i] <- NA
}
clean_iris = na.omit(iris)
clean_iris
## using 75% train, 25% test
set.seed(123) # set the seed for reproducibility
trainIndex <- createDataPartition(clean_iris$Species, p = 0.75, list = FALSE, times = 1)
train <- clean_iris[trainIndex,]
test <- clean_iris[-trainIndex,]

# compare accuracy of different classifiers
# using training set = 75%, testing set = 25%
# Naive Bayes
nbModel = naiveBayes(Species ~ .,train)
predictions <- predict(nbModel, test)
cm_nb <- table(test$Species, predictions)
cm_nb
# Model Evaluation
nb_cm <-confusionMatrix(cm_nb)
nb_acc_75 <- nb_cm$overall['Accuracy']


# K-nearest neighbor
knn_model <- knn(train[,1:4], test[,1:4], train$Species, k = 3)
cm_knn <- table(test$Species, knn_model)
cm_knn
# Model Evaluation
knn_cm <-confusionMatrix(cm_knn)
knn_acc_75 <- knn_cm$overall['Accuracy']

# Decision tree
dt_model <- rpart(Species ~ ., data = train, method = "class")
dt_pred <- predict(dt_model, newdata = test, type = "class")
cm_dt <- table(test$Species, dt_pred)
cm_dt
# Model Evaluation
dt_cm <-confusionMatrix(cm_dt)
dt_acc_75 <- dt_cm$overall['Accuracy']

# print accuracy of classifiers
cat("Naive Bayes accuracy for 75% train:", nb_acc_75, "\n")
cat("K-nearest neighbor accuracy for 75% train:", knn_acc_75, "\n")
cat("Decision tree accuracy for 75% train:", dt_acc_75, "\n")

# using training set = 66.6%, testing set = 33.3%
set.seed(123) # set the seed for reproducibility
trainIndex_66 <- createDataPartition(clean_iris$Species, p = 0.666, list = FALSE, times = 1)
train <- clean_iris[trainIndex_66,]
test <- clean_iris[-trainIndex_66,]

# Naive Bayes
nbModel = naiveBayes(Species ~ .,train)
predictions <- predict(nbModel, test)
cm_nb <- table(test$Species, predictions)
cm_nb
# Model Evaluation
nb_cm <-confusionMatrix(cm_nb)
nb_acc_66 <- nb_cm$overall['Accuracy']


# K-nearest neighbor
knn_model <- knn(train[,1:4], test[,1:4], train$Species, k = 3)
cm_knn <- table(test$Species, knn_model)
cm_knn
# Model Evaluation
knn_cm <-confusionMatrix(cm_knn)
knn_acc_66 <- knn_cm$overall['Accuracy']

# Decision tree
dt_model <- rpart(Species ~ ., data = train, method = "class")
dt_pred <- predict(dt_model, newdata = test, type = "class")
cm_dt <- table(test$Species, dt_pred)
cm_dt
# Model Evaluation
dt_cm <-confusionMatrix(cm_dt)
dt_acc_66 <- dt_cm$overall['Accuracy']

# print accuracy of classifiers
cat("Naive Bayes accuracy for 66% train:", nb_acc_66, "\n")
cat("K-nearest neighbor accuracy for 66% train:", knn_acc_66, "\n")
cat("Decision tree accuracy for 66% train:", dt_acc_66, "\n")