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

## using holdout method

trainIndex <- createDataPartition(clean_iris$Species, p=.8, list=FALSE)
train <- clean_iris[trainIndex,]
test <- clean_iris[-trainIndex,]

# Naive Bayes
nbModel = naiveBayes(Species ~ .,train)
predictions <- predict(nbModel, test)
cm_nb <- table(test$Species, predictions)
cm_nb
# Model Evaluation
nb_cm <-confusionMatrix(cm_nb)
nb_acc_hold <- nb_cm$overall['Accuracy']

# K-nearest neighbor
knn_model <- knn(train[,1:4], test[,1:4], train$Species, k = 3)
cm_knn <- table(test$Species, knn_model)
cm_knn
# Model Evaluation
knn_cm <-confusionMatrix(cm_knn)
knn_acc_hold <- knn_cm$overall['Accuracy']

# Decision tree
dt_model <- rpart(Species ~ ., data = train, method = "class")
dt_pred <- predict(dt_model, newdata = test, type = "class")
cm_dt <- table(test$Species, dt_pred)
cm_dt
# Model Evaluation
dt_cm <-confusionMatrix(cm_dt)
dt_acc_hold <- dt_cm$overall['Accuracy']

# print accuracy of classifiers
cat("Naive Bayes accuracy using Holdout:", nb_acc_hold, "\n")
cat("K-nearest neighbor accuracy using Holdout:", knn_acc_hold, "\n")
cat("Decision tree accuracyusing Holdout:", dt_acc_hold, "\n")


## using random subsampling
set.seed(123)
nb_acc_ran <- numeric(5)
knn_acc_ran <- numeric(5)
dt_acc_ran <- numeric(5)

for(x in 1:5) {
  split <- sample.split(clean_iris$Species, SplitRatio = 0.66)
  
  train_set_rndm <- subset(clean_iris, split == TRUE)
  test_set_rndm <- subset(clean_iris, split == FALSE) 
  
  dim(test_set_rndm)
  dim(train_set_rndm)
  
  train_scale_rndm <- scale(train_set_rndm[,1:4]) 
  test_scale_rndm <- scale(test_set_rndm[,1:4])
  
  # NAIVE BAYES 
  classifier_naive_rndm <- naiveBayes(Species~., data = train_set_rndm)
  predicted_y <- predict(classifier_naive_rndm, newdata = test_set_rndm)
  
  #confusion matrix 
  cm_nb <- table(test_set_rndm$Species, predicted_y)
  cm_nb
  nb_cm <- confusionMatrix(cm_nb)
  nb_acc_ran[x] <- nb_cm$overall['Accuracy']
  
  #KNN 
  classifier_knn_rndm <- knn(train = train_scale_rndm[, c(1,2,3,4)], test = 
                               test_scale_rndm[, c(1,2,3,4)], cl = train_set_rndm$Species, k = 1)
  
  #confusion matrix 
  cm_knn <- table(test_set_rndm$Species, classifier_knn_rndm)
  cm_knn
  knn_cm <- confusionMatrix(cm_knn)
  knn_acc_ran[x] <- knn_cm$overall['Accuracy']
  
  # Decision Tree
  dtm_rndm <- rpart(Species ~ ., train_set_rndm, method = "class")
  p <- predict(dtm_rndm, test_set_rndm, type = "class")
  
  #confusion matrix 
  cm_dt <- table(test_set_rndm$Species, p)
  cm_dt
  dt_cm <- confusionMatrix(cm_dt)
  dt_acc_ran[x] <- dt_cm$overall['Accuracy']
}

nb_acc_rs <- mean(nb_acc_ran)
knn_acc_rs <- mean(knn_acc_ran)
dt_acc_rs <- mean(dt_acc_ran)

# print accuracy of classifiers
cat("Naive Bayes accuracy using Random Subsampling:", nb_acc_rs, "\n")
cat("K-nearest neighbor accuracy using Random Subsampling:", knn_acc_rs, "\n")
cat("Decision tree accuracyusing Random Subsampling:", dt_acc_rs, "\n")

## using cv
# Naive Bayes
print("Naive bayes-Cross Validation")
nb_model<-train(clean_iris[,1:4],clean_iris[,5],'nb',
                trControl = trainControl(method = 'cv', number=3))
print(nb_model)
#KNN
print("KNN-Cross Validation-part b")
knn_model<-train(clean_iris[,1:4],clean_iris[,5],'knn',
                 trControl = trainControl(method = 'cv', number=3))
print(knn_model)
#DECISION TREE
print("Decision Tree-Cross Validation-part b")
Dtree_model<-train(clean_iris[,1:4],clean_iris[,5],'rpart',
                   trControl = trainControl(method = 'cv', number=3))
print(Dtree_model)
