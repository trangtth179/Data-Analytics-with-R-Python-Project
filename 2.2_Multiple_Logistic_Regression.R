#import Library
library(yardstick)
library(caTools)
library(ggplot2)
library(caret) 

#Load data set
dataset <- read.csv('./data/Social_Network_Ads.csv')
dataset <- dataset[3:5]

dim(dataset)

#Splitting dataset to training set and test set
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio <- 0.75)

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#feature  scaling
training_set[-3] <- scale(training_set[-3])
test_set[-3] <- scale(test_set[-3])

#Create model
classifier <- glm(formula = Purchased ~ ., family <- binomial, data <- training_set)

#Predict
prob_pred <- predict(classifier, type = 'response', newdata <- test_set[-3])
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

print(classifier)

xtab <- table(y_pred, test_set[, 3])

confusionMatrix(xtab)
