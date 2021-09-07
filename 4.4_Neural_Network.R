library(neuralnet)
library(ggplot2)
library(forecast)
library(knitr)
library(tidyverse)
library(mice)
library(lattice)
library(reshape2)
#install.packages("DataExplorer") if the following package is not available
library(DataExplorer)

# Load data

getwd()

dat <- read.csv("UCI_Credit_Card.csv")
summary(dat)

# Preprocessing
plot_correlation(na.omit(dat), maxcat = 5L)
plot_histogram(dat)
data_new <- select(dat, -one_of('ID','AGE', 'BILL_AMT2',
                                 'BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6'))
head(data_new)

#data partition
#create a list of random number ranging from 1 to number of rows from actual data 
#and 70% of the data into training data  
data2 = sort(sample(nrow(data_new), nrow(data_new)*.7))

#creating training data set by selecting the output row values
train <- data_new[data2,]

#creating test data set by not selecting the output row values
test <- data_new[-data2,]
dim(train)
dim(test)

#Neural networks
model = neuralnet(data= dat, default.payment.next.month ~ .,hidden=3, linear.output=FALSE, threshold=0.01)
plot(model,col.hidden = 'darkgreen',
     show.weights=TRUE,
     information=FALSE,
     fill='lightblue')
summary(model)

#Predict
library(neuralnet)
pred <- neuralnet::compute(model,test[,1:17])
str(pred)

pred <- pred$net.result*(max(test$default.payment.next.month)-min(test$default.payment.next.month))+min(test$default.payment.next.month)
actualValues <- (test$default.payment.next.month)*(max(test$default.payment.next.month)-min(test$default.payment.next.month))+min(test$default.payment.next.month)

MSE <- sum((pred - actualValues)^2)/nrow(test)
MSE

nn.results <- neuralnet::compute(model, test)
results <- data.frame(actual = test$default.payment.next.month, prediction = nn.results$net.result)
results


plot(test$LIMIT_BAL,pred,col='blue',main='Acutal vs Predict',pch=1,cex=0.9,type="p",xlab="Acutal", ylab="Predict")
abline(0,1,col="black")
