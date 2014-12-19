library(ISLR); library(ggplot2); library(caret)
data(Wage)
Wage<-subset(Wage, select=-c(logwage))
print(summary(Wage))

inTrain<-createDataPartition(Wage$wage, p=0.7, list=FALSE)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
