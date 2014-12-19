##question 1
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test)
vowel.test$y<-as.factor(vowel.test$y)
vowel.train$y<-as.factor(vowel.train$y)
set.seed(33833)
modVowel<-train(y~.,method="rf", data=vowel.train, verbose=FALSE)
modVowelBoost<-train(y~., method="gbm", data=vowel.train, verbose=FALSE)

predVowel<-predict(modVowel, vowel.test)
predVowelBoost<-predict(modVowelBoost, vowel.test)

qp<-qplot(predVowel, predVowelBoost, colour=y, data=vowel.test)
print(qp)

predDF<-data.frame(predVowel, predVowelBoost, y=vowel.test$y)
combModFit<-train(y~.,method="gam", data=predDF)
combPred<-predict(combModFit, predDF)


##question 2
library(gbm)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
modRF<-train(diagnosis~.,method="rf", data=training, verbose=FALSE)
modBoost<-train(diagnosis~., method="gbm", data=training, verbose=FALSE)
modLDA<-train(diagnosis~., method="lda", data=training, verbose=FALSE)

predRF<-predict(modRF, testing)
predBoost<-predict(modBoost, testing)
predLDA<-predict(modLDA, testing)


predAlzDF<-data.frame(predRF, predBoost, predLDA, diagnosis=testing$diagnosis)
combAlzModFit<-train(diagnosis~.,method="rf", data=predAlzDF)
combAlzPred<-predict(combAlzModFit, predAlzDF)


##question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
object2<-enet(as.matrix(training[,c(1:8)]),training$CompressiveStrength,lambda=0)
plot(object2)


##question 4
library(lubridate)  # For year() function below
dat<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")
#dat = read.csv("~/Desktop/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest =ts(testing$visitsTumblr, start=length(tstrain)+1)
fit<-bats(tstrain)
fcst<-forecast(fit, length(training$visitsTumblr))
plot(fcst)
lines(tstest, col="red")
sum<-0
upper <- fcst$upper
lower <- fcst$lower
for(i in 1:length(tstest)){
    print(paste(tstest[i],lower[i,1] , upper[i,2]))
    if(tstest[i] > lower[i,1] & tstest[i] < upper[i,2])
    {
        sum <- sum + 1
    }
}
print(sum/length(tstest))

##question 5
set.seed(3523)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
library(e1071)
model<-svm(CompressiveStrength~.,data=concrete)
pred<-predict(model, testing)
