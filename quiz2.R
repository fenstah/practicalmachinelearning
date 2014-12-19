library(AppliedPredictiveModeling)
library(caret)
library(Hmisc) 
library(ggplot2)

data(concrete)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + 
    theme_bw()

cutCS <- cut2(training$CompressiveStrength, g = 4)
ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + geom_jitter(col = "blue") + 
    theme_bw()

names <- colnames(concrete)
names <- names[-length(names)]

featurePlot(x=training[,names], y=training$CompressiveStrength, plot="pairs")


set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]

ILPredictors<- grep("^IL", colnames(training), value = TRUE)
preProc <- preProcess(training[, ILPredictors], method = "pca", thresh = 0.9)

set.seed(3433)
predictorsIL<-predictors[,ILPredictors]
df<-data.frame(diagnosis, predictorsIL)
inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
training = df[inTrain, ]
testing = df[-inTrain, ]

modelFit<-train(diagnosis~.,method="glm", data=training)
predictions1<-predict(modelFit, newdata=testing)
CM1<-confusionMatrix(predictions1, testing$diagnosis)
print(CM1)
