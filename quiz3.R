##question 1
data(segmentationOriginal)
inTrain<-createDataPartition(segmentationOriginal$Case, p=0.7, list=FALSE)
training<-segmentationOriginal[segmentationOriginal$Case=="Train",]
testing<-segmentationOriginal[segmentationOriginal$Case=="Test",]
set.seed(125)
modelRpart<-train(Class~.,method='rpart', data=training)
predRPart<-predict(modelRpart,newdata=testing)
library(rattle)
fancyRpartPlot(modelRpart$finalModel)
print(modelRpart$finalModel)

##question 3
library(pgmm)
data(olive)
olive=olive[,-1]
library(tree)
modOlive<-tree(Area~.,data=olive)
predOlive<-predict(modOlive, newdata=as.data.frame(t(colMeans(olive))))
print(predOlive)

##question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
modHeart<-train(chd~age+alcohol+obesity+tobacco+typea+ldl, method="glm", family="binomial", data=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
print(missClass(testSA$chd, predict(modHeart, testSA)))
print(missClass(trainSA$chd, predict(modHeart, trainSA)))

##question 5
data(vowel.train)
data(vowel.test)
vowel.test$y<-as.factor(vowel.test$y)
vowel.train$y<-as.factor(vowel.train$y)
set.seed(33833)
modVowel<-train(y~.,method="rf", data=vowel.train)
varImp(modVowel, scale=FALSE)



