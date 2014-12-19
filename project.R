library(caret)

#get the data
gadata.orig<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings= c("", "NA", "#DIV/0!"))

#clean the data
#remove columns that are greater than 90% NA
cleandata <- function(df)
{
    numCols<-ncol(df)
    i<-8
    while(i  < numCols) {
        if ((sum(is.na(df[,i]))/nrow(df)) > 0.9) { 
            df[i]<-NULL  
            numCols<-numCols-1
        }
        else i<-i+1
    }
    
    #remove the identifier column        
    return (df[-1])
}
gadata.clean<-cleandata(gadata.orig)


##create the training and test sets
set.seed(54321)
inTrain <- createDataPartition(gadata.clean$classe, p = 3/4, list=FALSE)
training <- gadata.clean[inTrain,]
testing<-gadata.clean[-inTrain,]

set.seed(12345)
modelFit<-train(classe~.,method="rf", data=training)
predictTest<-predict(modelFit, testing)
confusionMatrix(predictTest, testing$classe)

validation<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",na.strings= c("", "NA", "#DIV/0!"))
validation.clean<-cleandata(validation)
predictValidation<-predict(modelFit, validation.clean)