library(tm)
corpus = Corpus(VectorSource(c(eBayiPadTrain$description, eBayiPadTest$description)))

-------corpus<-Corpus(VectorSource(eBayiPadTrain$description))
corpus
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,PlainTextDocument)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeWords,stopwords("english"))
corpus<-tm_map(corpus,stemDocument)

dtm<-DocumentTermMatrix(corpus)
dtm
----dtmsparse<-removeSparseTerms(dtm,0.995)
dtmsparse<-removeSparseTerms(dtm,0.99)

dtmsparse

dtmframe<-as.data.frame(as.matrix(dtmsparse))

colnames(dtmframe) = make.names(colnames(dtmframe))
dtmframeTrain = head(dtmframe, nrow(eBayiPadTrain))
dtmframeTest = tail(dtmframe, nrow(eBayiPadTest))
---DescriptionWordsTrain$WordCount = eBayTrain$WordCount
---DescriptionWordsTest$WordCount = eBayTest$WordCount

trainWordCount<-nchar(eBayiPadTrain$description)
testWordCount<-nchar(eBayiPadTest$description)


dtmframeTrain$sold<-eBayiPadTrain$sold
dtmframeTrain$WordCount<-trainWordCount
dtmframeTest$WordCount<-testWordCount


library(rpart)
ipadtreecombineddtm<-rpart(sold~.,data=dtmframeTrain,method="class")
set.seed(24)
library(randomForest)
ipadforest<-randomForest(as.factor(sold)~.,data=dtmframeTrain,method="class")


forestpred<-predict(ipadforest)
table(dtmframeTrain$sold,forestpred)


treepred<-predict(ipadtreecombineddtm)
table(dtmframeTrain$sold,treepred[,2]>0.5)

dtmframeTrain$Abiddable<-eBayiPadTrain$biddable
dtmframeTrain$Astartprice<-eBayiPadTrain$startprice
dtmframeTrain$Acondition<-as.factor(eBayiPadTrain$condition)
dtmframeTrain$Acellular<-as.factor(eBayiPadTrain$cellular)
dtmframeTrain$Acarrier<-as.factor(eBayiPadTrain$carrier)
dtmframeTrain$Acolor<-as.factor(eBayiPadTrain$color)
dtmframeTrain$Astorage<-as.factor(eBayiPadTrain$storage)
dtmframeTrain$Aproductline<-as.factor(eBayiPadTrain$productline)

treepred2[,2]

testframe<-create_myhist_matrix(eBayiPadTest$description,originalMatrix=dtmsparse)
testframe<-as.data.frame(as.matrix(testframe))

testframe<-dtmframeTest
testframe$Abiddable<-eBayiPadTest$biddable
testframe$Astartprice<-eBayiPadTest$startprice
testframe$Acondition<-as.factor(eBayiPadTest$condition)
testframe$Acellular<-as.factor(eBayiPadTest$cellular)
testframe$Acarrier<-as.factor(eBayiPadTest$carrier)
testframe$Acolor<-as.factor(eBayiPadTest$color)
testframe$Astorage<-as.factor(eBayiPadTest$storage)
testframe$Aproductline<-as.factor(eBayiPadTest$productline)

levels(testframe$Aproductline)<-levels(dtmframeTrain$Aproductline)

testpred<-predict(ipadtreecombineddtm,newdata=testframe)
MySubmission = data.frame(UniqueID = eBayiPadTest$UniqueID, Probability1 = testpred[,2])
write.csv(MySubmission, "combineddtmtreewithcount.csv", row.names=FALSE)


MySubmission = data.frame(UniqueID = eBayiPadTest$UniqueID, Probability1 = forestpred)
write.csv(MySubmission, "combineddtmforestwithcount.csv", row.names=FALSE)

testpred<-predict(ipadforest,newdata=testframe,type="prob")[,2]
testpred

forestsub<-data.frame(eBayiPadTest$UniqueID,testpred)
colnames(forestsub)<-c("UniqueID","Probability1")
write.csv(forestsub,file="latestforest.csv")




clustsub<-data.frame(eBayiPadTest$UniqueID,AllPredictions)
colnames(firstsub)<-c("UniqueID","Probability1")
write.csv(firstsub,file="musub.csv")
clustSubmission = data.frame(UniqueID = eBayiPadTest$UniqueID, Probability1 = AllPredictions)

write.csv(clustSubmission, "clustSubmissionDescriptionLog.csv", row.names=FALSE)


testframe<-as.data.frame(as.matrix(testframe))

temp1<-testframe$Acarrier
testframe$Acarrier<-NULL
testframe$Acarrier<-temp1
