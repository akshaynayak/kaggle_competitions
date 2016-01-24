# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv", stringsAsFactors=F)
test  <- read.csv("../input/test.csv" ,stringsAsFactors=F)
train$Cat <- 'train'
test$Cat <- 'test'
test$Survived <- NA

full<-rbind(train,test)

full$Embarked[full$Embarked=='']<-'S'

full$Name<-as.character(full$Name)
full$Title<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][2])
full$Title<-gsub(' ','',full$Title)
full$Title[full$Title %in% c('Capt','Col','Don','Sir','Jonkheer','Major')]<-'Mr'
full$Title[full$Title %in% c('Lady','Ms','theCountess','Mlle','Mme','Ms','Dona')]<-'Miss'

full <- transform(full,
                  Cabin = factor(Cabin),
                  Pclass=factor(Pclass),
                  Sex=factor(Sex),
                  Embarked=factor(Embarked),
                  Title=factor(Title)
)

library(rpart)

fit.Age <- rpart(
  Age[!is.na(Age)] ~ Pclass + Title + Sex + SibSp + Parch + Fare,
  data = full[!is.na(full$Age),],
  method = 'anova')
full$Age[is.na(full$Age)] <- predict(fit.Age, full[is.na(full$Age), ])

fit.Fare <- rpart(
  Fare[!is.na(Fare)] ~ Pclass + Title + Sex + SibSp + Parch + Age,
  data = full[!is.na(full$Fare),],
  method = 'anova')
full$Fare[is.na(full$Fare)] <- predict(fit.Fare, full[is.na(full$Fare), ])

#split into train/test dataset
train <- full[full$Cat == 'train', ]
test <- full[full$Cat == 'test', ]

#randomForest fitting
library(randomForest)
fit <- randomForest(
  factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + Fare,
  data = train, 
  ntree = 2000)

varImpPlot(fit)
plot(fit)
test$Survived <- predict(fit,test)
submission <- test[,1:2]
write.csv(submission,'submissionrf_tit.csv', row.names = F)