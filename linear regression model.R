testdata <- read.csv("C:\\Users\\MUKESH\\Downloads\\test_HujdGe7\\test.csv",stringsAsFactors = T)
testdata$label <- 'test'

traindata <- read.csv("C:/Users/MUKESH/Downloads/train_oSwQCTC/train.csv",stringsAsFactors = T)
traindata$label <- 'train'
testdata$Purchase <- 1

finaldata <- rbind(testdata,traindata)

finaldata$Age <- as.numeric(finaldata$Age)

str(finaldata)
summary(finaldata)
names(finaldata)
finaldata1 <- finaldata[,-c(10,11)]

names(finaldata1)

finaldata1$label <- as.factor(finaldata1$label)


num_data <- finaldata1[sapply(finaldata1,is.numeric)]
fac_data <- finaldata1[sapply(finaldata1,is.factor)]

str(finaldata1)


for(i in 2:4){
  num_data[,i]=(num_data[,i]-min(num_data[,i]))/(max(num_data[,i])-min(num_data[,i]))
}

newdata <- cbind(num_data,fac_data)
summary(newdata)
str(newdata)

traindata <- subset(newdata,label == 'train')
testdata <- subset(newdata,label == 'test')

train <- traindata[1:425000,]
test <- traindata[425001:550068,]
names(train)
model1 <- lm(Purchase~.,data = newdata[,c(-1,-7)])
summary(model1)
pred <- predict(model1,newdata = test)

library(MLmetrics)
RMSE(pred,test$Purchase)

MAPE(pred,test$Purchase)

testdata$pred <- predict(model1,newdata = testdata)

names(testdata)
pred_upload <- testdata[,c(1,7,12)]
names(pred_upload)[3] <- 'Purchase'
write.csv(pred_upload,"pred_upload.csv",row.names = F)
getwd()
