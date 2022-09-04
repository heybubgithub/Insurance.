# Insurance.
Linear Regression Model

insu <- read.csv("C:\\Users\\MUKESH\\Downloads\\insurance.csv",stringsAsFactors = T)

insu
str(insu)
summary(insu)

insu$gender <- as.character(insu$gender)
insu$smoker <- as.character(insu$smoker)

insu$gender <- ifelse(insu$gender=="",NA,insu$gender)
insu$smoker <- ifelse(insu$smoker=="",NA,insu$smoker)

summary(insu)

insu$gender <- as.factor(insu$gender)
insu$smoker <- as.factor(insu$smoker)

summary(insu)

install.packages("Amelia") #this package is for better presentation
library(Amelia)

missmap(insu) #white line in map shows missing data

miss_rpw <- insu[!complete.cases(insu),] #!complete this means no complete case i want from data
miss_rpw

install.packages("DMwR2") #this package is for (knn) method which use to find missing or NA data  

library(DMwR2)
imputed_data <- knnImputation(insu,meth = 'median')



summary(imputed_data)

imputed_data1 <- imputed_data[sapply(imputed_data,is.numeric)] 
imputed_data1


imputed_data2 <- imputed_data[sapply(imputed_data,is.factor)]
imputed_data2 


install.packages("caret")
library(caret)


imputed_data1_nzv <- (nearZeroVar(imputed_data1))  
imputed_data1_nzv
names(imputed_data1)

for(i in 1:3){
  imputed_data1[,i] <- ifelse(imputed_data1[,i]<quantile(imputed_data1[,i],0.01),
                              quantile(imputed_data1[,i],0.01),imputed_data1[,i])
}



for(i in 1:3){
  imputed_data1[,i] <- ifelse(imputed_data1[,i]>quantile(imputed_data1[,i],0.99),
                              quantile(imputed_data1[,i],0.99),imputed_data1[,i])
}

summary(imputed_data1)

for(i in 1:3){
  imputed_data1[,i] <- (imputed_data1[,i] - min(imputed_data1[,i]))/(max(imputed_data1[,i])-min(imputed_data1[,i]))
}

final_data <- cbind(imputed_data1,imputed_data2)
final_data

train <- final_data[1:1000,]
test <- final_data[1001:1338,]

model1 <- lm(charges~.,data = train)
summary(model1)
library(car)
vif(model1)
durbinWatsonTest(model1)

test$pred <- predict(model1,test)

install.packages("MLmetrics")
library(MLmetrics)
RMSE = RMSE(test$pred,test$charges)
RMSE

MAPE(y_pred = test$pred,y_true = test$charges)

