library(ISLR)
require(xlsx)
library(crossval)
library(boot)
library(DAAG)

#looccv function implementation
LOOCV<-function(model) {
  unlist(sapply(seq(1,nrow(concrete.data)), function(i) {         
    training=concrete.data[-i,]#took out ith tuple
    test=concrete.data[i,]#testing tuple
    
    fit=eval(model)
    testValue = predict(fit, test)
    
    test[9]-testValue#the 9th column is the concrete strength column
  }))
}

concrete.data=read.xlsx("/home/aditya/Desktop/Aditya/Semper/Assignment 5/Assign_5-Data/Concrete_Data.xls",sheetIndex = 1)
attach(concrete.data)
cor(concrete.data)
#since the values are quantitative, therefore using lm instead of lr,qda and KNN
#also the correlation between superplasticizer and water is high, therfore 
#adding the interaction between superplasticizer and water
model=lm(Concrete.compressive.strength~.+Water:Superplasticizer,data=concrete.data)

#stepping the model in both the direction in order to create a model
model=step(model,direction = "both")

#accuracy in case of LOOCV
sqrt(sum(LOOCV(model))^2)/nrow(concrete.data)

#performing 10 cross fold validation
result=CVlm(data=concrete.data,model,m=10)
sqrt(sum((result$cvpred-result$Concrete.compressive.strength)^2))/length(result$Concrete.compressive.strength)

#performing 5 cross fold validation
result=CVlm(data=concrete.data,model,m=5)
sqrt(sum((result$cvpred-result$Concrete.compressive.strength)^2))/length(result$Concrete.compressive.strength)

