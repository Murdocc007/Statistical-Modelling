library(e1071)

#inporting the data
data = read.csv("/home/aditya/Desktop/Aditya/Semper/Assignment 11/Assign_11-Data/bloodDonorData.txt", header=T) 

#creating test and training dataset. Training set has 40% observations.
smp_size <- floor(0.6 * nrow(data)) 
train=sample(seq_len(nrow(data)), size = smp_size) 
data.train <- data[train, ] 
data.test <- data[-train, ] 



#fitting the support vector classifier
svmfit=svm(Donated~., data=data.train, kernel="linear", cost=10,scale=FALSE)
svmfit$index
summary(svmfit)
pred=predict (svmfit,newdata = data[-train,])
pred[pred<=0]=0
pred[pred>0]=1
mean(pred==data.test$Donated)


#fitting data with less cost which will in turn increase the number of support vectors
svmfit=svm(Donated~., data=data.train, kernel="linear", cost=0.1,scale=FALSE)
svmfit$index
summary(svmfit)
pred=predict (svmfit,newdata = data[-train,])
pred[pred<=0]=0
pred[pred>0]=1
mean(pred==data.test$Donated)

#using 10 fold cross validation to find the best value for cost
tune.out=tune(svm,Donated~.,data=data,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
pred=predict (bestmod,newdata = data[-train,])
pred[pred<0]=0
pred[pred>=0]=1
mean(pred==data.test$Donated)


#fitting the support vector classifier with a radial kernel
svmfit=svm(Donated~., data=data.train, kernel="radial", gamma=1,cost=1,scale=FALSE)
svmfit$index
summary(svmfit)
pred=predict (svmfit,newdata = data[-train,])
pred[pred<=0]=0
pred[pred>0]=1
mean(pred==data.test$Donated)

#using 10 fold cross validation to find the best value for cost
tune.out=tune(svm,Donated~. ,data=data,kernel='radial',ranges = list (cost = c (0.1 ,1 ,10 ,100 ,1000),gamma = c (0.5 ,1 ,2 ,3 ,4)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
pred=predict (bestmod,newdata = data[-train,])
pred[pred<0]=0
pred[pred>=0]=1
mean(pred==data.test$Donated)
