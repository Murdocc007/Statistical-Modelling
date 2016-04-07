data<- mmr_levee
names(data) <- c("Fail","Year", "River","Sedi","Borrow", "Meander","Channel","Floodway","Cons", "Land", "Veg","Sinu","Dred", "Reve")
str(data)
summary(data)
install.packages("e1071", dependencies = TRUE)
set.seed(1)
#removing revetement variable as it is 0 for all but 1 observations
data= data[, -c(14)] 
y= data$Fail
data= data[, -c(1)] 
x=data

#creating the data frame with y as response variable
dat=data.frame(x=x, y=as.factor(y))

library(e1071)
#creating test and training dataset. Training set has 0% observations.
smp_size <- floor(0.6 * nrow(dat)) 
train_ind=sample(seq_len(nrow(dat)), size = smp_size) 
data.train <- dat[train_ind, ] 
data.test <- dat[-train_ind, ] 

#fitting the support vector classifier
svmfit=svm(y~., data=data.train, kernel="linear", cost=10,scale=FALSE)
#this does not work as we have more  than 2 predictors
plot(svmfit, dat)  
#the support vectors are found using index on the fit
svmfit$index
summary(svmfit)
svmfit=svm(y~., data=data.train, kernel="linear", cost=0.1,scale=FALSE)
svmfit$index

#using 10 fold cross validation to find the best value for cost
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)


#using the model to predict for the test dataset
ypred=predict(bestmod,data.test)
table(predict=ypred, truth=data.test$y)
svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit,data.test)
table(predict=ypred, truth=data.test$y)

