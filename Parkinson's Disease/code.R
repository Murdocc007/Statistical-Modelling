require("MASS")
require("class")
parkinson.data=read.csv("/home/aditya/Desktop/Aditya/Semper/Assignment 4/Assign_4-Data/parkinsons.data.data",header = T)

# dropping the bwt and ID variable
drops=c("name")
parkinson.data=parkinson.data[,!names(parkinson.data) %in% drops]

# 75% of the data is training data
smp_size <- floor(0.75 * nrow(parkinson.data))
train_ind=sample(seq_len(nrow(parkinson.data)), size = smp_size)
train <- parkinson.data[train_ind, ]
test <- parkinson.data[-train_ind, ]

###########################################LDA###############################################################

#attaching the training data
attach(train)

model=lda(status~.,data=train)

#attribute MDVP.Jitter.Abs seems to collinear with a lot of data points therfore removing the column
drops=c("MDVP.Jitter.Abs.")
parkinson.data=parkinson.data[,!names(parkinson.data) %in% drops]

#creating the training and the testing data again
train <- parkinson.data[train_ind, ]
test <- parkinson.data[-train_ind, ]

#creating the model again
model=lda(status~.,data=train)

detach(train)

#predicting the values
model.predict=predict(model,test)

#creating the table
table(model.predict$class,test$status)

#calculating the accuracy
mean(model.predict$class==test$status)

#################################QDA###########################################
#attaching the training data
attach(train)

model=qda(status~.,data=train)

detach(train)

#predicting the values
model.predict=predict(model,test)

#creating the table
table(model.predict$class,test$status)

#calculating the accuracy
mean(model.predict$class==test$status)


######################################KNN#####################################
knn.pred=knn(train,test,train$status,k=1)
table(knn.pred,test$status)
knn.pred=knn(train,test,train$status,k=1)
table(knn.pred,test$status)
mean(knn.pred==test$status)
