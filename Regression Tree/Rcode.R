library(tree)

fullData = read.csv("/home/aditya/Desktop/Aditya/Semper/Assignment 8/adult.data", header=F) #import

names(fullData) = c("age", "workclass", "fnlwgt", "education", "educationnum", "maritalstatus", "occupation", "relationship", "race", "sex", "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "response")

fullData = fullData[, c(15, 1:13)] # remove a factor with more than 31 levels.


train = sample (1:nrow(fullData), .8*nrow(fullData)) # training row indices

inputData = fullData[train, ] # training data

testData = fullData[-train, ] # test data


treeMod = tree(response ~ ., data = inputData)  # model the tree, including all the variables

plot(treeMod)  # Plot the tree model

text(treeMod, pretty=0)  # Add text to the plot

cv.Model=cv.tree(treeMod) #running cross validation on the tree data

plot(cv.Model$size,cv.Model$dev,type='b')# plotting the cross validated tree

prune.model=prune.tree(treeMod,best = 8)# pruning the tree with 8 terminal nodes

plot(prune.model)# plotting the pruned tree

text(prune.model,pretty = 0)

yhat=predict(treeMod,newdata = fullData[-train,])# predicting the values on testing data

fulldata.test=fullData[-train,'response']# getting the response for the testing data

pred.response = colnames(yhat)[max.col(yhat, ties.method = c("first"))] # predicted

mean(pred.response!=fulldata.test)
