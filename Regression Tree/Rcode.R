fullData <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", header=F) #import

names(fullData) <- c("age", "workclass", "fnlwgt", "education", "educationnum", "maritalstatus", "occupation", "relationship", "race", "sex", "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "response")

fullData <- fullData[, c(15, 1:13)] # remove a factor with more than 31 levels.

set.seed(100)

train <- sample (1:nrow(fullData), .8*nrow(fullData)) # training row indices

inputData <- fullData[train, ] # training data

testData <- fullData[-train, ] # test data

library(tree)

treeMod <- tree(response ~ ., data = inputData)  # model the tree, including all the variables

plot(treeMod)  # Plot the tree model

text(treeMod, pretty=0)  # Add text to the plot

out <- predict(treeMod) # Predict the training data

input.response <- as.character(inputData$response) # actuals

pred.response <- colnames(out)[max.col(out, ties.method = c("first"))] # predicted

cvTree <- cv.tree(treeMod, FUN = prune.misclass)  # run the cross validation

plot(cvTree)  # plot the CV

treePrunedMod <- prune.misclass(treeMod, best = 9) # set size corresponding to lowest value in below plot. try 4 or 16.

plot(treePrunedMod)

text(treePrunedMod, pretty = 0)

out <- predict(treePrunedMod) # fit the pruned tree

pred.response <- colnames(out)[max.col(out, ties.method = c("random"))] # predicted

mean(inputData$response != pred.response) # Calculate Mis-classification error.

testout <- predict(treePrunedMod, testData)  # Predict testData with Pruned tree

predtest.response <- colnames(testout)[max.col(out, ties.method = c("random"))] # predicted

mean(testData$response != predtest.response) # Calculate Mis-classification error.