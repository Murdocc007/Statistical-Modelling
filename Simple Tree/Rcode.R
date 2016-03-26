library(tree)

fullData <- read.csv("/home/aditya/Desktop/Aditya/Semper/Assignment 8/adult.data", header=F) #import

names(fullData) <- c("age", "workclass", "fnlwgt", "education", "educationnum", "maritalstatus", "occupation", "relationship", "race", "sex", "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "response")

fullData <- fullData[, c(15, 1:13)] # remove a factor with more than 31 levels.

set.seed(100)

train <- sample (1:nrow(fullData), .8*nrow(fullData)) # training row indices

inputData <- fullData[train, ] # training data

testData <- fullData[-train, ] # test data


treeMod <- tree(response ~ ., data = inputData)  # model the tree, including all the variables

plot(treeMod)  # Plot the tree model

text(treeMod, pretty=0)  # Add text to the plot

out <- predict(treeMod) # Predict the training data

input.response <- as.character(inputData$response) # actuals

pred.response <- colnames(out)[max.col(out, ties.method = c("first"))] # predicted

mean (input.response != pred.response) # misclassification %