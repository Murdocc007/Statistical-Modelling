library(e1071)
writing.data=read.csv("/home/aditya/Desktop/Aditya/Semper/Final Project/train.csv",header = T)

smp_size <- floor(0.75 * nrow(writing.data))
train_ind=sample(seq_len(nrow(writing.data)), size = smp_size)
train <- writing.data[train_ind, ]
validation <- writing.data[-train_ind, ]

writing.data$label=as.factor(writing.data$label)
principalComps <- prcomp( ~. , data = train)  #retrieve PCs
num.of.comp.train = 50
pca_train.rotation <- principalComps$rotation
pca_train.rotation <- pca_train.rotation[,1:num.of.comp.train]
trainingPRC <- as.matrix(train) %*% pca_train.rotation  #apply PCs to training data
validationPRC <- as.matrix(validation) %*% pca_train.rotation  #apply PCs to validation data

train.model <- svm(trainingPRC,train$label ,  type = "C-classification", kernel = "radial")  #train SVM

svm.pred <- predict(train.model, validationPRC)  #predict labels for validation data
mean(svm.pred==validation$label)
