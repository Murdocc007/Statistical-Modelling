myopia.data = read.csv("D:/2-sem/Courses/Advanced Computational Methods for Data Science/Assgn-4/Assign_4-Data/myopia.csv")
names(myopia.data)
#SPHEQ is a quantitative variable, but if its values are divided into intervals
#then it helps in predicting the response variable.
#So we have converted SPHEQ as a quantitative variable into SPHEQCAT as a categorical variable.
myopia.data$SPHEQCAT <- cut(myopia.data$SPHEQ, c(-10,-0.25, 1, 10))
names(myopia.data)
#Dropping ID as it is an identifier
drops = c("ID")
myopia.data = myopia.data[,!names(myopia.data)%in%drops]
names(myopia.data)
#Dividing data into training and test data
smp_size <- floor(0.75 * nrow(myopia.data))
train_ind=sample(seq_len(nrow(myopia.data)), size = smp_size)
myopic.train <- myopia.data[train_ind, ]
myopic.test <- myopia.data[-train_ind, ]
##################################Training-Data##############################
attach(myopic.train)
glm.fit <- glm(MYOPIC~., data = myopic.train, family = "binomial")
summary(glm.fit)
#using step function to remove irrelevant predictor variables
step.fit <- step(glm.fit, direction = "both")
glm.fit2 <- glm(MYOPIC~GENDER+DADMY+AL+SPORTHR+VCD+MOMMY+SPHEQCAT, data = myopic.train, family = "binomial")
summary(glm.fit2)
#AIC values is least with the above predictor variables and p-value are also singnificant
#Predicting the response variable by mapping predicted values of response variable with observed values
glm.probs = predict(glm.fit2, type = "response")
MYOPIC <- as.factor(MYOPIC)
contrasts(MYOPIC)
glm.pred = rep("0", 463)
glm.pred[glm.probs >0.5] = "1"
table(glm.pred, MYOPIC)
mean(glm.pred == MYOPIC)
##mean is 0.88 for training data.
###############################Test Data############################
detach(myopic.train)
attach(myopic.test)
glm.test <- glm(MYOPIC~GENDER+DADMY+AL+SPORTHR+VCD+MOMMY+SPHEQCAT, data = myopic.test, family = "binomial")
summary(glm.test)
#AIC = 98.519
glm.testprobs = predict(glm.test, type = "response")
MYOPIC <- as.factor(myopic.test$MYOPIC)
contrasts(MYOPIC)
glm.testpred = rep("0", 155)
glm.testpred[glm.testprobs > 0.5] = "1"
table(glm.testpred, MYOPIC)
mean(glm.testpred == MYOPIC)
plot(glm.test)
##mean comes out to be 0.88 which means the accuracy of our model on the test data is 91% which is very good.
################LDA#########################
lda.fit <- lda(MYOPIC~GENDER+DADMY+AL+SPORTHR+VCD+MOMMY+SPHEQCAT, data = myopic.train)
lda.pred <- predict(lda.fit, myopic.test)
names(lda.pred)
table(lda.pred$class, myopic.test$MYOPIC)
mean(lda.pred$class == myopic.test$MYOPIC)
##mean comes out as 0.8774194 which is slightly lower than the accuracy obtained using logistic regression.
###############QDA###########################
qda.fit <- qda(MYOPIC~GENDER+DADMY+AL+SPORTHR+VCD+MOMMY+SPHEQCAT, data = myopic.train)
qda.class <- predict(qda.fit, myopic.test)$class
table(qda.class, myopic.test$MYOPIC)
mean(qda.class == myopic.test$MYOPIC)
## mean comes out as 0.8774194 which is comparable with the accuracy obtained using linear discriminant analysis.
##################KNN########################
myopia.data = read.csv("D:/2-sem/Courses/Advanced Computational Methods for Data Science/Assgn-4/Assign_4-Data/myopia.csv")
names(myopia.data)
myopia.data$SPHEQCAT <- cut(myopia.data$SPHEQ, c(-10,-0.25, 1, 10))
names(myopia.data)
drops = c("ID")
myopia.data = myopia.data[,!names(myopia.data)%in%drops]
names(myopia.data)
attach(myopia.data)
train = 1:463
test = 464:618
train.x = cbind(GENDER,DADMY,AL,SPORTHR,VCD,MOMMY,SPHEQCAT)[train]
test.x = cbind(GENDER,DADMY,AL,SPORTHR,VCD,MOMMY,SPHEQCAT)[test]
train.y = MYOPIC[train]
set.seed(1)
knn.pred = knn(data.frame(train.x),data.frame(test.x),train.y, k = 1)
test.myopia <- MYOPIC[test]
cm <- confusionMatrix(test.myopia,knn.pred)
cm <- confusionMatrix(test.myopia,knn.pred)
acc <- cm$overall['Accuracy']
acc
Accuracy 
0.8580645

