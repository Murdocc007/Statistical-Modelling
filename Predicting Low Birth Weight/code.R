lowbwt.data=read.csv("/home/aditya/Desktop/Aditya/Semper/Assignment3/lowbwt.csv",header = T)

# dropping the bwt and ID variable
drops=c("BWT","ID")
lowbwt.data=lowbwt.data[,!names(lowbwt.data) %in% drops]


# 75% of the data is training data
smp_size <- floor(0.75 * nrow(lowbwt.data))
train_ind=sample(seq_len(nrow(lowbwt.data)), size = smp_size)
train <- lowbwt.data[train_ind, ]
test <- lowbwt.data[-train_ind, ]

#attaching the training data
attach(train)

#attaching all the attributes
model=glm(LOW~.,data=train,family = binomial)
summary(model)

#stepwise selection both directions
model=step(model,direction = 'both')
summary(model)

#get the logistic probability model
glm.probs=predict(model,newdata = data.frame(LWT=test$LWT,PTL=test$PTL,RACE=test$RACE,SMOKE=test$SMOKE,HT=test$HT),type="response")

#intializing the glm.predict data frame
glm.predict=rep(0,length(test$LOW))

#converting the test$LOw attribute to a factor in order to make a contrast table
test$LOW=as.factor(test$LOW)
contrasts(test$LOW)

#assigining label 1 to all the tuples which have probability greater than 0.5 
glm.predict[glm.probs>0.5]=1

#creating the table to compute false positives and false negatives
table(glm.predict,LOW=test$LOW)

#accuracy
mean(glm.predict==test$LOW)


#checking the correlation between the attributes
cor(train)