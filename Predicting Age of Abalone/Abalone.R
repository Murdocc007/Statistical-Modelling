Data<- abalone
attach(Data)
head(Data)


colnames(Data) <- c('Sex', 'Length', 'Diameter', 'Height', 'Whole.Weight', 'Shucked.weight','Viscera.weight', 'Shell.weight', 'Rings')
#we check the correlations between variables
cor(abalone[,-1])

#This seems to be highly correlated data

#There are 4 different measures for weight i.e. Whole.weight, Shucked.weight, Viscera.weight and Shell.weight with high correlation between them. 
#And Whole.weight should be the easiest one to measure. 
#So, dropping out all the other measures.
Data = subset(Data, select = -c(Shucked.weight, Viscera.weight, Shell.weight))

str(Data)
#define some other measures of size that incorporate all 3 dimensions of length, diameter, height. 
#This allows us to describe 3 dimensions with 1 . We pick the euclidean norm of its size
Data$size.norm = sqrt(Length^2 + Diameter^2 + Height^2)

#Dropping Length, Diameter, Height
Data = subset(Data, select = -c(Length, Diameter, Height))

#Plot number of abalone with different rings.
plot(Rings)
#From the graph above, we can see that the range of the Rings is from 1 to 29,
#We divide this into two groups, young and old, with no of rings less than 10 as 0 and greater than 10 as 1
Age = c(rep(0, nrow(Data)))
for (i in 1:nrow(Data)) {
  if (Data[i, ]$Rings < 7) 
    Age[i] = 0
  if (Data[i, ]$Rings >= 7 & Data[i, ]$Rings <= 13) 
    Age[i] = 1
  if (Data[i, ]$Rings > 13) 
    Age[i] = 2
}
Data = cbind(Data, Age)
str(Data)

#creating training and test data
# 80% of the data is training data
smp_size <- floor(0.80 * nrow(Data))
train_ind=sample(seq_len(nrow(Data)), size = smp_size)
train <- Data[train_ind, ]
test <- Data[-train_ind, ]

#attaching the training data
attach(train)

#creating the model
model=lda(Age~.-Rings,data=train)
#predicting the values
model.predict=predict(model,test)
table(model.predict$class,test$Age)
#accuracy of LDA model
mean(model.predict$class==test$Age)

#creating the model
model=qda(Age~.-Rings,data=train)
#predicting the values
model.predict=predict(model,test)
table(model.predict$class,test$Age)

#accuracy of QDA model
mean(model.predict$class==test$Age)

