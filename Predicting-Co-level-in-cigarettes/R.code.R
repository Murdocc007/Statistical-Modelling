###########Predicting CO level in cigarettes################
data = read.csv("D:/2-sem/Courses/Advanced Computational Methods for Data Science/Assgn-3/cigarettes-data.csv")
names(data)
head(data)
smp_size <- floor(0.6 * nrow(data))
train=sample(seq_len(nrow(data)), size = smp_size)
train.data <- data[train, ]
test.data <- data[-train, ]
head(train.data)

attach(train.data)

lm.fit = lm(CO~., data = train.data)
summary(lm.fit)

#Brand is an identifier

lm.fit1 = lm(CO~.-Brand, data = train.data)
summary(lm.fit1)
#Checking correlation between predictors
cor(Tar, Nicotine)
#high correlation between Tar and Nicotine
cor(Tar, Weight)
#correlation between Tar and weight
cor(Nicotine,Weight)
#correlation between Nicotine and weight
#All three predictor variables are correlated
lm.fit2 = lm(CO~Tar, data = train.data)
sm = summary(lm.fit2)
mse(sm)

#Good value of R-square

lm.train = lm(CO~Tar*Nicotine, data = train.data)
sm.train = summary(lm.train)
sm.train


mse <- function(summary) { 
  mse <- mean(summary$residuals^2)
  return(mse)
}

mse(sm.train)

plot(lm.train)


############Test Data###################
head(test.data)
detach(train.data)
attach(test.data)
lm.fit.test = lm(CO~Tar*Nicotine, data = test.data)
summary.test=summary(lm.fit.test)

mse(summary.test)

#####New-Model###########
lm.fit.test1 = lm(CO~Tar, data=test.data)
summary1.test = summary(lm.fit.test1)

mse(summary1.test)

