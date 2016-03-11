

data<- AmesHousing
summary(data)
str(data)
attach(data)
sum(is.na(Fireplace.Qu)) 
nrow(data)
names(data)

# removing alley variable 
data <- data[, -c(8)] 
# removing id and 
data <- data[, -c(1:2)] 
#removing "Pool.QC","Fence","Misc.Feature", "Fireplace.Qu" as they have high proportions of NA
data[,c("Pool.QC","Fence","Misc.Feature","Fireplace.Qu")] <- list(NULL)

#counting no of NA in each column
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count
data<- na.omit(data)
dim(data)
#removing outliers as suggested in dataset documentation
data = data[which(with(data,Gr.Liv.Area<=4000)),]
dim(data)

#forward
regfit.fwd=regsubsets(SalePrice~.-SalePrice,data=data,nvmax=76,method="forward")
names(summary(regfit.fwd))

fwd.summary=summary(regfit.fwd)
par(mfrow=c(2,2))
plot(fwd.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(fwd.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")
which.max(fwd.summary$adjr2)
points(77,fwd.summary$adjr2[77], col="red",cex=2,pch =20)

plot(fwd.summary$cp ,xlab="Number of Variables ",ylab="Cp",type="l")
fwd.summary$cp
which.min(fwd.summary$cp )
points(77,fwd.summary$cp [77],col="red",cex=2,pch=20)
which.min(fwd.summary$bic )
plot(fwd.summary$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
points(56,fwd.summary$bic [56],col="red",cex=2,pch=20)
coef(regfit.fwd ,56)


#backward
regfit.bwd=regsubsets (SalePrice???. -SalePrice,data=data , nvmax=82,method="backward")

bwd.summary=summary(regfit.bwd)
par(mfrow=c(2,2))
plot(bwd.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(bwd.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")
which.max(bwd.summary$adjr2)
points(83,bwd.summary$adjr2[83], col="red",cex=2,pch =20)


plot(bwd.summary$cp ,xlab="Number of Variables ",ylab="Cp",type="l")
bwd.summary$cp
which.min(bwd.summary$cp )
points(83,bwd.summary$cp [83],col="red",cex=2,pch=20)
which.min(bwd.summary$bic )
plot(bwd.summary$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
points(54,bwd.summary$bic [54],col="red",cex=2,pch=20)
coef(regfit.bwd ,54)
#since bic chooses 54 predictors and its a simpler model, we choose 54 predictors from backward selection


# Ridge Regression
x=model.matrix(data$SalePrice~.,data)[,-75]
y=data$SalePrice

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]