library(splines)
library("gam", lib.loc="~/R/win-library/3.2")
data<- airfoil_self_noise
#renaming the predictor variable 
 names(data) <- c("Freq","Angle", "Chord","Velocity","Suction", "Pressure")
 head(data)


#no NA values in dataset
 sum(is.na(data))

#plotting the correlation matrix
cor(data)
#There is high correlation between angle and suction. So, remove suction.

 data <- data[, -c(5)]
 names(data)
 str(data)

#fitting smooth spline on Freq predictor using cross validation for the degree of freedom
set.seed(100)
smp_size <- floor(0.5 * nrow(data))
 train_ind=sample(seq_len(nrow(data)), size = smp_size)
 data.train <- data[train_ind, ]
 data.test <- data[-train_ind, ]
 head(data.train)
 timelims = range(data.train$Freq)
 time.grid = seq(from=timelims[1], to=timelims[2])
 plot(data.train$Freq,data.train$Pressure, xlim = timelims, col="darkgrey")
 title("Smooth Spline")
 train.fit=smooth.spline(data.train$Freq,data.train$Pressure, cv = TRUE)
 train.fit$df
#[1] 11.03061
#degree of freedom comes out to be around 11
 lines(train.fit,col="red", lwd=2)
 plot(data.train$Freq,data.train$Pressure, xlim = timelims, col="darkgrey")
 train.fit=smooth.spline(data.train$Freq,data.train$Pressure, df=9)
 train.fit=smooth.spline(data.train$Freq,data.train$Pressure, df=11)
 train.fit
 lines(train.fit,col="red", lwd=2)

 timelims = range(data.test$Freq)
 time.grid = seq(from=timelims[1], to=timelims[2])
 plot(data.test$Freq,data.test$Pressure, xlim = timelims, col="darkgrey")
 title("Smooth Spline")
 test.fit=smooth.spline(data.test$Freq,data.test$Pressure, df=11)
 test.fit
 lines(test.fit,col="red", lwd=2)


#using local regression for angle predictor
 anglelims = range(data$Angle)
 angle.grid = seq(from=anglelims[1], to=anglelims[2])
 angle=data$Angle
 pressure= data$Pressure
 plot(angle,pressure,xlim=timelims ,cex=.5,col="darkgrey ")
 title("Local Regression ")

 install.packages("bisoreg", dependencies=TRUE)
#cross validation for best model using local regression
 loess.wrapper(angle,pressure, span.vals = seq(0.3, .7, by = 0.05), folds = 5)
 fit=loess(pressure~angle,span=.4,data=data)
 fit2=loess(pressure~angle,span=.5,data=data)
 lines(angle.grid,predict(fit,data.frame(angle=angle.grid)),col="red",lwd=2)
 lines(angle.grid,predict(fit2,data.frame(angle=angle.grid)),col="green",lwd=2)
 legend("topright",legend=c("Span=0.4"," Span=0.5"), col=c("red","blue"),lty=1,lwd=2,cex=.8)

#final generalized additive model using smooth splines and local regression on Freq and Angle respectively. Chord and velocity are categorical variable.
 gam.m1=gam(Pressure ~ s(Freq,11)+lo(Angle,span=.5)+Chord+Velocity ,data=data)
 par(mfrow=c(1,4))
 plot.gam(gam.m1, se=TRUE, col="green")

