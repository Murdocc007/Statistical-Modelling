data=read.csv("D:/2-sem/Courses/Advanced Computational Methods for Data Science/Assgn-7/Assign_7-Data/Assign_7-Data/batteries2.csv", header=TRUE)

temp=factor(data$Brand,labels=c('0','1'))
data$Brand=temp

edata=data[which(data$Brand=='0'),]
udata=data[which(data$Brand=='1'),]

#edata-energizer

# 75% of the data is training data
smp_size <- floor(0.75 * nrow(edata))
train_ind=sample(seq_len(nrow(edata)), size = smp_size)
edata.train <- edata[train_ind, ]
edata.test <- edata[-train_ind, ]
#######training data#########
timelims = range(edata.train$Time)
time.grid = seq(from=timelims[1], to=timelims[2])
plot(edata.train$Time,edata.train$Voltage, xlim = timelims, col="darkgrey")
title("Smooth Spline")
e.train.fit=smooth.spline(edata.train$Time,edata.train$Voltage, cv = TRUE)
e.train.fit$df
#df=23.86
lines(e.train.fit,col="red", lwd=2)
#overfitted model
#reduce the degree of freedom so that the model does not overfit
plot(edata.train$Time,edata.train$Voltage, xlim = timelims, col="darkgrey")
e.train.fit=smooth.spline(edata.train$Time,edata.train$Voltage, df=6)
lines(e.train.fit,col="red", lwd=2)
#######test data##########
timelims = range(edata.test$Time)
time.grid = seq(from=timelims[1], to=timelims[2])
plot(edata.test$Time,edata.test$Voltage, xlim = timelims, col="darkgrey")
title("Smooth Spline")
e.test.fit=smooth.spline(edata.test$Time,edata.test$Voltage, df=6)
lines(e.test.fit,col="red", lwd=2)


#udata-ultracell


# 75% of the data is training data
smp_size <- floor(0.75 * nrow(udata))
train_ind=sample(seq_len(nrow(udata)), size = smp_size)
udata.train <- udata[train_ind, ]
udata.test <- udata[-train_ind, ]

#######training data#########
timelims = range(udata.train$Time)
time.grid = seq(from=timelims[1], to=timelims[2])
plot(udata.train$Time,udata.train$Voltage, xlim = timelims, col="darkgrey")
title("Smooth Spline")
u.train.fit=smooth.spline(udata.train$Time,udata.train$Voltage, cv = TRUE)
u.train.fit$df
#df=20.80
lines(u.train.fit,col="red", lwd=2)
#overfitted model
#reduce the degree of freedom so that the model does not overfit
plot(udata.train$Time,udata.train$Voltage, xlim = timelims, col="darkgrey")
u.train.fit=smooth.spline(udata.train$Time,udata.train$Voltage, df=6)
lines(u.train.fit,col="red", lwd=2)

#######test data##############
timelims = range(udata.test$Time)
time.grid = seq(from=timelims[1], to=timelims[2])
plot(udata.test$Time,udata.test$Voltage, xlim = timelims, col="darkgrey")
title("Smooth Spline")
u.test.fit=smooth.spline(udata.test$Time,udata.test$Voltage, df=6)
lines(u.test.fit,col="red", lwd=2)
