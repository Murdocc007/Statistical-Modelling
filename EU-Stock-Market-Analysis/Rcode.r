str(EuStockMarkets)
summary(EuStockMarkets)
head(EuStockMarkets)
mode(EuStockMarkets)
class(EuStockMarkets)
plot(EuStockMarkets)
data<-data.frame(EuStockMarkets)
attach(data)
par(mfrow=c(2,3))
plot(DAX,SMI)      
plot(DAX,CAC)      
plot(DAX,FTSE)
plot(SMI,CAC)
plot(SMI,FTSE)
plot(CAC,FTSE)