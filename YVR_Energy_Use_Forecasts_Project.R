mydata <- read.csv(file.choose(), header=TRUE)
library(fpp)
str(mydata) # Note that some variables are read by R as quantitative (integer or numeric), and some are read by R as categorical (factor). If R reads a variable incorrectly (like say you encoded a categorical variable with 1, 2, 3), then you need to change this before you start your analysis.

data <- ts(mydata$energy,frequency = 12, start = c(1997,1))

plot(data, ylab="Energy use (kilowatt hours)",xlab="Year",main = "YVR energy use within past 14 years")
seasonplot(data)

train <- window(data,end=2008-.01) # Select the training sert
train
testse <- window(data,start=2008) 
testse


plot(train, ylab="Energy use (kilowatt hours)",xlab="Year",main = "YVR energy use forecasts with basic methods",xlim=c(1997,2011),ylim=c(4900,9500))
lines(testse)
lines(meanf(train,h=36)$mean,col=4)
lines(rwf(train,drift=TRUE,h=36)$mean,col=3)
lines(rwf(train,h=36)$mean,col=2)
lines(snaive(train,h=36)$mean,col=6)
legend("topleft",lty=1,col=c(4,2,3,6), legend=c("Mean method","Naive method","Drift method","Seasonal naive method"))

fit <- stl(train, t.window=20, s.window="periodic", robust=TRUE)
plot(fit,main = "STL decomposition of the training set")

rbind(accuracy(meanf(train,h=36), testse)[2,c(2,3,5,6)],accuracy(rwf(train,drift=TRUE,h=36), testse)[2,c(2,3,5,6)],
      accuracy(rwf(train,h=36), testse)[2,c(2,3,5,6)],
      accuracy(snaive(train,h=36), testse)[2,c(2,3,5,6)])

fit1 <- ets(train,model="AAA",damped=FALSE) #ETS (A, N, N), where errors are additive, there is no trend and no seasonality
summary(fit1)
fit3.1 <- hw(train,seasonal="additive",h=36)
plot(train, ylab="Energy use (kilowatt hours)",xlab="Year",main = "YVR energy use within past 14 years",xlim=c(1997,2011),ylim=c(4900,9500))
lines(testse)
lines(fit3.1$mean, col="blue",lwd=3)
lines(fitted(fit1), col="blue") 

accuracy(fit1)
accuracy(fit3.1-4.86032,testse)

res <- residuals(fit3.1) # residuals from the multiplicative damped trend method fitted to the training set

plot(res, main="Residuals from Holt-Winter additive seasonal method", ylab="", xlab="Month")
abline(0,0, lty=2)
Acf(res, main="ACF of residuals")
Box.test(res, type="Ljung", lag=24)
hist(res, nclass="FD", main="Histogram of residuals")
mean(res)


ns <- nsdiffs(train); ns
nd <- ndiffs(train); nd
xstar <- diff(train,lag=frequency(train),differences=1)
plot(xstar,main="Energy use after seasonal differencing",ylab="Energy use (kilowatt hours)")
Acf(xstar)

nd <- ndiffs(train); nd
xstar <- diff(train,differences=1)
plot(xstar)
par( mfrow = c( 1, 2 ) )

Acf(xstar,main="ACF Plot")
Pacf(xstar,main="PACF Plot")


beer.fit.3 <- auto.arima(train, stepwise=FALSE, approximation=FALSE, trace=TRUE, ic="aicc")
summary(beer.fit.3)
Acf(residuals(beer.fit.3))
Pacf(residuals(beer.fit.3))

fit.3 <- Arima(train, order=c(0,1,3), seasonal=c(0,1,1), include.drift=FALSE)
summary(fit.3)

fit.4 <- Arima(train, order=c(0,1,3), seasonal=c(1,1,0), include.drift=FALSE)
summary(fit.4)

energy.forecasts <- forecast(fit.3, h=36)$mean
energy.forecasts

plot(train, ylab="Energy use (kilowatt hours)",xlab="Year",main = "YVR energy use within past 14 years",xlim=c(1997,2011),ylim=c(4900,9500))
lines(testse)
lines(energy.forecasts, col="blue",lwd=3)
lines(fitted(fit.3), col="blue") 

accuracy(forecast(fit.3, h=36),testse)
res <- residuals(fit.3)
plot(res, main="Residuals from ARIMA method", ylab="", xlab="Month")
abline(0,0, lty=2)
Acf(res, main="ACF of residuals of ARIMA method")
Box.test(res, type="Ljung", lag=24)
hist(res, nclass="FD", main="Histogram of residuals")
mean(res)

par( mfrow = c( 2, 1 ) )
plot(train, ylab="Energy use (kilowatt hours)",xlab="Year",main = "YVR energy use forecasts by ARIMA Model",xlim=c(1997,2011),ylim=c(4900,9500))
lines(testse)
lines(energy.forecasts, col="blue",lwd=3)
lines(fitted(fit.3), col="blue") 

plot(train, ylab="Energy use (kilowatt hours)",xlab="Year",main = "YVR energy use forecasts by ETS Model",xlim=c(1997,2011),ylim=c(4900,9500))
lines(testse)
lines(fit3.1$mean-4.86032, col="blue",lwd=3)
lines(fitted(fit1), col="blue") 

forecastfinal <- hw(data,seasonal="additive",h=36)
plot(data, ylab="Energy use (kilowatt hours)",xlab="Year",main = "YVR energy use forecasts",xlim=c(1997,2014),ylim=c(4900,11000))
lines(forecastfinal$mean, col="red")

