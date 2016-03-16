library(pastecs)
library(knitr)
library(astsa)
library(psych)

data <- read.csv("INJCJC.csv")
str(data)
dim(data)
head(data)
tail(data)

data.ts <- ts(data$INJCJC, frequency = 52, start = c(1990,1), end = c(2014,52))
summary(data.ts)
plot.ts(data.ts)

# Part d
INJCJC.time <- time(INJCJC)
head(cbind(INJCJC.time, data.ts), 10)

# Part e1
plot.ts(data.ts, xlab="Years", ylab="Number of Claims", main="Initial Jobless Claims")

#Part e2
hist(data.ts, xlab="Number of  Claims", main="Initial Jobless Claims", breaks=30)

#Part e3
acf(data.ts)

#Part e4
pacf(data.ts)

#Part e5
lag.plot(data.ts, lags=9, layout=c(3,3), diag=TRUE, disg.col="red",main="Autocorrelation")

#Part f1
mo52 = filter(data.ts, sides = 2, rep(1,52)/52)
mo5=filter(data.ts, sides = 2, rep(1, 5)/5)

plot(data.ts, main="INJCJC", pch=4, lty=5, lwd=1, xlab="Year", ylab="Number of Claims")
lines(mo52, lty=1, lwd=1.5, col="green")
lines(mo5, lty=1, lwd=1.5, col="blue")
# Add Legend
leg.txt <- c("Original Series", "5-Point Symmetric Moving Average", "53-Point Symmetric Moving Average")
legend("topright", legend=leg.txt, lty=c(1,1,1), col=c("black","green","blue"),
       bty='n', cex=1, merge = TRUE, bg=336)


#Part f2
wk = time(data.ts) - mean(time(data.ts))  
wk2 = wk^2 
wk3 = wk^3
cs = cos(2*pi*wk)  
sn = sin(2*pi*wk)
reg1 = lm(data.ts~wk + wk2 + wk3, na.action=NULL)
reg2 = lm(data.ts~wk + wk2 + wk3 + cs + sn, na.action=NULL)
plot(data.ts, main="Initial Jobless Claims (Weekly Series) and Regression Smoothing", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of claims per week")
lines(fitted(reg1), lty=1, lwd=1.5, col="green")
lines(fitted(reg2), lty=1, lwd=1.5, col="blue")
# Add Legend
leg.txt <- c("Original Series", "Cubic Trend Regression Smoothing", "Periodic Regression Smoothing")
legend("topright", legend=leg.txt, lty=c(1,1,1), col=c("black","green","blue"),
       bty='n', cex=1, merge = TRUE, bg=336)

#Parf f3
plot(data.ts, main="Initial Jobless Claims (Weekly Series) and Kernel Smoothing", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of claims per week")
lines(ksmooth(time(data.ts), data.ts, "normal", bandwidth=5/52),lty=1, lwd=1.5, col="green")
lines(ksmooth(time(data.ts), data.ts, "normal", bandwidth=2),lty=1, lwd=1.5, col="blue")
# Add Legend
leg.txt <- c("Original Series", "Kernel Smoothing: bandwidth=5/52", "Kernel Smoothing: bandwidth=2")
legend("topright", legend=leg.txt, lty=c(1,1,1), col=c("black","green","blue"),
       bty='n', cex=1, merge = TRUE, bg=336)

#Part f4
plot(data.ts, main="Initial Jobless Claims (Weekly Series) and Nearest Neighborhood Smoothing", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of claims per week")
lines(supsmu(time(data.ts), data.ts, span=.01),lty=1, lwd=1.5, col="green")
lines(supsmu(time(data.ts), data.ts, span=.5),lty=1, lwd=1.5, col="blue")
# Add Legend
leg.txt <- c("Original Series", "NN Smoothing: bandwidth=.01", "NN Smoothing: bandwidth=.5")
legend("topright", legend=leg.txt, lty=c(1,1,1), col=c("black","green","blue"),
       bty='n', cex=1, merge = TRUE, bg=336)

# Part f5
plot(data.ts, main="Initial Jobless Claims (Weekly Series) and LOWESS Smoothing", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of deaths per week")
lines(lowess(data.ts, f=.02),lty=1, lwd=1.5, col="green")
lines(lowess(data.ts, f=2/3),lty=1, lwd=1.5, col="blue")
# Add Legend
leg.txt <- c("Original Series", "LOWESS Smoothing: bandwidth=.02", "LOWESS Smoothing: bandwidth=2/3")
legend("topright", legend=leg.txt, lty=c(1,1,1), col=c("black","green","blue"),
       bty='n', cex=1, merge = TRUE, bg=336)


# Part f6
plot(data.ts, main="Initial Jobless Claims (Weekly Series) and Smoothing Splines", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of claims per week")
lines(smooth.spline(time(data.ts), data.ts, spar=0.05),lty=1, lwd=1.5, col="green")          
lines(smooth.spline(time(data.ts), data.ts, spar=0.9),lty=1, lwd=1.5, col="blue")  
# Add Legend
leg.txt <- c("Original Series", "Spline: Smoothing Parameter=.05", "Spline: Smoothing Parameter=0.8")
legend("topright", legend=leg.txt, lty=c(1,1,1), col=c("black","green","blue"),
       bty='n', cex=1, merge = TRUE, bg=336)
