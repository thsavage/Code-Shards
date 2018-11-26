library(quantmod)
library(stargazer)

# Start with AAPL which trades on NASDAQ
getSymbols(c('AAPL','^IXIC'), from="2006-01-01", to="2017-12-31")
AAPL = AAPL$AAPL.Adjusted
NASDAQ = IXIC$IXIC.Adjusted
plot(AAPL, main="AAPL ($/Share)", col = "darkblue")
plot(NASDAQ, main="NASDAQ Index", col = "darkblue")
data = merge(as.zoo(AAPL), as.zoo(NASDAQ))  ## Merge into single time-series dataset
names = c("AAPL", "NASDAQ")
colnames(data) = names

data.level <- as.xts(data)  ## Levels 
data.returns <- diff(log(data.level), lag=1)  ## Log returns
data.returns <- na.omit(data.returns)  ## Dump missing values

summary(data.returns$AAPL)
hist(data.returns$AAPL, breaks=100, col="darkblue", freq=F, 
     main="Histogram of AAPL Daily Returns (2006-2017)", xlab="Daily Returns", 
     xlim=c(-.2, .2))  
abline(v=0, col="red")
# Histogram of returns.  Do they look normally distributed?  Lots of work in finance depends on normally distributed returns.

summary(data.returns$NASDAQ)
hist(data.returns$NASDAQ, breaks=100, col="darkblue", freq=F, 
     main="Histogram of NASDAQ Daily Returns (2006-2017)", xlab="Daily Returns",
     xlim=c(-.2, .2))  
abline(v=0, col="red")

plot.ts(y=data.returns$NASDAQ, x=data.returns$AAPL, pch=16, 
        col="darkblue", main="CAPM Data", xlab="Returns of NASDAQ", 
        xlim=c(-.1,.1), ylab="Returns of AAPL", ylim=c(-.1,.1))  ## time series plot in R  
grid(lw = 2)

abline(lm(data.returns$AAPL ~ data.returns$NASDAQ), col="red")  ## I added the best fit line so the graph looks similar to that presented in class for Apple.library(ggplot2)

capm.ols = lm(data.returns$AAPL ~ data.returns$NASDAQ)
stargazer(capm.ols, type="text", title="CAPM Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)


# Let's replicate with Amazon.
getSymbols(c('AMZN','^IXIC'), from="2006-01-01", to="2017-12-31")
AMZN = AMZN$AMZN.Adjusted
NASDAQ = IXIC$IXIC.Adjusted
plot(AMZN, main="AMZN ($/Share)", col = "darkblue")
plot(NASDAQ, main="NASDAQ Index", col = "darkblue")
data = merge(as.zoo(AMZN), as.zoo(NASDAQ))  ## Merge into single time-series dataset
names = c("AMZN", "NASDAQ")
colnames(data) = names

data.level = as.xts(data)  ## Levels 
data.returns = diff(log(data.level), lag=1)  ## Log returns
data.returns = na.omit(data.returns)  ## Dump missing values

summary(data.returns$AMZN)
hist(data.returns$AMZN, breaks=100, col="darkblue", freq=F, 
     main="Histogram of AMZN Daily Returns (2006-2017)", xlab="Daily Returns", 
     xlim=c(-.2, .2))  
abline(v=0, col="red")
# Histogram of returns.  Do they look normally distributed?  Lots of work in finance depends on normally distributed returns.

summary(data.returns$NASDAQ)
hist(data.returns$NASDAQ, breaks=100, col="darkblue", freq=F, 
     main="Histogram of NASDAQ Daily Returns (2006-2017)", xlab="Daily Returns",
     xlim=c(-.2, .2))  
abline(v=0, col="red")

plot.ts(y=data.returns$NASDAQ, x=data.returns$AMZN, pch=16, 
        col="darkblue", main="CAPM Data", xlab="Returns of NASDAQ", 
        xlim=c(-.1,.1), ylab="Returns of AMZN", ylim=c(-.1,.1))  ## time series plot in R  
grid(lw = 2)
abline(lm(data.returns$AMZN ~ data.returns$NASDAQ), col="red")  ## I added the best fit line so the graph looks similar to that presented in class for Apple.library(ggplot2)

capm.ols = lm(data.returns$AMZN ~ data.returns$NASDAQ)
stargazer(capm.ols, type="text", title="CAPM Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)


# Let's do another using interest rates.
threemonth <- drop_na(fredr(series_id = "DGS3MO", observation_start = as.Date("2008-01-01")))
tenyear <- drop_na(fredr(series_id = "DGS10", observation_start = as.Date("2008-01-01")))
plot(threemonth$value, tenyear$value, 
     xlab=TeX("3 Month Yields"), ylab=TeX("10 Year Yields"), 
     main="Daily Interest Rates Since 2008", pch=16, col='blue')
grid(lw = 2)
abline(lm(tenyear$value ~ threemonth$value), col='red')

# You really want to focus on contemporaneous changes in rates.
tenyear$change = Delt(tenyear$value, type=c("arithmetic"))
threemonth$change = Delt(threemonth$value, type=c("arithmetic"))
tenyear$change[is.na(tenyear$change)] = 0
threemonth$change[is.na(threemonth$change)] = 0
tenyear$change[is.infinite(tenyear$change)] = 0
threemonth$change[is.infinite(threemonth$change)] = 0

plot(threemonth$change, tenyear$change, 
     xlab=TeX("3 Month Yield Changes"), ylab=TeX("10 Year Yields"), 
     main="Daily Interest Rate Changes Since 2008", pch=16, col='blue')
grid(lw = 2)
abline(lm(tenyear$change ~ threemonth$change), col='red')

capm.ols = lm(tenyear$change ~ threemonth$change)
stargazer(capm.ols, type="text", title="CAPM Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

# Let's Do Another One!
# It is not CAPM, but it is fun.
# Coke versus Pepsi.

getSymbols(c('PEP','COKE'), from="2006-01-01", to="2017-12-31")
PEP = PEP$PEP.Adjusted
COKE = COKE$COKE.Adjusted
plot(PEP, main="PEP ($/Share)", col = "darkblue")
plot(COKE, main="COKE ($/Share)", col = "darkblue")
data = merge(as.zoo(PEP), as.zoo(COKE))  ## Merge into single time-series dataset
names = c("PEP", "COKE")
colnames(data) = names

data.level = as.xts(data)  ## Levels 
data.returns = diff(log(data.level), lag=1)  ## Log returns
data.returns = na.omit(data.returns)  ## Dump missing values

summary(data.returns$PEP)
hist(data.returns$PEP, breaks=100, col="darkblue", freq=F, 
     main="Histogram of PEP Daily Returns (2006-2017)", xlab="Daily Returns", 
     xlim=c(-.2, .2))  
abline(v=0, col="red")
# Histogram of returns.  Do they look normally distributed?  Lots of work in finance depends on normally distributed returns.

summary(data.returns$COKE)
hist(data.returns$COKE, breaks=100, col="darkblue", freq=F, 
     main="Histogram of COKE Daily Returns (2006-2017)", xlab="Daily Returns",
     xlim=c(-.2, .2))  
abline(v=0, col="red")

plot.ts(y=data.returns$PEP, x=data.returns$COKE, pch=16, 
        col="darkblue", main="CAPM Data", xlab="Returns of COKE", 
        xlim=c(-.1,.1), ylab="Returns of PEP", ylim=c(-.1,.1))  ## time series plot in R  
grid(lw = 2)
abline(lm(data.returns$PEP ~ data.returns$COKE), col="red")  ## I added the best fit line so the graph looks similar to that presented in class for Apple.library(ggplot2)

capm.ols = lm(data.returns$PEP ~ data.returns$COKE)
stargazer(capm.ols, type="text", title="Coke Versus Pepsi Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

# In-class assignment:  Do CAPM for IBM stock.

