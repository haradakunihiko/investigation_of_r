library(quantmod)
library(xts)
library(moments)  # to get skew & kurtosis

# googleだと取得できないのでyahooにする
getSymbols("SPY", src="yahoo", from = "2004-01-01")

is.xts(SPY) # returns TRUE

head(SPY)
tail(SPY)

SPY.Close <- SPY[, "SPY.Close"]
is.xts(SPY.Close) # returns TRUE



x1 <- SPY['2006-01/2007-12'] # store the output in x1, etc
x2 <- SPY['/2005-07']
x3 <- SPY['2010']
x4 <- SPY['2010-12']
x5 <- to.period(SPY['2010'], 'months')
x6 <- to.period(SPY['2010'], 'quarters')
head(x5)
head(x6)

x7 <- to.monthly(SPY['2010'])
x8 <- to.quarterly(SPY['2010'])
head(x7)
head(x8)

head(SPY.Close)

plot(SPY.Close, main = "Closing Daily Prices for SP 500 Index ETF (SPY)",
     col = "red",xlab = "Date", ylab = "Price", major.ticks='years',
     minor.ticks=FALSE)

SPY.ret <- diff(log(SPY.Close), lag = 1)
SPY.ret <- SPY.ret[-1] # Remove resulting NA in the 1st position

plot(SPY.ret, main = "Closing Daily Prices for SP 500 Index ETF (SPY)",
     col = "red", xlab = "Date", ylab = "Return", major.ticks='years',
     minor.ticks=FALSE)

statNames <- c("mean", "std dev", "skewness", "kurtosis")
SPY.stats <- c(mean(SPY.ret), sd(SPY.ret), skewness(SPY.ret), kurtosis(SPY.ret))
names(SPY.stats) <- statNames
SPY.stats
