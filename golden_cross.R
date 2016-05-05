library(quantmod)
start = '2015-01-01'
end = '2015-12-31'
ticker = 'GOOG'
GOOG = getSymbols(ticker, src = 'yahoo', from = start, to = end, auto.assign=F)

chartSeries(GOOG)

df <- GOOG[, c("GOOG.Adjusted",  'GOOG.Volume')]
names(df) <- c('price', 'volume')
head(df)

df$return <- diff(log(df[,1]))

start = '2013-01-01'
end = '2014-01-01'
pepsi = getSymbols('PEP', from = start, to = end, auto.assign=F, adjust = T)
coke = getSymbols('COKE', from = start, to = end, auto.assign=F, adjust = T)
Sys.setenv(TZ = "UTC")
prices <- cbind(pepsi[,6], coke[,6])
head(prices)
price_changes <- apply(prices, 2, diff)
head(price_changes)


require('quantmod')
SPY <- getSymbols('SPY', from='2014-01-01', auto.assign = FALSE)

head(SPY$SPY.Close)

MA50 <- rollapply(SPY$SPY.Close, width = 50, FUN = mean, by.column = FALSE, align = 'right')
MA200 <- rollapply(SPY$SPY.Close, width = 200, FUN = mean, by.column = TRUE, align = 'right')

MA50200 <- cbind(MA50, MA200)
names(MA50200) <- c('MA50', 'MA200')
head(MA50200,10)

Lag

ifelse(MA50200$MA50 > MA50200$MA200, 1, 0)
Lag(ifelse(MA50200$MA50 > MA50200$MA200, 1, 0))

