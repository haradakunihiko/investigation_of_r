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
calc_ma <- function (a) {
  return(a)  
}
tes <- head(SPY$SPY.Close)

diff(tes)

rollapply(tes, width = 3, FUN = calc_ma, by.column = TRUE, align = 'right')


