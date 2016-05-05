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
df$cuts <- cut(abs(df$return)), breaks = c(0,0.02,0.04,0.25), include.lowest = TRUE)

start = '2013-01-01'
end = '2014-01-01'
pepsi = getSymbols('PEP', from = start, to = end, auto.assign=F, adjust = T)
coke = getSymbols('COKE', from = start, to = end, auto.assign=F, adjust = T)
Sys.setenv(TZ = "UTC")
prices <- cbind(pepsi[,6], coke[,6])

price_changes <- apply(prices, 2, diff)
head(price_changes)
plot(price_changes, cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
grid()

ans <- lm(price_changes[,2] ~ price_changes[,1])
ans$coefficients[2]

ans <- lm(price_changes[,1] ~ price_changes[,2])
ans$coefficients[2]
#http://quanttrader.info/public/CRUG_MeetUp.pdf
# 総最小二乗法

start = '2009-01-01'
end = '2012-12-31'
SPY = getSymbols('SPY', from = start, to = end, auto.assign=F, adjust = T)
AAPL = getSymbols('AAPL', from = start, to = end, auto.assign=F, adjust = T)

tail(AAPL)

x <- diff(as.numeric(SPY[,4]))
y <- diff(as.numeric(AAPL[,4]))
plot(x,y)
abline(lm(y ~ x))
abline(lm(x ~ y), lty=2)
grid()

#PCA分析
r <- prcomp( ~ x + y)
r
r$rotation[2,1]
r$center
slope <- r$rotation[2,1] / r$rotation[1,1]
intercept <- r$center[2] - slope * r$center[1]
abline(a = intercept, b = slope, lty = 3)


calculate_spread = function (x,y,beta) {
  return (y - beta * x)
}

calculate_beta_and_lebel = function (x,y,start_date, end_date) {
  time_range <- paste(start_date, "::", end_date, sep = "")
  x <- x[time_range]
  y <- y[time_range]
  dx <- diff(x)
  dy <- diff(y)
  r <- prcomp(~ dx + dy)
  beta <- r$rotation[2,1] / r$rotation[1,1]
  spread <- calculate_spread(x,y,beta)
  names(spread) <- "spread"
  level <- mean(spread, na.rm = TRUE)
  outL <- list()
  outL$spread <- spread
  outL$beta <- beta
  outL$level <- level
  return (outL)
}

calculate_buy_sell_signals <- function(spread,beta,level,lower_threshold, upper_threshold) {
  buy_signals <- ifelse(spread <= level - lower_threshold, 1, 0)
  sell_signals <- ifelse(spread>=level + upper_threshold, 1, 0)
  
  output <- cbind(spread, buy_signals, sell_signals)
  colnames(output) <- c("spread", "buy_signals", "sell_signals")
  return (output)
}
start_date <- '2011-01-01'
end_date <- '2011-12-31'
head(SPY)
head(AAPL)
x<- SPY[, 4]
y <- AAPL[, 4]
results <- calculate_beta_and_lebel(x,y,start_date, end_date)

results$beta
results$level
results$spread
plot(results$spread)

start_date_out_sampple <- "2012-01-01"
end_date_out_sample <- "2012-10-22"
range <- paste(start_date_out_sampple, "::",  end_date_out_sample, sep = "")
spread_out_of_sample <- calculate_spread(x[range], y[range], results$beta)
plot(spread_out_of_sample)
abline(h = results$level, lwd = 2)


window_length <- 10
start_date <- "2011-01-01"
end_date <- "2011-12-31"
range <- paste(start_date, '::', end_date, sep='')
range
x <- SPY[range, 4]
y <- AAPL[range, 4]


dF <- cbind(x,y)

names(dF) <- c("x", "y")

run_regression <- function(dF) {
  return (coef(lm(y ~ x - 1, data = as.data.frame(dF))))
}
rolling_beta <- function(z, width) {
  rollapply(z, width = width, FUN = run_regression, by.column = FALSE, align = 'right')
}
betas <- rolling_beta(diff(dF), 10)
data <- merge(betas, dF)
data$spread <- data$y - lag(betas, 1) * data$x

returns <- diff(dF)/ dF
return_beta <- rolling_beta(returns, 10)
data$spreadR <- diff(data$y) / data$y - return_beta * diff(data$x) / data$x
tail (data)
plot(data)
