library(quantmod)

# 準備編
start = '2015-01-01'
end = '2015-12-31'
ticker = 'GOOG'
GOOG = getSymbols(ticker, src = 'yahoo', from = start, to = end, auto.assign=F)
summary(GOOG)
head(GOOG)
str(GOOG)
GOOG['2014-01/2014-12']
GOOG['2014-01::']
GOOG['2014-01']
GOOG['2014-01-30']

apply.daily(GOOG[, 6], max)
apply.weekly(GOOG[, 6], max)
apply.monthly(GOOG[, 6], max)
apply.quarterly(GOOG[, 6], max)
apply.yearly(GOOG[, 6], max)


apply.monthly(GOOG, function(y)sapply(y, max))

tail(rollapply(GOOG, 120, mean))
tail(rollapply(GOOG, 1, function(x_)sapply(x_, )))
tail(aggregate(GOOG, as.yearmon, last))

# 必要な技術
# plot

plot(GOOG[,'GOOG.Close'], main='GOOG Closing Prices')


# 必要な技術
# テーブルのマージ
# 移動平均を計算する
x <- c(1, 2, 3, 1, 2, 9, 4, 2, 6, 1)
mutate(x)
filter(x, c(1,1,1,1), sides=1)
rep(1/10,10)

GOOG.index = index(GOOG)
GOOG.MVA = xts(filter(GOOG[,'GOOG.Close'], rep(1/10, 10), sides=1), order.by = GOOG.index)
GOOG.MVA20 = xts(filter(GOOG[,'GOOG.Close'], rep(1/20, 20), sides=1), order.by = GOOG.index)
GOOG.MVA50 = xts(filter(GOOG[,'GOOG.Close'], rep(1/50, 50), sides=1), order.by = GOOG.index)
GOOG.MVA100 = xts(filter(GOOG[,'GOOG.Close'], rep(1/100, 100), sides=1), order.by = GOOG.index)
head(GOOG.MVA)

# 移動平均
head(SMA(GOOG[,'GOOG.Close'], 10), 30)

head(GOOG.MVA, 30)
GOOG = merge(GOOG, GOOG.MVA, all=TRUE)
GOOG = merge(GOOG, GOOG.MVA20, all=TRUE)
GOOG = merge(GOOG, GOOG.MVA50, all=TRUE)
GOOG = merge(GOOG, GOOG.MVA100, all=TRUE)


head(GOOG)
head(GOOG[,c('GOOG.Close', 'GOOG.MVA')])


# 重ねあわせ
plot.zoo(GOOG[,c('GOOG.Close', 'GOOG.MVA20', 'GOOG.MVA100')], plot.type = "single", col = c("red", "blue", "green"))


# 追加してplotするには？

# 前日比の取得

head(GOOG[,'GOOG.Close'])

# 階差
head(diff(GOOG[,'GOOG.Close']))
head(GOOG[,'GOOG.Close'])
head(lag.xts(GOOG[,'GOOG.Close'], 1))
head(lag(GOOG[,'GOOG.Close'], 1))



c(1,2,3,4)
lag(c(1,2,3,4),1)
diff(c(1,2,4,7))

head(GOOG[,'GOOG.Close'] / lag.xts(GOOG[,'GOOG.Close'], 1))
# ここが違う！
GOOG.PREV_DAY_RATE = xts(GOOG[,'GOOG.Close'] / lag.xts(GOOG[,'GOOG.Close'], 1) - 1)
GOOG.PREV_DAY_RATE <- na.omit(GOOG.PREV_DAY_RATE)
GOOG = merge(GOOG, GOOG.PREV_DAY_RATE, all=TRUE)
# allってなに？
head(GOOG.PREV_DAY_RATE)


plot(GOOG.PREV_DAY_RATE)

hist(GOOG.PREV_DAY_RATE, breaks = 100 )
#lines(density(GOOG[,'GOOG.Close.3']), col = "orange", lwd = 2)

tail(GOOG)
rbind(GOOG, c(1,1,1,1,1,1))


days <- 365
dt <- 1/days
mean(GOOG[,'GOOG.Close'])
mu <- mean(GOOG.PREV_DAY_RATE)
mu

# あってる？
sigma <- sd(GOOG.PREV_DAY_RATE)
sigma

# 世紀分布に従う乱数
rnorm(10)

shock <- sigma * rnorm(10) * sqrt(dt)

rnorm(1)

res = rep(0, days)
for (i in 2:(days -1)) {
 res[i] <- i
}
mu
mu * sigma

rnorm(1)
sqrt(dt)
sigma

sigma * rnorm(1) * sqrt(dt)

motecarlo <- function (startPrice, days, mu, sigma) {
  dt = 1/days
  price = rep(1, days)
  price[1] <- startPrice
  drift <- mu * dt
  for (i in 2:(days)) {
    shock = sigma * rnorm(1) * sqrt(dt)
    price[i] <- price[i - 1] + price[i - 1] *( drift + shock)
  }
  return (price);
}

fut <- c()
for(i in 1:10) {
  fut <-cbind(fut, motecarlo(524, 365,mu,sigma))
}


days <- 365
num <- 10000
simu <- rep(0,num)
for(i in 1:num) {
  simu[i] <-motecarlo(524, days,mu,sigma)[days -1]
}
simu
hist(simu, breaks = 100)
mean(simu)
quantile(simu, .01)

# zooパッケージ
fut.zoo = as.zoo(fut)
plot(x = fut.zoo, ylab = "Cumulative Return", main = "Cumulative Returns",
     col = tsRainbow, screens = 1)

tsRainbow <- rainbow(ncol(fut))


plot.zoo(fut, plot.type = "single", col = tsRainbow,  xlab ="Days", ylab="Price")
legend(x = "topleft", legend = c("1", "2", "3", "4", "5"), 
       lty = 1,col = tsRainbow)

# 合成
plot(fut[,1], t = 'l', ylim = c(min(fut), max(fut)))
par(new=T)
plot(fut[,2], t = 'l', ylim = c(min(fut), max(fut)))
par(new=T)
plot(fut[,3], t = 'l', ylim = c(min(fut), max(fut)))
par(new=T)
plot(fut[,4], t = 'l', ylim = c(min(fut), max(fut)))

# lineを足す
plot(fut[,1], t = 'l', ylim = c(min(fut), max(fut)))
lines(fut[,2])
lines(fut[,3])
lines(fut[,4])


# 相関


last <- index(last(GOOG[,'GOOG.Adjusted']))
last
xts(1, index(last(GOOG[,'GOOG.Adjusted']))+1:days)

start = '2015-01-02'
end = '2015-12-31'
GOOG = getSymbols('GOOG', src = 'yahoo', from = start, to = end, auto.assign=F)
AMZN = getSymbols('AMZN', src = 'yahoo', from = start, to = end, auto.assign=F)
MSFT = getSymbols('MSFT', src = 'yahoo', from = start, to = end, auto.assign=F)
AAPL = getSymbols('AAPL', src = 'yahoo', from = start, to = end, auto.assign=F)
head(GOOG)

GOOG.PREV_DAY_RATE = xts(GOOG[,'GOOG.Adjusted'] / lag.xts(GOOG[,'GOOG.Adjusted'], 1))
AMZN.PREV_DAY_RATE = xts(AMZN[,'AMZN.Adjusted'] / lag.xts(AMZN[,'AMZN.Adjusted'], 1))
MSFT.PREV_DAY_RATE = xts(MSFT[,'MSFT.Adjusted'] / lag.xts(MSFT[,'MSFT.Adjusted'], 1))
AAPL.PREV_DAY_RATE = xts(AAPL[,'AAPL.Adjusted'] / lag.xts(AAPL[,'AAPL.Adjusted'], 1))

hist(GOOG.PREV_DAY_RATE, breaks = 100)
hist(AMZN.PREV_DAY_RATE, breaks = 100)
ALL = merge(AAPL.PREV_DAY_RATE, AMZN.PREV_DAY_RATE,  GOOG.PREV_DAY_RATE,  MSFT.PREV_DAY_RATE, all= TRUE)
head(ALL)
tail(ALL)
ALL = na.omit(ALL)

cor(ALL)
library(PerformanceAnalytics)
chart.Correlation(ALL)
warnings()






# 細かくする？

# 前日比の分散 = risk
# 前日比の比較　（match度はどうする？）
# golden crossの取得
# 前日比からシミュレーション
# 1年後の価格予測
# 10000回やった場合の１年後の価格予測のplot