# http://qiita.com/HirofumiYashima/items/c5eb01b6536f8b62137f

address <- "http://indexes.nikkei.co.jp/nkave/historical/nikkei_stock_average_daily_jp.csv"
nikkei225 <- read.csv(file=url(address), skip = 1, header=FALSE, sep=",", fileEncoding='Shift_JIS')
head(nikkei225)
tail(nikkei225)

nikkei225 <- nikkei225[-815, ]
tail(nikkei225)

colnames(nikkei225) <- c("date","end", "start", "high", "low")
head(nikkei225)

# 簡単にアクセスできるようにする？
attach(nikkei225)
prices <- cbind(end, start, high, low)

tail(prices)

par(mfrow=c(2,2), new=F)

plot(prices[, 1], type="l", xlab="データ開始日 2011/01/04 からの\n営業日経過日数", ylab="日経平均", col="blue1", main="終値")
plot(prices[, 2], type="l", xlab="データ開始日 2011/01/04 からの\n営業日経過日数", ylab="日経平均", col="deeppink1", main="始値")
plot(prices[, 3], type="l", xlab="データ開始日 2011/01/04 からの\n営業日経過日数", ylab="日経平均", col="red1", main="高値")
plot(prices[, 4], type="l", xlab="データ開始日 2011/01/04 からの\n営業日経過日数", ylab="日経平均", col="green3", main="安値")


close.price <- prices[, 1]

head(close.price)
close.price <- ts(close.price, start=c(2011,01), frequency=4)
head(close.price)
class(close.price)
change.1 <-((close.price - lag(close.price,-1))/lag(close.price,-1))*100
head(change.1)
