# plot, 相関係数、matrix、vector
#2-1
bhigh <- c(116,128,120,116,118)
blow <- c(71,70, 72,68,69)
c(median(bhigh), mean(bhigh), sd(bhigh))
c(median(blow), mean(blow), sd(blow))


c(mean(bhigh) - 2 * sd(bhigh),mean(bhigh) + 2 * sd(bhigh))
c(mean(blow) - 2 * sd(blow),mean(blow) + 2 * sd(blow))

#2-2
GDP <- c(143.694,48.997,36.559,28.565,26.526,23.031,15.945,14.996,10.852,10.337,9.291,8.729,7.300,5.283,5.049,5.003,4.790,4.518,4.129,3.503,3.408,2.706,2.663,2.436,2.161,1.542,1.278,950,576,168)
GDPp <- c(47.186,38.371,44.519,44.550,43.237,38.455,34.971,44.950,10.183,48.049,19.115,53.094,10.270,13.861,47.151,64.885,51.954,94.763,49.527,31.174,62.054,50.931,59.944,22.929,20.719,15.363,29.693,17.566,117.967,52.568)
country <- c("アメリカ合衆国","日本","ドイツ","フランス","イギリス","イタリア","スペイン","カナダ","メキシコ","オーストラリア","韓国","オランダ","トルコ","ポーランド","ベルギー","スイス","スウェーデン","ノルウェー","オーストリア","ギリシャ","デンマーク","フィンランド","アイルランド","ポルトガル","チェコ","ハンガリー","ニュージーランド","スロバキア","ルクセンブルグ","アイスランド")
GDPmat <- cbind(GDP,GDPp)
rownames(GDPmat) <- country

sort(GDPmat[,'GDP'])
rev(order(GDPmat[,'GDP']))
GDPmat[rev(order(GDPmat[,'GDPp'])),]

hist(GDP)
hist(GDPp)
summary(GDP)
summary(GDPp)

#2-4
airport <- read.csv('/Users/haradakunihiko/Documents/airport.csv', header = FALSE)

airport[rev(order(airport[,'V2'])),]

hist(log10( airport[,'V2']), )

#2-5
temprature <- c(29.5,31.0,28.8,31.6,30.3,30.6,27.8,30.1,27.8,31.1,28.9,29.1,27.2,31.3,31.4,31.9,32.1,31.7,34.5,34.5,36.3,36.1,35.7,35.8,34.4,33.3,33.8,34.2,27.9,29.2,32.2)
power <- c(4900,4916,4049,4066,4984,5022,4716,4916,4640,4146,3725,4689,4367,4679,4899,5249,4492,4228,4793,5726,5918,5965,5999,5213,4715,5550,5666,5596,4659,4953,4712)

plot(temprature, power)
cor(temprature,power)


#2-6 累積和 => 不動産価格
men <- c(17.6,17.3,14.4,17.1,10.6,10.5,12.5)
women <-c(11.8,16.8,16.3,15.2,11.3,9.0,19.6)
cumsum(men)
barplot(cumsum(men))
barplot(cumsum(women))


#2-8 GNIと保有台数・相関係数
gni <- c("日本",39574,591,"ブラジル",8332,198,"インド",1080,15,"イギリス",44187,526,"韓国",19487,346,"イタリア",37936,673,"タイ",3884,134,"ギリシャ",29983,560,"中国",3316,37,"スペイン",34830,606,"トルコ",9871,138,"ドイツ",44887,554,"マレーシア",7921,334,"フランス",45085,598,"アメリカ合衆国",46236,809,"ポーランド",13614,495,"カナダ",44549,605,"ロシア",11451,245,"メキシコ",9855,264,"オーストラリア",45402,687)
gniMatrix <- matrix(gni, ncol=3, byrow = TRUE)

cols <- gniMatrix[,1]
gniMatrix <- cbind(as.integer(gniMatrix[,2]),as.integer(gniMatrix[,3]))

rownames(gniMatrix) <- cols

cor(gniMatrix[,1],gniMatrix[,2])


#3 
coin <- c("おもて", "うら")
res <- sample(coin , times, replace =TRUE)
length(which(res == 'おもて'))/times

times <- 100

expriment <- sapply(rep(100,100), function (n) {
  res <- sample(coin , times, replace =TRUE)
  return (length(which(res == 'おもて'))/times)
})
hist(expriment)
hist(expriment, freq=F, add=TRUE)
curve(dnorm(x,0.5,0.05))


#3-2 投票のシミュレーション
population <- c(rep("支持",20000), rep("不支持",80000))

length(which(sample(population, 100) == '支持'))/100

simulation <- sapply(rep(1,10000), function (x) {
  sm <- sample(population, 1000)
  return (length(which(sm == '支持'))/length(sm))
})
hist(simulation)


# 3-3 実験とシミュレーション・視聴率 dnorm
total <- 1000000
audi <- 100000
sample(c(rep(1,audi), rep(0, total-audi)))

check <- function(x) {
  mean(sample(c(rep(1,audi), rep(0, total-audi)), 600))
}

res <- sapply(rep(1,1000), check)
hist(res, breaks = 100, freq = FALSE)
curve(dnorm(x, mean(res), sd(res)), add=T, col=2)
dnorm(x, mean(res), sd(res)

#3-4 正規分布からのランダム抽出
res <- rnorm(100, 10, 2)
c(mean(res), sd(res))
hist(res, freq=FALSE)
curve(dnorm(x, mean(res), sd(res)),add=TRUE, col=2 )

#4-7 一様な乱数の平均値の分布
m <- 12
n <- 10000
res <- sapply(rep(1,n), function (x) mean(runif(m)))
hist(res)

# 5回帰分析までやる？
