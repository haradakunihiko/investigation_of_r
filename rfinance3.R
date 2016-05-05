library(quantmod)
tckrs <- c("SPY", "QQQ", "GDX", "DBO", "VWO")
getSymbols(tckrs, from = "2007-01-01")

SPY.Close <- SPY[,4]
QQQ.Close <- QQQ[,4]
GDX.Close <- GDX[,4]
DBO.Close <- DBO[,4]
VWO.Close <- VWO[,4]

SPY1 <- as.numeric(SPY.Close[1])
QQQ1 <- as.numeric(QQQ.Close[1])
GDX1 <- as.numeric(GDX.Close[1])
DBO1 <- as.numeric(DBO.Close[1])
VWO1 <- as.numeric(VWO.Close[1])

SPY <- SPY.Close/SPY1
QQQ <- QQQ.Close/QQQ1
GDX <- GDX.Close/GDX1
DBO <- DBO.Close/DBO1
VWO <- VWO.Close/VWO1

basket <- cbind(SPY, QQQ, GDX, DBO, VWO)

head(basket)

zoo.basket <- as.zoo(basket)
# Set a color scheme:
tsRainbow <- rainbow(ncol(zoo.basket))
# Plot the overlayed series
plot(x = zoo.basket, ylab = "Cumulative Return", main = "Cumulative Returns",
     col = tsRainbow, screens = 1)
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topleft", legend = c("SPY", "QQQ", "GDX", "DBO", "VWO"), 
       lty = 1,col = tsRainbow)



