library(quantmod)
tckrs <- c("SPY", "QQQ", "GDX", "DBO", "VWO")
getSymbols(tckrs, from = "2007-01-01")
head(SPY)

SPY.Adjusted <- SPY[,6]
QQQ.Adjusted <- QQQ[,6]
GDX.Adjusted <- GDX[,6]
DBO.Adjusted <- DBO[,6]
VWO.Adjusted <- VWO[,6]


basket <- cbind(SPY.Adjusted, QQQ.Adjusted, GDX.Adjusted, DBO.Adjusted, VWO.Adjusted)
head(basket)

# The xts merge(.) function will only accept two series at a time.
# We can, however, merge multiple columns by downcasting to *zoo* objects.
# Remark:  "all = FALSE" uses an inner join to merge the data.
mktPrices <- merge(SPY.Adjusted, QQQ.Adjusted, GDX.Adjusted, DBO.Adjusted, VWO.Adjusted, all = FALSE)
head(mktPrices)
is.xts(mktPrices)
mktRtns <- diff(log(mktPrices), lag = 1)
head(mktRtns)

mktRtns <- mktRtns[-1, ]  # Remove resulting NA in the 1st row
require(gplots)

generate_heat_map <- function(correlationMatrix, title)
{
  heatmap.2(x = correlationMatrix,		# the correlation matrix input
            cellnote = correlationMatrix,	# places correlation value in each cell
            main = title,			# heat map title
            symm = TRUE,			# configure diagram as standard correlation matrix
            dendrogram="none",		# do not draw a row dendrogram
            Rowv = FALSE,			# keep ordering consistent
            trace="none",			# turns off trace lines inside the heat map
            density.info="none",		# turns off density plot inside color legend
            notecol="black")		# set font color of cell labels to black
}

corr1 <- cor(mktRtns) * 100
corr2 <- cor(mktRtns['2004-01/2004-12']) * 100
corr3 <- cor(mktRtns['2008-10/2009-05']) * 100

generate_heat_map(corr1, "Correlations of World Market Returns, Jan 1998 - Present")
generate_heat_map(corr2, "Correlations of World Market Returns, Jan - Dec 2004")
generate_heat_map(corr3, "Correlations of World Market Returns, Oct 2008 - May 2009")
