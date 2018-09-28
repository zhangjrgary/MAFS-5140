library(xts)
setwd('/Users/gary/Desktop/5140/MAFS')
data <- read.table("bch", sep="\t", dec=",", row.names=1)
head(as.xts(data))

#plot(sample_matrix[, 4], type = "l", main = "Stock prices")

