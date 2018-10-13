library(xts)
library(quantmod)
library(TTR)

load("data_20180916_20180923.RData")

time_list = names(data_format2)
pstart = 1
pend = length(data_format1[["LTC-USD"]]$close)
xt <- seq(pstart,pend,by=1)
x1 <- data_format1[["BCH-USD"]]$close[pstart:pend]
x2 <- data_format1[["ETH-USD"]]$close[pstart:pend]

m <- lm (x1 ~ x2)
sprd <- x1 - coef(m)[2] * x2 - coef(m)[1]
# spread_mean
print(coef(m)[1])
# spread_sigma
print(sd(sprd))
# hedge ratio
print(coef(m)[2])
