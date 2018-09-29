# your team's working folder, must contain a "strategy.R" file
path <- getwd()
setwd(path)

# load data_XXXXXXXX_XXXXXXXX.RData provided by TA
# Note: You can test your strategy on different periods. Try to make your strategy profitable stably.
tt <- "data_201809.RData"
load(tt)

##############################################################
## The following code is for backtesting. DO NOT CHANGE IT! ##
##############################################################

# source your strategy file
source(file = "strategy.R")

# import some packages for testing performance of your strategy
# Note: You should install the following packages in your computer firstly (by commands such as install.packages("PerformanceAnalytics")) otherwise you will get error in your R console
require(PerformanceAnalytics)
require(xts)
require(TTR)
options(scipen = 999)
options(digits = 7)

# some important constants
# Note:
# 1. The following constants will not be changed during the whole semester
# 2. DO NOT set your customer variables the same names as the following constants otherwise you may get unexpected error or results
initial_cash = 100000
cash_balance_lower_limit = 10000
transaction = 0.0005
time_list = names(data_format2)


# begin to test your strategy minute by minute
for (time in time_list) {
    
    # 1. initialization
    if (time == time_list[[1]]) {
        cash_balance = initial_cash
        total_balance = initial_cash
        revenue = 0
        crypto_balance = 0
        average_price_old = rowMeans(data_format2[[time]][1:4])
        position_old = rep(0, length(data_format1))
        position_new = rep(0, length(data_format1))
        details = data.frame()
        stop_signal = FALSE
    }
    
    
    # 2. calculate position & cash/crypto/total balance & transaction cost etc.
    position_change = position_new - position_old
    mask = abs(position_change) > .25*data_format2[[time]]$volume
    position_change[mask] = (.25*data_format2[[time]]$volume*sign(position_change))[mask]
    position_new = position_old + position_change
    average_price = rowMeans(data_format2[[time]][1:4])
    transaction_cost = sum(abs(position_change)*transaction*average_price)
    revenue = sum(position_old*(average_price - average_price_old)) - transaction_cost
    
    crypto_balance = sum(abs(position_new*average_price))
    total_balance = total_balance + revenue
    cash_balance = total_balance - crypto_balance
    
    
    details = rbind(details, c(position_new, cash_balance, crypto_balance, revenue, total_balance, transaction_cost))
    position_old = position_new
    average_price_old = average_price
    
    
    # 3. check special cases
    # if cash balance is less than lower limit, the program will stop all trading actions in the future
    if((cash_balance < cash_balance_lower_limit) & (stop_signal == FALSE)){
        stop_signal = TRUE
        print(paste("Current cash balance is lower than:", as.character(cash_balance_lower_limit)))
        print("Your strategy is forced to stop.")
    }
    
    # when stop_signal is TRUE, stop the strategy part and keep the current balance (i.e., all cash)
    if(stop_signal){
        if (substr(time, 12, 19) == "00:00:00") print(substr(time, 1, 10))
        next
    }
    
    # 4. update position for NEXT minute using students' strategy function
    output = strategy(time = time,
                      data = data_format2[[time]],
                      initial_cash = initial_cash,
                      transaction = transaction,
                      cash_balance = cash_balance,
                      crypto_balance = crypto_balance,
                      total_balance = total_balance,
                      position_current = position_new,
                      memory = memory)
    position_new = output[['position']]
    memory = output[['memory']]
    if (substr(time, 12, 19) == "00:00:00") print(substr(time, 1, 10))
}

colnames(details) = c(names(data_format1), "cash.balance", "crypto.balance", "revenue", "total.balance", "transaction.cost")
rownames(details) = time_list

# show minute-level details about your strategy, including:
# 1. positions of all 4 crypto currencies
# 2. cash/crypto/total balance
# 3. revenue (profit or loss) at each minute
# 4. transaction cost at each minute
View(details)

# calculate summary statistics (Sharpe ratio, total return, average daily return and maximum dropdown)
balance_xts = xts(x=details$total.balance, order.by=as.POSIXct(time_list))
roc_min = ROC(balance_xts, n = 1, type = "discrete")[2:length(time_list)]
roc_daily = period.apply(roc_min, endpoints(roc_min, "days"), function(x) prod(1+x)-1)
roc_hour = period.apply(roc_min, seq(0, length(time_list)-1, 60), function(x) prod(1+x)-1)
total_ret = prod(1+roc_min)-1
sharpe_ratio = mean(roc_daily, na.rm=T)/sd(roc_daily, na.rm=T)*sqrt(365)
daily_ret = mean(roc_daily)
max_drawdown = min(balance_xts/cummax(balance_xts)-1, na.rm=T)
print(paste("Total Return:", as.character(total_ret)))
print(paste("Average Daily Return:", as.character(daily_ret)))
print(paste("Sharpe Ratio:", as.character(sharpe_ratio)))
print(paste("Maximum Drawdown:", as.character(max_drawdown)))

# Draw performance chart (hour-level)
par(mfrow=c(2,1))
chart.CumReturns(roc_hour, main = "Cumulative Return")
chart.Drawdown(roc_hour, main = "Drawdown")