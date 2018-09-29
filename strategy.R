require(PerformanceAnalytics)
require(xts)
require(TTR)

# path <- getwd()
# setwd(path)

tt <- "data_20180916_20180923"
load(tt)

asset = 2 
bar_length = 30 # each bar is 30 minutes long
options(scipen = 999)
options(digits = 7)

initial_cash = 100000
cash_balance_lower_limit = 10000
transaction = 0.0005
time_list = names(data_format2)

memory = list(counter = 0,
              data_list = list(),
              bar = NULL,
              bar_prev = NULL,
              long_stop_loss = -Inf,
              short_stop_loss = Inf,
              long_profit_target = Inf,
              short_profit_target = -Inf,
              ws_check_table = data.frame(bar_close = numeric(),
                                          is_white_soider = logical()),
              bc_check_table = data.frame(bar_close = numeric(),
                                          is_black_craw = logical())
)

strategy = function(time, # current minute
                    data, # data for current minute bar (in format 2)
                    initial_cash, # your initial cash, a constant
                    transaction, # transaction ratio, a constant
                    cash_balance, # your cash balance at current minute
                    crypto_balance, # your crpyto currency balance at current minute
                    total_balance, # your total balance at current minute
                    position_current, # your position for 4 crypto currencies at this minute
                    memory # a list, containing the information you saved so far
){
    
    # Load variables from memory
    counter = memory[["counter"]]
    data_list = memory[["data_list"]]
    bar = memory[["bar"]]
    bar_prev = memory[["bar_prev"]]
    ws_check_table = memory[["ws_check_table"]]
    bc_check_table = memory[["bc_check_table"]]
    long_stop_loss = memory[["long_stop_loss"]]
    short_stop_loss = memory[["short_stop_loss"]]
    long_profit_target = memory[["long_profit_target"]]
    short_profit_target = memory[["short_profit_target"]]
    
    # Update counter
    counter = counter + 1
    
    # Generate OHLC data for every 15 minutes
    if(counter%%bar_length == 0){
        # save minute data to data_list
        data_list[[bar_length]] = data
        
        bar = generate_bar(data_list)
        
        if(!is.null(bar_prev)){
            
            # check patterns for a single bar
            ws_check = white_soider(data = bar, data_prev = bar_prev, asset = asset)
            ws_check_table = rbind(ws_check_table, ws_check)
            bc_check = black_craw(data = bar, data_prev = bar_prev, asset = asset)
            bc_check_table = rbind(bc_check_table, bc_check)
            
            bar_num = nrow(ws_check_table)
            if(bar_num > 3){
                
                # long signal 
                # When there is a three white soider signal, long 1 BTC at next minute unless the current cash balance is less than 30,000
                if(sum(ws_check_table$is_white_soider[(bar_num-2):bar_num])==3){
                    if(cash_balance > my_cash_balance_lower_limit){
                        position_new[asset] = position_current[asset] + 1
                        long_stop_loss = ws_check_table$bar_close[(bar_num-2)]
                        long_profit_target = ws_check_table$bar_close[bar_num]*(1+0.05)
                    }
                }
                
                # short signal 
                # When there is a three black craw signal, short 1 BTC at next minute unless the current cash balance is less than 30,000
                if(sum(bc_check_table$is_black_craw[(bar_num-2):bar_num])==3){
                    if(cash_balance > my_cash_balance_lower_limit){
                        position_new[asset] = position_current[asset] - 1
                        short_stop_loss = bc_check_table$bar_close[(bar_num-2)]
                        short_profit_target = bc_check_table$bar_close[bar_num]*(1-0.05)
                    }
                }
            }
        }
        
        bar_prev = bar
        
    } else {
        # save minute data to data_list
        data_list[[counter%%bar_length]] = data
    }
    
    
    # close signal
    # When reach stop loss / target profit points, clear all long / short positions
    average_price = rowMeans(data[1:4])[asset]
    if(position_new[asset] > 0){
        if(average_price > long_profit_target | average_price < long_stop_loss)
            position_new[asset] = 0
    } else if(position_new[asset] < 0) {
        if(average_price > short_stop_loss | average_price < short_profit_target)
            position_new[asset] = 0
    } else {
        position_new[asset] = 0
    }
    
    
    # Update memory
    memory[["counter"]] = counter
    memory[["data_list"]] = data_list
    memory[["bar"]] = bar
    memory[["bar_prev"]] = bar_prev
    memory[["ws_check_table"]] = ws_check_table
    memory[["bc_check_table"]] = bc_check_table
    memory[["long_stop_loss"]] = long_stop_loss
    memory[["short_stop_loss"]] = short_stop_loss
    memory[["long_profit_target"]] = long_profit_target
    memory[["short_profit_target"]] = short_profit_target
    
    return(list(position=position_new, memory=memory))
}

starttrade <- function(minutedata, daydata, minutesinday=240, k1=0.5, k2=0.2, startmoney=1000000, borrowed_rate = 0.5){
    daydata$hmc = daydata$High - daydata$Close
    daydata$cml = daydata$Close - daydata$Low
    daydata$maxhmccml = (daydata$hmc + daydata$cml + abs(daydata$hmc - daydata$cml)) / 2
    daydata$trigger1 = daydata$maxhmccml * k1
    daydata$trigger2 = daydata$maxhmccml * k2
    print(daydata)
    
    timevetor = c()
    cashvetor = c()
    stockassetvetor = c()
    allvetor = cashvetor + stockassetvetor
    
    cash = startmoney
    hands = 0
    stockasset = 0
    borrowed_money = startmoney * borrowed_rate
    borrowed_hands = 0
    has_borrowed = FALSE
    
    for(i in 2:nrow(daydata)){
        trigger1 = as.numeric(daydata$trigger1[i-1])
        trigger2 = as.numeric(daydata$trigger2[i-1])
        
        for(k in ((i-1)*minutesinday+1):(i*minutesinday)){
            # access this day's minute data
            if(as.numeric(minutedata[k]$Open) > (as.numeric(daydata[i]$Open)+trigger1)){
                # buy
                print('buyyyyyyyyyyyyy!')
                thishands = cash %/% as.numeric(minutedata[k]$Open)
                cash = cash - thishands * as.numeric(minutedata[k]$Open)
                hands = thishands + hands - borrowed_hands
                stockasset = hands * as.numeric(minutedata[k]$Open)
                borrowed_hands = 0
                has_borrowed = FALSE
            } else if(as.numeric(minutedata[k]$Open) < (as.numeric(daydata[i]$Open)-trigger2)){
                # sell
                print('sellllllllllllll!')
                if(!has_borrowed){
                    borrowed_hands_this_time = borrowed_money %/% as.numeric(minutedata[k]$Open)
                    has_borrowed = TRUE
                } else{
                    borrowed_hands_this_time = 0
                }
                borrowed_hands = borrowed_hands + borrowed_hands_this_time
                cash = cash + (borrowed_hands_this_time + hands) * as.numeric(minutedata[k]$Open)
                hands = 0
                stockasset = 0
            } else{
                stockasset = hands * as.numeric(minutedata[k]$Open)
            }
            
            #print(borrowed_hands*as.numeric(minutedata[k]$Open))
            #print(borrowed_hands)
            #print(cash)
            #print(cash-borrowed_hands*as.numeric(minutedata[k]$Open))
            #print(as.numeric(minutedata[k]$Open))
            realcash = cash-borrowed_hands*as.numeric(minutedata[k]$Open)
            timevetor = c(timevetor, index(minutedata)[k])
            cashvetor = c(cashvetor, realcash)
            stockassetvetor = c(stockassetvetor, stockasset)
            allvetor = c(allvetor, realcash+stockasset)
            print(paste('i: ', i, '  k: ', k, '  realcash: ', realcash, '  stockasset: ', stockasset, '  ',index(minutedata)[k] ))
        }
    }
    
    return(data.frame(time=as.POSIXct(timevetor, origin='1970-01-01', tz='UTC'), realcash=cashvetor, stockasset=stockassetvetor, all=allvetor))
}