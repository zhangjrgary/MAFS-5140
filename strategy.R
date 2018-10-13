# Team name: vibe
# Team members: 
# Ding Tianhao	20566490
# Huang Bo	20567793
# Yang Yu	20551732
# Zhang Jiarui	20568096

# Strategy:
# Mean Reversion Pair Trading

# Time: 2018/10/14

# Noticing the prices of crypto currencies often move in similar manners, 
# we assume that the spread between different currencies' price should always follow "mean reversion".

# Denote the prices of two kinds of crypto currencies by pA and pB. 
# A linear model is built to describe the assumed linear relationship between pA and pB:
# pA = beta2 * pB + beta1 + residual.
# The linear regression model is in the file "LinearModelParameter.R"
# Then, we have portfolio A with value pA and portfolio B with value beta2 * pB, where beta2 is the hedge ratio.
# The difference between their values is the spread.
# Based on linear regression, the spread has the mean of beta1 and standard deviation std(residual) = sigma
# Thus, the spread is believed to move around the mean with a large proabability.

# The mean reversion property leads to our pair trading strategy:
# If spread > sigma at time t, the value of portfolio A (pA) rises, while that of portfolio B (beta * pB) drops.
# After the mean reversion in the future, the value of portfolio A should decrease while that of portfolio B should increase.
# Thus, at time t, we should short portfolio A and long portfolio B.
# Conversely, if spread < -sigma, we should long portfolio A and short portfolio B.
# When the spread goes back to its mean, we close position.
# Similarly, when spread crosses other thresholds like +/- 2/3/4 sigma, we can increase postion.
# In doing this, we could obtain more gains when spread gets back to mean.

# However, the strategy has some risks to lose money.
# 1. When we open a position, the spread may not narrow but continue expanding instead.
#    In this case, if we close position, we will lose money.
#    This may occur when the mean of spread has shifted from our predicted value based on the previous data.
# 2. If the hedge ratio we set cannot offset the risk exposure completely, 
#    we may lose money when crypto currencies' price change.

#### Working area 1 ####
#### Working area 2 ####
#### Working area 3 ####

memory = list(counter = 0, 
              save_average_price = data.frame(BCH = 0, BTC = 0, 
                                              ETH = 0, LTC=0), # Record the average prices
              position_state = 0, # Initial position is empty.
              spread_mean = 24.33403, # Spread mean calculated from linear regression model
              spread_sigma = 0.5828585, # Spread standard deviation calculated from linear regression model
              hedge_ratio = 0.1495153, # Linear regression coefficients based on previous data
              save_spread_zscore = data.frame(spread = 0) # Record zscored spread used for analysis
              )


#### Working area 4 ####

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
  save_average_price = memory[["save_average_price"]]
  position_state = memory[["position_state"]] # the state of price spread of paired assests
  spread_sigma = memory[["spread_sigma"]] # the standard deviation of spread within a specific previous period
  spread_mean = memory[["spread_mean"]] # the mean of spread within a specific previous period
  hedge_ratio = memory[["hedge_ratio"]] # the coefficients for linear fitting
  save_spread_zscore = memory[["save_spread_zscore"]]
  
  # Update counter
  counter = counter + 1
  
  # record the average price calculated by "test.py"
  save_average_price[counter,"BCH"] = average_price["BCH-USD"]
  save_average_price[counter,"BTC"] = average_price["BTC-USD"]
  save_average_price[counter,"ETH"] = average_price["ETH-USD"]
  save_average_price[counter,"LTC"] = average_price["LTC-USD"]

  # share for unit position
  position_share = 150 
  
  # Calculate the spread based on the linear model
  # Here we choose LTC and ETH as the trading pair
  spread = average_price["LTC-USD"] - hedge_ratio * average_price["ETH-USD"]
  # Zscore the spread
  spread_zscore <- (spread - spread_mean)/spread_sigma
  #print(spread_zscore)
  
  # Recored zscored spread used for further analysis
  save_spread_zscore[counter,'spread'] <- spread_zscore
  
  # Close position if cash balance is less than 20000
  if (cash_balance < 20000)
    position_new = rep(0, 4)
  else{
    # Empty position
    if (position_state == 0){
      if (all(position_current == rep(0, 4))){
        for (j in c(1 : 4)){
          #if z-scored spread is greater than j, open a position 1: short x1 and long x2 (for j share)
          if (spread_zscore > j){ 
            position_new = + j* position_share * c(0, 0, hedge_ratio, -1)
            position_state = +1
          }
          #if z-scored spread is less than -j, open a position -1: long x1 and short x2 (for j share)
          else if (spread_zscore < -j){ 
            position_new = - j* position_share * c(0, 0, hedge_ratio, -1)
            position_state = -1
          }
        }
      }
    }
    
    # Close position +1 if z-scored spread is less than 0; otherwise, change position based on signal
    if (position_state == 1) {
      for (j in c(2 : 4)){
        if (spread_zscore > j){
          position_new = + j* position_share * c(0, 0, hedge_ratio, -1)
          # print(c(1, spread_zscore))
        }
        else if (spread_zscore < 0){
          position_new = rep(0, 4) 
          position_state = 0
        }
      }
    }
    # Close position -1 if z-scored spread is greater than 0; otherwise, change position based on signal
    if (position_state == -1) {
      for (j in c(2 : 4)){
        if (spread_zscore < -j){
          position_new = - j* position_share * c(0, 0, hedge_ratio, -1)
          # print(c(1, spread_zscore))
        }
        else if (spread_zscore > 0){
          position_new = rep(0, 4) 
          position_state = 0
        }
      }
    }
    
    # Keep closing position upon signal for closing position
    if (position_state == 0 && spread_zscore > -.2 && spread_zscore < .2)
      position_new = rep(0, 4)
  }
  # Update memory
  memory[["counter"]] = counter
  memory[["save_average_price"]] = save_average_price
  memory[["position_state"]] = position_state
  memory[["spread_sigma"]] = spread_sigma
  memory[["spread_mean"]] = spread_mean
  memory[["hedge_ratio"]] = hedge_ratio
  memory[["save_spread_zscore"]] = save_spread_zscore
  
  return(list(position=position_new, memory=memory))
}
