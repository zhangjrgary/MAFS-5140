# MAFS-5140 Project: Trading Strategies Implement in R

The purpose of this project is to cmparative market prices analysis in R, backtest strategies and place trades.

This repo is to be used only for learning purposes of how to handle data and trading strategies, not suitable for trading.

## Inspiration
Currently in ETHUSD pairs. 
The "buy" signal initiates a limit order at te current bid, and iterates until the order is filled. 
The buy signal is a combination of EMA and RSI indicators which signal that ETH is oversold. 
The sell signal is the opposite with overbought being the prime mover. 
This script is scheduled to run every 5-10 minutes using a tool such as Windows Task Scheduler.
## Structure
`utility` functions
`data` prices data of four kind of cryptocurrency
`script` main interfaces
`scratch` draft codes

## Workflow
### Initial

### On Going
* *Load* datas and check data is sorted and unique
* 
