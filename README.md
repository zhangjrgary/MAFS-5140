# MAFS-5140 Project: Crypto Arbitrage in R

The purpose of this project is to cmparative market prices analysis in R, backtest strategies and place trades.

Predicts next-day closing price and executes trade at next-day price.

This repo is to be used only for learning purposes of how to handle data and trading strategies, not suitable for trading.

## Inspiration

10 min sliding window (memory).

Currently in pairs. 
The "buy" signal initiates a limit order at te current bid, and iterates until the order is filled. 
The buy signal is a combination of EMA and RSI indicators which signal that ETH is oversold. 
The sell signal is the opposite with overbought being the prime mover. 

## Structure
`utility` functions

`data` prices data of four kind of cryptocurrency

`script` main interfaces

`scratch` draft codes

## Workflow

### Initial

### On Going
* *Load* datas and check data is sorted and unique

* http://blog.fens.me/finance-chase-sell/

## Contribution
https://numex-blog.com/crypto-portfolio-optimization-part-i-correlation-matrix/
https://www.analyticsvidhya.com/blog/2017/09/comparative-stock-analysis/
