#-------------------------------------------------------------------------------------------------------------------------
# Script for the automated trading of cryptocurrencies on the Binance test network
#-------------------------------------------------------------------------------------------------------------------------

# Libraries
# library(devtools)
# install_github("daroczig/binancer")
library(magrittr)
library(plotly)
library(quantmod)
library(dplyr)
library(DescTools)
library(binancer)

# Set warn messages to 'off'
options(warn=-1)

# API keys
key    <- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
secret <- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

binance_ping()
binance_credentials(key, secret)

data <- binance_klines('BTCUSDT', interval = '1d')

precioBTC <- binance_coins_prices(unit = "USDT")  %>%
  filter(symbol == "BTC")
precioBTC$usd

head(data)
str(data)
x <- data$open_time
y <- data %>% dplyr::select(open, high, low, close) %>% as.matrix()

# Candlestick
data %>% 
  plot_ly(x = data$open_time, type="candlestick",
                 open = data$open, close = data$close,
                 high = data$high, low = data$low) %>%
  layout(title = "Candlestick Chart")

# MACD
macd  <- MACD(data[,"close"], 12, 26, 9, maType="EMA" )
macd1 <- as.data.frame(macd)
head(macd1)
tail(macd1)

# RSI 
rsi  <- RSI(data$close, n = 14, maType = "SMA")
rsi1 <- as.data.frame(rsi)
head(rsi1)
tail(rsi1)

# BBands
bbands  <- BBands( data[,c("high","low","close")] )
bbands1 <- as.data.frame(bbands)
head(bbands1)
tail(bbands1)

# Candlestick with MACD, RSI and BBands
chartSeries(data, theme=chartTheme('black'))
addMACD(fast=12,slow=26,signal=9,type="EMA")
addRSI(data$close, n = 14, maType = "SMA")
addBBands(n = 20, sd = 2, maType = "SMA", draw = 'bands')
