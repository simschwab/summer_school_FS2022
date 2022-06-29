#-------------------------------------------------------------------------------
# Script for cryptocurrency data collection and analysis
#-------------------------------------------------------------------------------

# Links:
# https://stackoverflow.com/questions/65659346/how-to-place-a-binance-order-through-r
# https://www.r-bloggers.com/2017/03/native-support-for-candlestick-charts-in-plotly-and-r/
# https://plotly.com/r/candlestick-charts/

#-------------------------------------------------------------------------------
# Install/load required packages
#-------------------------------------------------------------------------------

library(devtools)
# Installing the packages 'binancer' from GitHub 
# devtools::install_github("https://github.com/daroczig/binancer")

# Warning setting (=off)
options(warn=-1)

#-------------------------------------------------------------------------------
# Getting data from the 'Binance' cryptocurrency exchange
#-------------------------------------------------------------------------------

library(binancer)

# Get valid symbol names (names of coin pairs)
binance_symbols(all = FALSE)

# Settings
coin.pair      = 'BTCUSDT'
time.interval  = '1d'
start.time     = '2020-07-08 00:00:00'

# Get data
df.orig  <- binance_klines(coin.pair, interval=time.interval, start_time=start.time, end_time=Sys.Date())
df.orig

# Rename the data
df        <- df.orig[,c('open_time', 'open', 'high', 'low', 'close', 'volume')]
names(df) <- c('OpenTime', 'Open', 'High', 'Low', 'Close', 'Volume')
df

dim(df)

#-------------------------------------------------------------------------------
# Exploratory data analysis
#-------------------------------------------------------------------------------

# Summary statistics of variables
summary(df)

# Histogram of close price
hist(df$Close, nclass=20)

# Histogram of volume
hist(df$Volume, nclass=20)

# Scatterplot highest versus lowest price
plot(df$High, df$Low, pch=21, bg="black")

# Scatterplot highest versus lowest price
plot(df$High, df$Low, pch=21, bg="black")

# Scatterplot open versus close price
plot(df$Open, df$Close, pch=21, bg="black")

# Scatterplot volume versus close price
plot(df$Volume, df$Close, pch=21, bg="black")

#-------------------------------------------------------------------------------
# Calculating indicators for the technical analysis (TA)
#-------------------------------------------------------------------------------

library(TTR)

#---------------------------------------------
# Simple moving average (SMA)
#---------------------------------------------

sma.20    <- SMA(df$Close, n=20)
df$SMA20  <- sma.20 
df

#---------------------------------------------
# Relative Strength Index (RSI)
#---------------------------------------------

rsi    <- RSI(df$Close, n=14)
df$RSI <- rsi
df

#---------------------------------------------
# Bollinger Bands (BBands)
#---------------------------------------------

# Create Bollinger Bands
bbands <- BBands(df[,c("High","Low","Close")], n=20, maType="SMA")
bbands

# Join and subset data
df$DN   <- data.frame(bbands[,1])
df$MAVG <- data.frame(bbands[,2])
df$UP   <- data.frame(bbands[,3])
df

#---------------------------------------------
# Moving Average Convergence/Divergence (MACD)
#---------------------------------------------

# See also: https://www.r-bloggers.com/2015/10/an-example-of-a-trading-strategy-coded-in-r

# Calculate MACD and Signal
macd <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType="EMA", percent = TRUE)
macd

# Join the calulated values to the data frame
df$MACD   <- data.frame(macd[,1])
df$Signal <- data.frame(macd[,2])
df

#-------------------------------------------------------------------------------
# Plot simple interactive candlestick chart using the plotly package
#-------------------------------------------------------------------------------

library(plotly)
library(quantmod)

# Candlestick chart
fig <- df %>% plot_ly(x     = ~ OpenTime, 
                      type  = 'candlestick',
                      open  = ~ Open, 
                      close = ~ Close,
                      high  = ~ High, 
                      low   = ~ Low,
                      # Fill Color of candles
                      increasing = list(line = list(color = '#17BECF')), 
                      decreasing = list(line = list(color = '#7F7F7F'))
               )

fig <- fig %>% layout(title = "Basic Candlestick Chart")
fig

#-------------------------------------------------------------------------------
# Add Volume, Bollinger Bands (BBands) and Moving Average (MA) to the plot
#-------------------------------------------------------------------------------

# Colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$Close[i] >= df$Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}
i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))
df

# Plot candlestick chart
fig <- df %>% plot_ly(x = ~OpenTime, type="candlestick",
                      open = ~Open, close = ~Close,
                      high = ~High, low = ~Low, name = coin.pair,
                      increasing = i, decreasing = d) 

fig <- fig %>% add_lines(x = ~OpenTime, y = ~UP , name = "BBands",
                         line = list(color = '#ccc', width = 0.8),
                         legendgroup = "Bollinger Bands",
                         hoverinfo = "none")

fig <- fig %>% add_lines(x = ~OpenTime, y = ~DN, name = "BBands",
                         line = list(color = '#ccc', width = 0.8),
                         legendgroup = "Bollinger Bands",
                         showlegend = FALSE, hoverinfo = "none") 

fig <- fig %>% add_lines(x = ~OpenTime, y = ~MAVG, name = "MA",
                         line = list(color = '#E377C2', width = 0.8),
                         hoverinfo = "none")

fig <- fig %>% layout(yaxis = list(title = "Price"))

# Plot volume bar chart
fig2 <- df 
fig2 <- fig2 %>% plot_ly(x=~OpenTime, y=~Volume, type='bar', name = "Volume",
                         color = ~direction, colors = c('#17BECF','#7F7F7F')) 
fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))

# Create range-selector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=1,
                  label='1 YR',
                  step='year',
                  stepmode='backward'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward')
           ))

# Subplot with shared x axis
fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
               shareX = TRUE, titleY = TRUE)

fig <- fig %>% layout(title = paste("BTCUSDT: 2020-07-08 to", Sys.Date()),
                      xaxis = list(rangeselector = rs),
                      legend = list(orientation = 'h', x = 0.5, y = 1,
                                    xanchor = 'center', yref = 'paper',
                                    font = list(size = 10),
                                    bgcolor = 'transparent'))

fig

#-------------------------------------------------------------------------------
# Backtesting a simple intraday strategy
#-------------------------------------------------------------------------------

# See also this video: https://www.youtube.com/watch?v=f2I1RJnyLRg

library(tidyverse)
library(TTR)
library(PerformanceAnalytics)

# Rearranging the data frame and calculating profit & loss (=pnl)
df_bt <- df %>%
         arrange(OpenTime) %>%
         mutate(pnl=replace_na(Close/lag(Close) -1, 0))
df_bt

# Defining the strategy
strategy <- df_bt %>%
            arrange(OpenTime) %>%
            mutate(date = as.Date(OpenTime),
                   fast_ma = SMA(Close, 5),
                   slow_ma = SMA(Close, 20),
                   pos = if_else(fast_ma > slow_ma, 1, -1) %>% lag() %>% replace_na(0),
                   str_pnl = pos*pnl
            )
strategy

# Dataframe with date and pnl only
daily <- strategy %>%
         select(date, pnl=str_pnl)
daily

# Convert daily to xts object (xts has an ordered index)
daily_xts <- xts(daily$pnl, order.by=daily$date)
daily_xts

#------------------------------
# Backtesting result
#------------------------------

# Infos:
# https://en.wikipedia.org/wiki/Sharpe_ratio

# Histogram of daily_xts
hist(daily_xts[,1]*100, nclass=20, main='Distribution of returns', col="#17BECF", xlab='Returns (%)')

# Quantiles of returns
as.matrix(quantile(daily_xts[,1]*100, probs=seq(0,1, 0.1)))

# Result of the strategy in %
round((prod(daily$pnl + 1) -1 ) * 100, 1)

# Maximum drawdown from peak equity in %
round(maxDrawdown(daily_xts) * 100, 1)

# Annualized returns
tab <- table.AnnualizedReturns(daily_xts)

# Performance chart
charts.PerformanceSummary(daily_xts, col="red3")
