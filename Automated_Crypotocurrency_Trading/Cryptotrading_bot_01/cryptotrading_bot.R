#-------------------------------------------------------------------------------------------------------------------------
# Script for the automated trading of cryptocurrencies on the Binance Spot Test Network
#-------------------------------------------------------------------------------------------------------------------------

# Example taken (and adapted to make it running) from:
# https://medium.com/mcd-unison/cryptotrading-bot-with-r-no-real-transactions-beginner-friendly-9d9407dc1d40

# Libraries
# install.packages("devtools", dependecies=TRUE)
# install_github("daroczig/binancer")
library(magrittr)
library(plotly)
library(quantmod)
library(dplyr)
library(PerformanceAnalytics)
library(binancer)
library(emayili)

# Set working directory
setwd("U:/Lektionen/Summerschool_FS2021/Automated_Crypotocurrency_Trading/Cryptotrading_bot_01")
getwd()

# Clean working directory
rm(list = ls())

# Begin cycle
repeat {
  
  # Source functions 
  source("emailing.R")
  source("indication.R")
  
  # API keys
  key    <- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
  secret <- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
  
  # Calling the API
  binance_ping()
  binance_credentials(key, secret)
  
  # Email fields (it is recommended to use a gmail-test account)
  from     <- 'user@gmail.com'
  to       <- 'user@gmail.com'
  password <- 'password'
  
  # Generate new data every minute
  # Note that BTCUSDT is giving the indication to use bitcoin and in USD, this key can change as needed.
  
  data <- binance_klines('BTCUSDT', interval = '1m')
  data[1:10,]

  # MACD
  macd  <- MACD(data[,"close"], 12, 26, 9, maType="EMA")
  macd1 <- as.data.frame(macd)
  macd1
  
  # RSI
  rsi  <- RSI(data$close, n = 14, maType = "SMA") 
  rsi1 <- as.data.frame(rsi)
  
  # Create a new line for the log file 
  df     <- indication(macd1, rsi1)
  df[1:10,]
  status <- df$decision[500]
  status
  
if(status == "Buy"){
         
       balance <- df$price[500]
       GanPer  <- 0
       results <- cbind(df[500,], balance, GanPer)
      
       # Save to CSV
       write.table(results, file="results.csv", append = T, sep = ',', row.names=F, col.names=F)
       
       # Send email
       # emailing(from, to, 'Transaction notice' , 'You bought bitcoin!', password)
       
       while(status == "Buy" | status == "Wait"){
        
         data   <- binance_klines('BTCUSDT', interval = '1m')
         macd   <- MACD(data[,"close"], 12, 26, 9, maType="EMA")
         macd1  <- as.data.frame(macd)
         rsi    <- RSI(data$close, n = 14, maType = "SMA") 
         rsi1   <- as.data.frame(rsi)
         df     <- indication(macd1, rsi1)
         status <- df$decision[500]
         
       }
       
       if (status == "Sell"){
         
         balance  <- df$price[500] - balance
         GanPer   <- balance
         results  <- cbind(df[500,], balance, GanPer)
     
         # Save to CSV
         write.table(results, file="results.csv", append = T, sep = ',', row.names=F, col.names=F)
         
         # Send email
         # emailing(from, to, 'Transaction notice' , 'You sold bitcoin!', password)
         
         while(status == "Sell"){
           
           data   <- binance_klines('BTCUSDT', interval = '1m')
           macd   <- MACD(data[,"close"], 12, 26, 9, maType="EMA")
           macd1  <- as.data.frame(macd)
           rsi    <- RSI(data$close, n = 14, maType = "SMA") 
           rsi1   <- as.data.frame(rsi)
           df     <- indication(macd1, rsi1)
           status <- df$decision[500]
                                 
         }
       } 
       
} else {
   
   write.table(df[500,], file="df.csv", append = T, sep = ',', row.names=F, col.names=F)
  
}
  
 Sys.sleep(time=60)

}

# Profit calculation
results   <- read.csv('results.csv', header = F)
total     <- sum(results$V7) + results[1,6]
concept   <- c('Init_Invest', 'Total', 'Profit')
values    <- c(results[1,6], total, total - results[1,6])
values    <- data.frame(concept, values)
values
