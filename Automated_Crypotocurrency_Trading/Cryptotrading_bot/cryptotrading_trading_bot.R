#------------------------------------------------------------------------------------------------------
# Script for automatic cryptocurrency trading on Coinbase
#
# It uses the Coinbase-Sandbox: https://public.sandbox.pro.coinbase.com
#
# Example taken and (with some effort and bug-fixing) made running from: 
# https://towardsdatascience.com/build-a-cryptocurrency-trading-bot-with-r-1445c429e1b1

# Further information about rgdax (Coinbase):
# https://rdrr.io/github/DheerajAgarwal/rgdax/src/R/auth.R
# https://randerson112358.medium.com/create-a-cryptocurrency-trading-bot-in-r-54a445136408

#-------------------------------------------------------------------------------
# Prerequisites
#-------------------------------------------------------------------------------

# Coinbase Pro account with verification (pass photo upload)
# Coinbase Pro public sandbox open: https://public.sandbox.pro.coinbase.com
# Deposit EUR
# API-Key
# Secret-Key
# Passphrase

#-------------------------------------------------------------------------------
# Path to working directory
#-------------------------------------------------------------------------------

setwd('U:/Lektionen/Summerschool_FS2022/Automated_Crypotocurrency_Trading/Cryptotrading_Bot')
getwd()

#-------------------------------------------------------------------------------
# Loop (will be replaced later by scheduling using cron)
#-------------------------------------------------------------------------------

# Loop start
for(i in 1:1000) {
  
#-------------------------------------------------------------------------------
# Define RSI thresholds for the bot to start trading (according to the Strategy)
#-------------------------------------------------------------------------------

# RSI - thresholds
RSI14_th     <- 30 # >=
RSI14_L01_th <- 30 # <=
RSI14_L02_th <- 30 # <
RSI14_L03_th <- 30 # <
RSI14_L04_th <- 30 # <

#-------------------------------------------------------------------------------
# Load libraries and define working directory
#-------------------------------------------------------------------------------

# Install package rgdax from lokal folder (bug-fixed GitHub Repository)
# devtools::install("C:/Users/gell/rgdax", force=TRUE)

library(rgdax)
library(stringi)
library(curl)
library(xts)
library(TTR)
library(dplyr)
library(jsonlite)

#-------------------------------------
# Authentication
#-------------------------------------

# Import API-Keys from separate file
source("api_keys.R")

#-------------------------------------
# Overview of Coinbase accounts
#-------------------------------------

accounts(api.key = api_key, 
         secret = secret, 
         passphrase = passphrase
)

#-------------------------------------
# Get valid product IDs on Coinbase
#-------------------------------------

# First ten valid product IDs
# public_info()[1:10,]

#-------------------------------------
# Functions required by the bot
#-------------------------------------

# Function to get the balance (EUR)
curr_bal_eur <- function(x){
  m <-  accounts(api.key = api_key, 
                 secret = secret, 
                 passphrase = passphrase
        )
  m <-  subset(m$available, m$currency == 'EUR')
  return(as.numeric(m))
}

# Function to get the balance (BTC)
curr_bal_btc <- function(x){
  n <- accounts(api.key = api_key, 
                secret = secret, 
                passphrase = passphrase
       )
  n <- subset(n$available, n$currency == 'BTC')
  return(as.numeric(n))
}

# Function to get the current RSI14
rsi14_api_fkt <- function(x){
  df <- rgdax::public_candles(product_id = "BTC-EUR",
                              granularity = 60) # 1 minute
  rsi_gdax <- tail(TTR::RSI(df[,5],
                            n = 14),
                   n = 1)
  rsi_gdax
}

# Function to get the RSI14 (less one)
rsi14_api_less_one_fkt <- function(x){
  df <- rgdax::public_candles(product_id = "BTC-EUR",
                              granularity = 60) # 1 minute
  rsi_gdax_less_one <- head(tail(TTR::RSI(df[,5],
                                          n = 14),
                                 n = 2),n=1)
  rsi_gdax_less_one
}

# Function to get the RSI14 (less two)
rsi14_api_less_two_fkt <- function(x){
  df <- rgdax::public_candles(product_id = "BTC-EUR",
                              granularity = 60) # 1 minute
  rsi_gdax_less_two <- head(tail(TTR::RSI(df[,5],
                                          n = 14),
                                 n = 3),n=1)
  rsi_gdax_less_two
}

# Function to get the RSI14 (less three)
rsi14_api_less_three_fkt <- function(x){
  df <- rgdax::public_candles(product_id = "BTC-EUR",
                              granularity = 60) # 1 minute
  rsi_gdax_less_three <- head(tail(TTR::RSI(df[,5],
                                            n = 14),
                                   n = 4),n=1)
  rsi_gdax_less_three
}

# Function to get the RSI14 (less four)
rsi14_api_less_four_fkt <- function(x){
  df <- rgdax::public_candles(product_id = "BTC-EUR",
                              granularity = 60) # 1 minute
  rsi_gdax_less_four <- head(tail(TTR::RSI(df[,5],
                                           n = 14),
                                  n = 5),n=1)
  rsi_gdax_less_four
}

# Function to get the bid price
bid <- function(x) {
  
  d.req <- fromJSON("https://api.pro.coinbase.com/products/BTC-EUR/book?level=1")
  d.dat <- unlist(d.req)
  
  return(as.numeric(d.dat[1]))
  
}

# Function to get the ask price
ask <- function(x) {
  
  d.req <- fromJSON("https://api.pro.coinbase.com/products/BTC-EUR/book?level=1")
  d.dat <- unlist(d.req)
  
  return(as.numeric(d.dat[4]))
  
}

# Function to get hold details for an account 
eur_hold <- function(x){
  holds(currency = "EUR", 
        api.key = api_key, 
        secret = secret, 
        passphrase = passphrase
  )
}

# Function to get holds
btc_hold <- function(x){
  holds <- holds(currency = "BTC", 
                 api.key = api_key, 
                 secret = secret, 
                 passphrase = passphrase
            )
  holds
}

# Function to cancel orders
cancel_orders <- function(x){
  cancel_orders <- cancel_order(order_id = "all", 
                                api.key = api_key, 
                                secret = secret, 
                                passphrase = passphrase
                   )
  cancel_orders
}

# Function that places the buy orders
buy_exe <- function(x){
  
  # Get order size as the number of coins as allowed by the EUR balance
  # order_size <- round(curr_bal_eur()/ask(),5)[1]-0.005
  order_size <- 1
  
  # While-Loop places buy orders as long as the coin balance equals zero
  while(curr_bal_btc() == 0){
    
    add_order('BTC-EUR', 
               api.key = api_key, 
               secret = secret, 
               passphrase = passphrase,
               type="limit", 
               price = bid(), 
               side = "b",  
               size = order_size
    )
    
    # Sleep to see if order takes
    Sys.sleep(20)
    
    # Check to see if BTC balance >= order amt
    if(curr_bal_btc() > 0){print("buysuccess")}else{
      
      cancel_orders() # if curr_btc_bal not > 0, cancel order and start over 
    }
  }
}

# Function that places the sell orders (if needed)
sell_exe <- function(x){
  
  # While-Loop places sell orders until the coin balance is > 0
  while(curr_bal_btc() > 0){
    
    add_order("BTC-EUR", 
               api.key = api_key, 
               secret = secret, 
               passphrase = passphrase,
               type="limit", 
               price = ask(), 
               side = "s",  
               size = curr_bal_btc()
      )
    
    # Sleep to see if order takes
    Sys.sleep(20)
    
    # Check to see if BTC balance >= order amt
    if(curr_bal_btc() == 0){print("sellsuccess")}else{
      
      cancel_orders() # if curr_eth_bal not > 0, cancel order and start over
    }
  }
}

#-------------------------------------------------
# Store calculated RSI values in variables 
# (to not exceed rate limit of API)
#-------------------------------------------------

rsi14_api            <- rsi14_api_fkt()
rsi14_api_less_one   <- rsi14_api_less_one_fkt()
rsi14_api_less_two   <- rsi14_api_less_two_fkt()
rsi14_api_less_three <- rsi14_api_less_three_fkt()
rsi14_api_less_four  <- rsi14_api_less_four_fkt()

# Show RSI values in table
df_rsi <- data.frame( RSI14 = rsi14_api,
                      RSI14_L01 = rsi14_api_less_one,
                      RSI14_L02 = rsi14_api_less_two,
                      RSI14_L03 = rsi14_api_less_three,
                      RSI14_L04 = rsi14_api_less_four
          )
df_rsi

# Colors for bar chart
cols <- rep('#7F7F7F', 5)
cols[1] <- ifelse(df_rsi$RSI14     >= RSI14_th, '#17BECF', cols[1])
cols[2] <- ifelse(df_rsi$RSI14_L01 <= RSI14_L01_th, '#17BECF', cols[2])
cols[3] <- ifelse(df_rsi$RSI14_L02  < RSI14_L02_th, '#17BECF', cols[3])
cols[4] <- ifelse(df_rsi$RSI14_L03  < RSI14_L03_th, '#17BECF', cols[4])
cols[5] <- ifelse(df_rsi$RSI14_L04  < RSI14_L04_th, '#17BECF', cols[5])
cols

# Plot RSI-values
barplot(height=t(df_rsi)[,1], 
        names=rownames(t(df_rsi)),
        ylim=c(0,100),
        xlab='RSI types',
        ylab='RSI value',
        main='Current RSI-values as used for the trading strategy',
        col = cols
)
abline(h=35, lty=2, col='gray80', lwd=3)
legend("topleft", 
       legend=c('RSI-threshold condition met', 'RSI-threshold condition not met'),
       bty='n', 
       fill=c('#17BECF', '#7F7F7F'),
       cex=1.2
)

#-------------------------------------------------
# Trading Loop (starts if all conditions are met)
#-------------------------------------------------

# Conditions of trading strategy
if(curr_bal_eur() >= 20) {      # if you have more than 20 EUR start loop
  if(rsi14_api >= RSI14_th &      # and current rsi >= threshold
     rsi14_api_less_one <= RSI14_L01_th &    # and previous close RSI <= threshold
      (rsi14_api_less_two < RSI14_L02_th | rsi14_api_less_three < RSI14_L03_th | rsi14_api_less_four < RSI14_L04_th) ) { # and i-2, i-3 or i-4 RSI < threshold
    
    #----------------------------------
    # Execute buy order
    #----------------------------------
    
    buy_exe()
    
    # Wait 180 seconds
    Sys.sleep(180)
    
    #----------------------------------
    # Save buy price in csv-file
    #----------------------------------
    
    tab = data.frame(Time=as.character(Sys.time()), Price=bid())
    write.table(tab, 
               'bid_price_log.csv', 
                sep=";", 
                row.names = FALSE, 
                col.names = FALSE,
                append=TRUE
    )
    
    #----------------------------------
    # Print for logs
    #----------------------------------
    
    print("buy")
    
    #----------------------------------
    # Enter tiered limit sell orders
    #----------------------------------
    
    # Sell-Order (1): Take 1/3 profits at 1% gain
    order_size_tiered3  <- round(curr_bal_btc()/3,3)
    order_price_tiered3 <- round(bid() * 1.01,2)
    add_order(product_id = "BTC-EUR", 
              api.key = api_key, 
              secret = secret, 
              passphrase = passphrase,
              type="limit", 
              price = order_price_tiered3, 
              side = "s",  
              size = order_size_tiered3 )
    
    Sys.sleep(20)
    
    # Sell-Order (2): Take 1/3 profits at 4% gain
    order_size_tiered5  <- round(curr_bal_btc()/2,3)
    order_price_tiered5 <- round(bid() * 1.04,2)
    add_order(product_id = "BTC-EUR", 
              api.key = api_key, 
              secret = secret, 
              passphrase = passphrase,
              type="limit", 
              price = order_price_tiered5, 
              side = "s",  
              size = order_size_tiered5 )
    
    Sys.sleep(20)
    
    # Sell-Order (3): take 1/3 profits at 7% gain
    order_size_tiered8  <- round(curr_bal_btc(),3)
    order_price_tiered8 <- round(bid() * 1.07,2)
    add_order(product_id = "BTC-EUR", 
              api.key = api_key, 
              secret = secret, 
              passphrase = passphrase,
              type="limit", 
              price = order_price_tiered8, 
              side = "s",  
              size = order_size_tiered8 )
    
  }else{cat("System time:", as.character(Sys.time()), "|", "no coins traded so far", "\n")}
}else{cat("System time:", as.character(Sys.time()), "|", "no coins traded so far", "\n")}

Sys.sleep(30)

}