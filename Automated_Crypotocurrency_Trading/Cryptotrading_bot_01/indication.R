indication <- function(macd1, rsi1){
  
  strategy1 <- ifelse ((macd1$signal < macd1$macd), 1, 0)
  strategy1[is.na(strategy1)] <- 0
  
  
  strategy2 <- ifelse (macd1$signal < macd1$macd & (rsi1$rsi > 60), 1, 0)
  strategy2[is.na(strategy2)] <- 0
  
  
  strategy3 <- ifelse (macd1$signal > macd1$macd & (rsi1$rsi < 30), 1, 0)
  strategy3[is.na(strategy3)] <- 0
  
  
  decision <- ifelse(strategy2 == 1, "Sell", 
                     ifelse(strategy3 == 1, "Buy", "Wait"))
  
  
  df <- cbind(datetime = data$open_time, 
               contrast = ifelse(strategy1, "Signal < MACD", "MACD < Signal"),
               RSI = rsi1, decision, price = data$open) 
  
  return(df)
}