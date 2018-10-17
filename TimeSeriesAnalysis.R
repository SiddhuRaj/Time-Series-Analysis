#################################################################
## Author:  Siddhu Raj
## Date:    2018-10-16
## Title:   TimeSeriesAnalysis.R
## BitCoin data obtained from coindesk.com.
## Datasets involved -  S&P500 (SP500), the London bullion market price for gold in US dollars (GOLDAMGBD228NLBM), the US/Euro exchange rate (DEXUSEU), and the West Texas Intermediate spot price of oil (DCOILWTICO). 
## Purpose: Correcting spurious correlation between the datasets. Forecasting BitCoin Prices using ARIMA & VAR models
#################################################################

rm(list=ls())
library(ggplot2)
library(gtable)
library(grid)
library(tseries)
library(sandwich)
library(tidyverse)
library(broom)
library(lmtest)
library(forecast)
library(fastDummies)
library(TSA)
library(vars)
my_dataset <- as.data.frame(DataSet)
my_dataset$Date <- as.Date(my_dataset$Date)
class(my_dataset$Date)

p_bitcoin <- ggplot(my_dataset,aes(x=Date,y=BitCoin)) + geom_line() 
grid.draw(p_bitcoin)
p_SP500 <- ggplot(my_dataset,aes(x=Date,y=SP500)) + geom_line()
grid.draw(p_SP500)
p_GOLDAMGBD228NLBM <- ggplot(my_dataset,aes(x=Date,y=GOLDAMGBD228NLBM )) + geom_line()
grid.draw(p_GOLDAMGBD228NLBM)
p_DEXUSEU <- ggplot(my_dataset,aes(x=Date,y=DEXUSEU )) + geom_line()
grid.draw(p_DEXUSEU)
p_DCOILWTICO <- ggplot(my_dataset,aes(x=Date,y=DCOILWTICO )) + geom_line()
grid.draw(p_DCOILWTICO)
model <- lm(BitCoin~SP500+GOLDAMGBD228NLBM+DEXUSEU+DCOILWTICO, data=my_dataset)
summary(model) #SP500,GOLDAMGBD228NLBM,DEXUSEU and DCOILWTICO are significant with respect to BitCoin Prices. This is because the data is not stationarized. Time-Series Data should be stationarized before working on it.

#Stationarizing all the components of time-series data at the usual 5% significant level
kpss.test(my_dataset$BitCoin) # p-value=0.01. 
auto.kpss <- function(series,alpha=0.05,d=5){
  diff <- 0
  for(i in 1:d){
    suppressWarnings(pval <- kpss.test(series,null="Level")$p.value)
    if(pval>=alpha){
      return(c(diff,"Level",pval))
    }
    suppressWarnings(pval <- kpss.test(series,null="Trend")$p.value)
    if(pval>=alpha){
      return(c(diff,"Trend",pval))
    }
    diff <- diff + 1
    series <- diff(series)
  }
  return(NULL)
}
auto.kpss(my_dataset$BitCoin) # Level-Stationary in first diff.  p-value=0.1
auto.kpss(my_dataset$SP500) # Level-Stationary in first diff. p-value=0.1
auto.kpss(my_dataset$GOLDAMGBD228NLBM) #Level-Stationary in first diff. p-value=0.1
auto.kpss(my_dataset$DEXUSEU) # Level-Stationary in first diff. p-value=0.08695002
auto.kpss(my_dataset$DCOILWTICO) # Trend-Stationary in first diff. p-value= 0.1

ts.plot(diff(my_dataset$BitCoin))
ts.plot(diff(my_dataset$SP500))
ts.plot(diff(my_dataset$GOLDAMGBD228NLBM))
ts.plot(diff(my_dataset$DEXUSEU))
ts.plot(diff(my_dataset$DCOILWTICO))
model2 <- lm(diff(BitCoin)~diff(SP500)+diff(GOLDAMGBD228NLBM)+diff(DEXUSEU)+diff(DCOILWTICO)+as.numeric(my_dataset$Date)[2:1155], data=my_dataset)
summary(model2) 
# Newey-West correction
coeftest(model2,vcov=NeweyWest(model2,lag=10)) #After correcting for heteroskedasticity & auto-correlation, we find that there is no significance to any of the variables.

# Considering only the bitcoin data after 2017 to predict the forecast for the next 30 days.
subset_my_dataset <- subset(my_dataset, my_dataset>'2016-12-31')
subset_my_dataset <- na.omit(subset_Bitcoin)
View(subset_my_dataset)
subP_bitcoin <- ggplot(subset_my_dataset,aes(x=Date,y=BitCoin)) + geom_line() 
grid.draw(subP_bitcoin)

# Using ACF & PACF to determine the number of AR & MA terms in ARIMA model!
acf(diff(subset_data$BitCoin))
pacf(diff(subset_data$BitCoin))
auto.arima(subset_data$BitCoin,d=1,max.p=10,max.q=10)
model1 <- stats::arima(subset_data$BitCoin, c(0,1,0))
summary(model1)
forec1 <- forecast(model1,h=30)
plot(forec1)
forec1$mean
# The forecast is flat indicating that there is no seasonality in the data!
# Verifying using Periodogram to observe any seasonality
periodogram(diff(subset_data$BitCoin)) #No proper Seasonality observed.

#Using VAR model to compare the forecasts from ARIMA model
diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}
x <- subset_my_dataset %>% dplyr::select(BitCoin,SP500,GOLDAMGBD228NLBM,DEXUSEU,DCOILWTICO) %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC
model2 <- VAR(x,p=2,type="both")
summary(model2)
n <- nrow(subset_my_dataset)
forec2 <- predict(model2,n.ahead=30)$fcst$BitCoin
forec2 <- forec2[,1]
forec2
cumsum(forec2)
subset_my_dataset$BitCoin[n]
forec2 <- subset_my_dataset$BitCoin[n] + cumsum(forec2)
forec2
cbind(forec1$mean,forec2) #forec1 - ARIMA forecast; forec2 - VAR forecast
# We can compare the predictions between ARIMA & VAR model. ARIMA model gives a flat forecast but VAR model provides a better prediction than ARIMA model.
# Time Series:
#   Start = 418 
# End = 447 
# Frequency = 1 
# forec1$mean   forec2
# 418     6492.37 6470.319
# 419     6492.37 6466.663
# 420     6492.37 6445.933
# 421     6492.37 6430.152
# 422     6492.37 6412.245
# 423     6492.37 6394.434
# 424     6492.37 6376.345
# 425     6492.37 6358.228
# 426     6492.37 6339.890
# 427     6492.37 6321.419
# 428     6492.37 6302.802
# 429     6492.37 6284.039
# 430     6492.37 6265.128
# 431     6492.37 6246.070
# 432     6492.37 6226.866
# 433     6492.37 6207.514
# 434     6492.37 6188.016
# 435     6492.37 6168.371
# 436     6492.37 6148.580
# 437     6492.37 6128.641
# 438     6492.37 6108.556
# 439     6492.37 6088.324
# 440     6492.37 6067.945
# 441     6492.37 6047.419
# 442     6492.37 6026.747
# 443     6492.37 6005.928
# 444     6492.37 5984.962
# 445     6492.37 5963.849
# 446     6492.37 5942.590
# 447     6492.37 5921.183