library(TSA)
library(astsa)
library(forecast)
library(tseries)
library(dplyr)
library(lmtest)
library(stringr)
library(lubridate)
library(ggplot2)

# THIS PREDICTIVE PROGRAM IS DESIGNED TO DEAL WITH DATA WITH THE FOLLOWING PROPERTIES:
# 1) OVER A YEAR OR MORE
# 2) WITHOUT SIGNIFICANT OUTLIERS - IF IT DOES, RESULTS ARE LESS TRUSTWORTHY
# 3) TRADED FREQUENTLY - RARE GOODS ARE NOT AS SUBJECT TO MARKET TRENDS AND SEASONS
#
# IT IS INTENDED TO BE USED IN CONJUNCTION WITH GEPREDICT - GE PRICE EXTRACTOR

# GUIDE (REQUIRES SOME CODING KNOW-HOW):
# COMPLETE THE SETUP (SET PATH AND FILE TO IMPORT)
# RUN THROUGH SECTIONS 1-5:
#   1) SET UP THE DATA AND NUMBER OF DAYS TO PREDICT
#   2) LOOK AT DATA IN PLOT
#   3) LOOK AT AUTOCORRELATION FOR POTENTIALLY BIASED DATA
#   4) BREAK DOWN DATA INTO TREND, SEASONALITY, and RANDOMNESS (ADDITIVELY)
#   5) BREAK DOWN DATA LIKE ABOVE (MULTIPLICATIVELY)
#
# FOR DATA WITH MINIMAL SEASONALITY / NO OBVIOUS PATTERNS, SECTION 6
# FOR DATA THAT IS NOT STATIONARY, SECTION 7
# FOR DATA THAT HAS SEASONALITY, SECTION 8

#### SETUP ####
# PATH TO DATA (THE FOLDER THAT HOLDS ALL THE DATA)
getwd()
setwd("C:/Users/yangs/Documents/RS3 - GEPredict/data")

# IMPORTS DATA INTO RSTUDIO
filename = "ItemID 51096, 2021-07-30, 180D.csv"
item = read.csv(filename, header = TRUE)

#### 1) SETTINGS: TIME SERIES AND PREDICTIONS ####
#SET HOW A YEAR IS SUBDIVIDED
freq = 90

#DAYS THE FIRST DATE IN THE DATA SET IS AWAY FROM BEGINNING OF ITS YEAR
day1 = item$Date[1]
day1.year = as.character(year(day1))
day1 = as.Date(paste(day1.year,"-01-01",sep=""))

dayfromstart = as.numeric(difftime(as.Date(item$Date[1]),day1, units = c("days")))

#FIND WHICH SECTION AND LOCATION IN SECTION IS IN THERE
timesec = ceiling(dayfromstart/freq)
subspot = dayfromstart %% freq

# NUMBER OF PREDICTION DAYS
predictdays = 14

# ITEM NAME
itemname = "Greater chain codex"

# CONVERTS DATA TO TIME SERIES
GEItem=ts(item$GE.Price,start=c(timesec,subspot),frequency=freq)


#### 2) INITAL PLOT ####
plot(GEItem, type = "o")

#### 3) AUTOCORRELATION FUNCTIONS ####
acf(GEItem,length(GEItem))
pacf(GEItem,length(GEItem))

#### 4) ADDITIVE DECOMPOSITION ####
plot(decompose(GEItem)) #Additive decomp

#### 5) MULTIPLICATIVE DECOMPOSITION ####
decomp=decompose(GEItem,type="mult")
plot(decomp)

trend = decomp$trend
season = decomp$season
random = decomp$random

GEItem.TS=cbind(trend,trend*season)
plot(GEItem.TS,main="Item: Trend, and Trend*Season")

GEItem.TR=cbind(trend,trend*random)
plot(GEItem.TR,main="Item: Trend, and Trend*Season")

GEItem.SR=cbind(season,season*random)
plot(GEItem.SR,main="Item: Trend, and Trend*Season")

#### 6) NON-STATIONARY DATA -> DATA SMOOTHING ####
#potentially use smoothed data in case GEItem is not stationary
#adjust params in smoothing methods to get to stationary data

#FIRST DIFFERENCING w/o seasonality
adj = GEItem/season
plot(adj)
diff = diff(adj, differences = 1)
plot(diff)
acf(diff,length(diff))
pacf(diff,length(diff))

#FIRST DIFFERENCING w/ seasonality
diff2 = diff(GEItem, differences = 1)
plot(diff2)
acf(diff2,length(diff2))
pacf(diff2,length(diff2))

#BANDWIDTH
plot(GEItem, type="p")
lines(ksmooth(time(GEItem), GEItem, "normal"))
lines(ksmooth(time(GEItem), GEItem, "normal", bandwidth=subspot/freq))

band = ksmooth(time(GEItem), GEItem, "normal", bandwidth=subspot/freq)$y
band=ts(band,start=c(timesec,subspot),frequency=freq)

acf(band,length(band))
pacf(band,length(band))

#NEAREST NEIGHBOR
plot(GEItem, type="p") #PICK BEST LOOKING NEAREST NEIGHBOR LINE
lines(supsmu(time(GEItem), GEItem, span=.5))
lines(supsmu(time(GEItem), GEItem, span=.25))
lines(supsmu(time(GEItem), GEItem, span=.05))
lines(supsmu(time(GEItem), GEItem, span=.01))

nearest = supsmu(time(GEItem), GEItem, span=.05)$y
nearest=ts(nearest,start=c(timesec,subspot),frequency=freq)

acf(nearest,length(nearest))
pacf(nearest,length(nearest))

#LOWESS
plot(GEItem, type="p")
lines(lowess(GEItem, f=.5))
lines(lowess(GEItem, f=.25))
lines(lowess(GEItem, f=.1))
lines(lowess(GEItem, f=.05))

lowess = lowess(GEItem, f=.05)$y
lowess=ts(lowess,start=c(timesec,subspot),frequency=freq)

acf(lowess,length(lowess))
pacf(lowess,length(lowess))

#SMOOTH SPLINE
plot(GEItem, type="p")
lines(smooth.spline(time(GEItem), GEItem))
lines(smooth.spline(time(GEItem), GEItem, spar=.5))

spline = smooth.spline(time(GEItem), GEItem, spar=0.5)$y
spline=ts(spline,start=c(timesec,subspot),frequency=freq)

acf(spline,length(spline))
pacf(spline,length(spline))

#USE ADF TEST TO LOOK FOR STATIONARITY ISSUES and select data with best p-val
adf.test(diff) # Make sure to add this to final observation of GEItem data
adf.test(diff2) # or this
adf.test(band)
adf.test(nearest)
adf.test(lowess)
adf.test(spline)
adf.test(GEItem)


#### 7) MINIMAL SEASONALITY -> HOLT WINTERS FORECASTING #### 
GEItem.hw<-HoltWinters(GEItem,seasonal="mult")# seasonal effect can be additive or multiplicative
plot(GEItem.hw)

GEItem.hwp<-predict(GEItem.hw,n.ahead=predictdays)
plot(GEItem.hw, GEItem.hwp)

GEItem.hwp2<-predict(GEItem.hw,n.ahead=predictdays,prediction.interval = TRUE,level=.95)
plot(GEItem.hw, GEItem.hwp2)

dates = seq.Date(Sys.Date(), by='day',length.out=predictdays+1)[-1]
fc = data.frame(dates,GEItem.hwp2)
colnames(fc)=c("Date","Predicted GE","Upper 95% PI", "Lower 95% PI")
fc # forecast price

#### 7b) FIRST DIFFERENCE HOLT WINTERS FORECASTING) ####
#  NOT RECOMMENDED (Holt-Winters works best w/ non-stationary data
diff.hw<-HoltWinters(diff,seasonal="mult")# seasonal effect can be additive or multiplicative
plot(diff.hw)

diff.hwp<-predict(diff.hw,n.ahead=predictdays)
plot(diff.hw, diff.hwp)

diff.hwp2<-predict(diff.hw,n.ahead=predictdays,prediction.interval = TRUE,level=.95)
plot(diff.hw, diff.hwp2)

dates = seq.Date(Sys.Date(), by='day',length.out=predictdays+1)[-1]
fc = data.frame(dates,diff.hwp2)
colnames(fc)=c("Date","Predicted GE","Upper 95% PI", "Lower 95% PI")
fc$`Predicted GE`[1] = fc$`Predicted GE`[1] + GEItem[length(GEItem)]
fc$`Upper 95% PI`[1] = fc$`Upper 95% PI`[1] + GEItem[length(GEItem)]
fc$`Lower 95% PI`[1] = fc$`Lower 95% PI`[1] + GEItem[length(GEItem)]

for (i in 1:(length(fc$Date)-1)){
  fc$`Predicted GE`[i+1] = fc$`Predicted GE`[i] + fc$`Predicted GE`[i+1]
  fc$`Upper 95% PI`[i+1] = fc$`Upper 95% PI`[i] + fc$`Upper 95% PI`[i+1]
  fc$`Lower 95% PI`[i+1] = fc$`Lower 95% PI`[i] + fc$`Lower 95% PI`[i+1]
}

fc # forecast price

#### 8) SIGNIFICANT SEASONALITY -> SARIMA MODELING + FORECASTING ####
# NOT RECOMMENDED- BEST USED WITH STATIONARY DATA
auto.arima(GEItem, trace=TRUE) 
fitARIMA <- Arima(GEItem, order=c(3,1,2),seasonal = list(order = c(0,0,0), period = freq),lambda = 0)
#coeftest(fitARIMA) 
#confint(fitARIMA)

fitARIMA %>% forecast(h = predictdays) %>% autoplot()

pred = fitARIMA %>% forecast(h = predictdays)
dates = seq.Date(Sys.Date(), by='day',length.out=predictdays+1)[-1]
fc2 = data.frame(dates,pred$mean,pred$upper[,2],pred$lower[,2])
colnames(fc2)=c("Date","Predicted GE","Upper 95% PI", "Lower 95% PI")
fc2 #FORECAST PRICE

#### 8b) FIRST DIFFERENCE SARIMA MODELING + FORECASTING ####
auto.arima(diff, trace=TRUE) 
fitARIMA <- Arima(diff, order=c(4,0,1),seasonal = list(order = c(0,0,1), period = freq),lambda = 0)
#coeftest(fitARIMA) 
#confint(fitARIMA)

fitARIMA %>% forecast(h = predictdays) %>% autoplot()

pred = fitARIMA %>% forecast(h = predictdays)
dates = seq.Date(Sys.Date(), by='day',length.out=predictdays+1)[-1]
fc2 = data.frame(dates,pred$mean,pred$upper[,2],pred$lower[,2])
colnames(fc2)=c("Date","Predicted GE","Upper 95% PI", "Lower 95% PI")
fc2 #FORECAST PRICE

fc2$`Predicted GE`[1] = fc2$`Predicted GE`[1] + GEItem[length(GEItem)]
fc2$`Upper 95% PI`[1] = fc2$`Upper 95% PI`[1] + GEItem[length(GEItem)]
fc2$`Lower 95% PI`[1] = fc2$`Lower 95% PI`[1] + GEItem[length(GEItem)]

for (i in 1:(length(fc2$Date)-1)){
  fc2$`Predicted GE`[i+1] = fc2$`Predicted GE`[i] + fc2$`Predicted GE`[i+1]
  fc2$`Upper 95% PI`[i+1] = fc2$`Upper 95% PI`[i] + fc2$`Upper 95% PI`[i+1]
  fc2$`Lower 95% PI`[i+1] = fc2$`Lower 95% PI`[i] + fc2$`Lower 95% PI`[i+1]
}

fc2

#### 9) EXPORT DATA TO CSV ####
path = getwd()
path = str_replace(path, "RS3 - GEPredict/data", "RS3 - GEPredict/forecast")

# HOLT-WINTERS FORECAST
exit = paste(path,"/",itemname," ",predictdays,"D HW Forecast.csv",sep="")
write.csv(fc,exit, row.names = FALSE)

# SARIMA FORECAST
exit = paste(path,"/",itemname," ",predictdays,"D SARIMA Forecast.csv",sep="")
write.csv(fc2,exit, row.names = FALSE)

