#library(TSA)
#library(astsa)
#library(forecast)
#library(tseries)
#library(dplyr)
#library(lmtest)
#library(stringr)
library(lubridate)
library(ggplot2)
library(chron)
library(timeDate)
library(tidyverse)
library(caret)
library(neuralnet)
library(GGally)

# THIS PREDICTIVE PROGRAM IS DESIGNED TO DEAL WITH DATA WITH THE FOLLOWING PROPERTIES:
# 1) OVER A YEAR OR MORE- NEED LOTS OF DATA TO BUILD THE MACHINE LEARNING MODEL
# 2) WITHOUT SIGNIFICANT OUTLIERS - IF IT DOES, RESULTS ARE LESS TRUSTWORTHY
# 3) TRADED FREQUENTLY - RARE GOODS ARE NOT AS SUBJECT TO MARKET TRENDS AND SEASONS
#
# IT IS INTENDED TO BE USED IN CONJUNCTION WITH GEPREDICT - GE PRICE EXTRACTOR

# THIS WILL EXTRACT RELEVANT DATA FOR THE MACHINE LEARNING MODEL, INCLUDING-
#   1) time series data- prices for the item in the previous week
#   2) occurrence of day type (holiday, weekend, weekday)
#   3) occurrence of in-game event (double exp, boss release)
#   4) how long the item has been available for trading
#   5) difficulty in getting item (may need community rating)

#### SETUP ####
# PATH TO DATA (THE FOLDER THAT HOLDS ALL THE DATA)
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(paste(path,"/data",sep=''))
getwd()

# IMPORTS DATA INTO RSTUDIO
filename = "ItemID 389, 2022-03-27, 730D.csv"
item = read.csv(filename, header = TRUE)

#### 1) SETTINGS: TIME SERIES AND PREDICTIONS ####
# NUMBER OF PREDICTION DAYS
predictdays = 60

# ITEM NAME
itemname = "Manta ray"

#### 2) DATA PREPPING ####
#lengthen differenced data to length of list before additional cleaning
NAtoLength <- function(inputlist,listlength){
  if(length(inputlist) != listlength){
    inputlist2=c(NA,inputlist)
    NAtoLength(inputlist2,listlength)
  }
  else{
    return(inputlist)
  }
}

# get the difference in price from prior days (past week)
diff1 = diff(item$GE.Price, lag = 1)
diff2 = diff(item$GE.Price, lag = 2)
diff3 = diff(item$GE.Price, lag = 3)
diff4 = diff(item$GE.Price, lag = 4)
diff5 = diff(item$GE.Price, lag = 5)
diff6 = diff(item$GE.Price, lag = 6)
diff7 = diff(item$GE.Price, lag = 7)
diff14 = diff(item$GE.Price, lag = 14)
diff21= diff(item$GE.Price, lag = 21)
diff30 = diff(item$GE.Price, lag = 30)

# get time-lagged prices
item$"t1" = item$GE.Price-NAtoLength(diff1,length(item$GE.Price)) #yesterday
item$"t2" = item$GE.Price-NAtoLength(diff2,length(item$GE.Price))
item$"t3" = item$GE.Price-NAtoLength(diff3,length(item$GE.Price))
item$"t4" = item$GE.Price-NAtoLength(diff4,length(item$GE.Price))
item$"t5" = item$GE.Price-NAtoLength(diff5,length(item$GE.Price))
item$"t6" = item$GE.Price-NAtoLength(diff6,length(item$GE.Price))
item$"t7" = item$GE.Price-NAtoLength(diff7,length(item$GE.Price)) #last week
item$"t14" = item$GE.Price-NAtoLength(diff14,length(item$GE.Price)) #last 2 week
item$"t21" = item$GE.Price-NAtoLength(diff21,length(item$GE.Price)) #last 3 week
item$"t30" = item$GE.Price-NAtoLength(diff30,length(item$GE.Price)) #last 30 days

# get 30-day average price
pastmonthAVG = rep(NA,30)
for(d in 31:length(item$GE.Price)){
  pastmonthAVG = append(pastmonthAVG,mean(item$GE.Price[(d-30):(d-1)],na.rm=TRUE))  
}
item$"PastMonthAVG" = pastmonthAVG

# get whether the date was on a holiday and weekend/weekday
item$Date = as.Date(item$Date)
hlist <- c("USChristmasDay","USGoodFriday","USIndependenceDay","USLaborDay",
           "USNewYearsDay","USThanksgivingDay")        
myholidays  <- dates(as.character(holiday(2000:2050,hlist)),format="Y-M-D")

item = item %>% mutate(weekend = ifelse(is.weekend(Date),1,0), holiday = ifelse(is.holiday(Date,myholidays),1,0)) 

# get whether or not the date of the price was during an event

# get DXP occurance dates

# get DXP annoucement dates

# get the date of boss releases + a week or so

# etc

item_clean = item[rowSums(is.na(item))==0,]
item_clean_data = item_clean %>% select(-Date) %>% mutate_all(as.numeric) 

#item_clean_data_scaled = item_clean_data %>% select(-weekend, -holiday) %>% scale() %>% as.data.frame()
#item_clean_data_scaled$weekend = item_clean_data$weekend
#item_clean_data_scaled$holiday = item_clean_data$holiday

#### 3) DATA PARTITIONING / CROSS VALIDATION FUNCTION ####

set.seed(123)

# K-FOLDS CROSS VALIDATION FUNCTION
# consider alteration to allow different activation functions: 
# i.e. act.fct = "logistic", act.fct="tanh",
NNmodelEval = function(folds, data, hiddenlayers, thresh){
  
  RSMErecord = c()
  
  for(i in 1:folds){
    index <- createDataPartition(data$GE.Price, p = 0.7,list = FALSE)
    
    train <- data[index, ]
    preProcVals = preProcess(train[1:12], method = c("center", "scale"))
    covMean = preProcVals$mean
    covStd = preProcVals$std
    
    train[1:12] = predict(preProcVals,train[1:12])
    test  <- data[-index, ]
    test[1:12] = predict(preProcVals,test[1:12])
    
    testy<-test[,"GE.Price"]
    testx<-test[,-1]
    
    nnfit <-neuralnet(GE.Price ~., data=train, hidden = hiddenlayers, 
                      linear.output=TRUE, threshold=thresh,
                      stepmax=1e6) 
    nnfit$result.matrix  
    
    #plot(nnfit) # - USE TO SEE STRUCTURE OF NN MODEL, SOMEWHAT
    nnfit.results <- compute(nnfit,testx)
    
    #Compute error statistics
    resultnnfit <-nnfit.results$net.result
    results <- data.frame(actual = test$GE.Price, prediction = nnfit.results$net.result)
    head(results)
    
    RMSE_t = sqrt(sum(abs(testy - resultnnfit))^2)
    RSMErecord = c(RSMErecord,RMSE_t)
  }
  
  # RETURN LIST OF RSMES
  return (RSMErecord)
}

#### 4) NEURAL NET TESTING - RSME ANALYSIS

RSMElist = list()
k = 10

# MODEL 1
# 1 hidden layer, k-folds, 0.01 threshold
RSMElist[[1]]= NNmodelEval(k, item_clean_data, 1, 0.1)

# MODEL 2
# 3,2 hidden layer, k folds, 0.05 threshold
layers= c(3,2)
RSMElist[[2]]=NNmodelEval(k, item_clean_data, layers, 0.1)

# MODEL 3
# 4,1 hidden layer, k folds, 0.07 threshold
layers= c(4,1)
RSMElist[[3]]=NNmodelEval(k, item_clean_data, layers, 0.1)

# MODEL 4
# 5,2 hidden layer, k folds, 0.05 threshold
layers= c(5,2)
RSMElist[[4]]=NNmodelEval(k, item_clean_data, layers, 0.1)

# MODEL 5
# 5,3,1 hidden layer, k folds, 0.05 threshold
layers= c(5,3,1)
RSMElist[[5]]=NNmodelEval(k, item_clean_data, layers, 0.1)

# COLLECTION OF DATA FOR BOXPLOT COMPARISON
RSMEdata = c()
for (i in RSMElist){
  RSMEdata = c(RSMEdata,i)
}

RSMEID = c()
for (i in 1:length(RSMElist)){
  RSMEID = c(RSMEID, rep(i,length(RSMElist[[i]])))
}

boxDATA = data.frame("RSME"=RSMEdata, "Model" = RSMEID)
boxDATA$Model = as.factor(boxDATA$Model)

# BOXPLOT
boxDATA %>% ggplot(aes(x=Model, y=RSME)) + 
  geom_boxplot(aes(fill=Model)) +
  stat_summary(fun=mean, geom="point", shape=1, size=2) +
  labs(title=paste("RSME distributions of ", k,"-fold cross-validated neural net models",sep="")) +
  theme_classic()

#### 5) NEURAL NET PREDICTIONS ####
# GET SCALING TRANSFORMATION AND COMPONENTS TO REVERT FOR LATER
index <- createDataPartition(item_clean_data$GE.Price, p = 0.7,list = FALSE)

train <- item_clean_data[index, ]
preProcVals = preProcess(train[1:12], method = c("center", "scale"))
covMean = preProcVals$mean
covStd = preProcVals$std

train[1:12] = predict(preProcVals,train[1:12])
test  <- item_clean_data[-index, ]
test[1:12] = predict(preProcVals,test[1:12])

head(train)

# PUT FINAL PARAMS HERE
selectedNN = neuralnet(GE.Price ~., data=train, hidden = 1, linear.output=TRUE, threshold=.01, stepmax=1e7) 

item_predictions = item_clean

currentDate = item_predictions$Date[length(item_predictions$Date)]

#future predictdays prediction
for(i in 1:predictdays){
  currentDate = currentDate + 1
  newrow = list(currentDate,0, 
             item_predictions$GE.Price[length(item_predictions$GE.Price)],
             item_predictions$GE.Price[length(item_predictions$GE.Price)-1],
             item_predictions$GE.Price[length(item_predictions$GE.Price)-2],
             item_predictions$GE.Price[length(item_predictions$GE.Price)-3],
             item_predictions$GE.Price[length(item_predictions$GE.Price)-4],
             item_predictions$GE.Price[length(item_predictions$GE.Price)-5],
             item_predictions$GE.Price[length(item_predictions$GE.Price)-6],
             item_predictions$GE.Price[length(item_predictions$GE.Price)-13],
             item_predictions$GE.Price[length(item_predictions$GE.Price)-20],
             item_predictions$GE.Price[length(item_predictions$GE.Price)-29],
             mean(item_predictions$GE.Price[(length(item_predictions$GE.Price)-29):length(item_predictions$GE.Price)],
                  na.rm=TRUE),
             ifelse(is.weekend(currentDate),1,0), 
             ifelse(is.holiday(currentDate,myholidays),1,0))
  
  item_predictions = rbind(item_predictions, newrow)
  
  item_predictions[length(item_predictions$GE.Price),2:13] = predict(preProcVals,
                                                                     item_predictions[length(item_predictions$GE.Price),2:13])
  item_predictions[length(item_predictions$GE.Price),2] = compute(selectedNN,
                                                                  item_predictions[length(item_predictions$GE.Price),3:15])$net.result
  item_predictions[length(item_predictions$GE.Price),2:13] = item_predictions[length(item_predictions$GE.Price),2:13]*covStd+covMean

}
len = length(item_predictions$Date)
GE.Price.Predict = item_predictions[(len-predictdays+1):len,1:2]

#most recent predictdays verification check
currentDate = item_predictions$Date[length(item_predictions$Date)]-2*predictdays-1
for(i in 1:predictdays){
  currentDate = currentDate + 1
  newrow = list(currentDate,0, 
                item_predictions$GE.Price[length(item_predictions$GE.Price)],
                item_predictions$GE.Price[length(item_predictions$GE.Price)-1],
                item_predictions$GE.Price[length(item_predictions$GE.Price)-2],
                item_predictions$GE.Price[length(item_predictions$GE.Price)-3],
                item_predictions$GE.Price[length(item_predictions$GE.Price)-4],
                item_predictions$GE.Price[length(item_predictions$GE.Price)-5],
                item_predictions$GE.Price[length(item_predictions$GE.Price)-6],
                item_predictions$GE.Price[length(item_predictions$GE.Price)-13],
                item_predictions$GE.Price[length(item_predictions$GE.Price)-20],
                item_predictions$GE.Price[length(item_predictions$GE.Price)-29],
                mean(item_predictions$GE.Price[(length(item_predictions$GE.Price)-29):length(item_predictions$GE.Price)],
                     na.rm=TRUE),
                ifelse(is.weekend(currentDate),1,0), 
                ifelse(is.holiday(currentDate,myholidays),1,0))
  
  item_predictions = rbind(item_predictions, newrow)
  
  item_predictions[length(item_predictions$GE.Price),2:13] = predict(preProcVals,
                                                                     item_predictions[length(item_predictions$GE.Price),2:13])
  item_predictions[length(item_predictions$GE.Price),2] = compute(selectedNN,
                                                                  item_predictions[length(item_predictions$GE.Price),3:15])$net.result
  item_predictions[length(item_predictions$GE.Price),2:13] = item_predictions[length(item_predictions$GE.Price),2:13]*covStd+covMean
  
}

#### 6) PLOTTING PREDICTION ####
len = length(item_predictions$Date)
#GE.Price.Predict = item_predictions[(len-predictdays+1):len,1:2]

metadatalist = rev(c(rep("Validation",predictdays),rep("Prediction",predictdays),rep("Data",len-2*predictdays)))

item_predictions$metadata = metadatalist

item_predictions %>% ggplot(aes(x = Date, y = GE.Price)) + 
  geom_line(aes(col=metadata, group = 1))+
  #geom_point(aes(col=metadata, group = 1))+
  labs(title = paste("Neural Net Prediction:",itemname,seq=" "),
       x="Date", y="Grand Exchange Price (gp)")+
  scale_color_discrete(name = "Data Type")+
  theme_light()

validLeft = item_predictions %>% filter(metadata == "Validation") %>% select(Date,GE.Price) %>% 
  mutate(Date = as.character((Date)))
validRight = item_predictions %>% filter(metadata != "Validation") %>% select(Date,GE.Price) %>%
  mutate(Date = as.character((Date)))

validJoined = merge(x=validLeft, y=validRight, by="Date")

validation = validJoined %>% mutate(GEDiff = GE.Price.x - GE.Price.y) %>%
  summarize("Date"=Date,"GE Predicted"=GE.Price.x, "GE Real" = GE.Price.y, "GE Difference" = GEDiff, "GE % Diff" = GEDiff/GE.Price.y*100)

mean(validation$`GE % Diff`)

validation %>% ggplot(aes(x = `GE % Diff`)) + 
  geom_histogram(aes(y=..density..),binwidth=1, color="black", fill="blue") +
  geom_density(alpha=.2, fill="red") +
  geom_vline(aes(xintercept=mean(`GE % Diff`)), color="black", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(`GE % Diff`)), color="gray", linetype="dashed", size=1)+
  theme_minimal()

  
#### 7) EXPORT DATA ####
path = getwd()
path = str_replace(path, "RS3 - GEPredict/data", "RS3 - GEPredict/forecast")

exit = paste(path,"/",itemname," ",predictdays,"D NN Validation.csv",sep="")
write.csv(validation,exit, row.names = FALSE)

exit = paste(path,"/",itemname," ",predictdays,"D NN Forecast.csv",sep="")
write.csv(GE.Price.Predict,exit, row.names = FALSE)
