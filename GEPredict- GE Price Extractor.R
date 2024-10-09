#install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tibble)
library(lubridate)

# THIS PROGRAM WILL DRAWS DAILY GE PRICE DATA FROM THE RUNESCAPE WIKI API:
# i.e. https://api.weirdgloop.org/exchange/history/rs/all?id=41961&lang=en.json
#
# THE NUMBER CAN BE REPLACED WITH THE SPECIFIC ITEM ID
# NUMBER OF DAYS IN DATA CAN ALSO BE SET
# IT IS INTENDED TO ONLY WORK WITH TRADEABLE ITEMS

#### SETUP ####
basetime = as.POSIXct("1970-01-01 12:00:00", tz = "EST") 

#setwd("C:/Users/yangs/Documents/RS3 - GEPredict/data")
#path = getwd()

path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(paste(path,"/data",sep=''))
getwd()

#### SETTINGS ####
#ADJUST FOR SPECIFIC ITEM DATA AND DAYS
itemID = 389
days = 365*2

#### GE DAILY PRICE GATHER ####
reference = paste("https://api.weirdgloop.org/exchange/history/rs/all?id=",itemID,"&lang=en.json",sep="")
res = GET(reference)

data = fromJSON(rawToChar(res$content))
list <- enframe(data)
df = list[[2]][[1]]
df = df[,c(-1,-3)]

# GE DAILY PRICE REFORMATING
df = tail(df,days)
df = data.frame(df$timestamp,df$price)
colnames(df) = c("Date","GE Price")
df$Date = as.numeric(df$Date)
df$`GE Price` = as.numeric(df$`GE Price`)
df$Date = basetime + dseconds(df$Date/1000)
df$Date = as.Date(df$Date)

#EXPORTING DATA
current = Sys.Date()
exit = paste("ItemID ",itemID,", ",current,", ",days,"D.csv",sep="")
write.csv(df,exit, row.names = FALSE)

