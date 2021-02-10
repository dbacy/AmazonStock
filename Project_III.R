install.packages("quantmod")
library(quantmod)
library(forecast)

#fetching data from amazon 
amazon = getSymbols.yahoo("AMZN", auto.assign = F,
                    from = "2013-01-01" ,
                    to = "2017-01-01")
#View(amazon) - use in console

plot(as.ts(amazon$AMZN.Open))

#candlestick chart
chartSeries(amazon,type = "candlesticks")

#acf & pacf charts
ggtsdisplay(amazon$AMZN.Open)

#arima model
amazonarima = auto.arima(amazon$AMZN.Open,
                         stepwise = T,
                         approximation = F,
                         trace = T); amazonarima

#Arima Drift
amazonarima2 = Arima(amazon$AMZN.Open,
                     order = c(0,1,1))
amazonarima2

plot(forecast(amazonarima, h = 20))
plot(forecast(amazonarima2, h = 20))

#ets model
amazonets = ets(amazon$AMZN.Open)
#plot ets
plot(forecast(amazonets, h = 20))

##########################################################

#Getting a regular time series
#Conversion to dataframe
amazon = as.data.frame(amazon)

#Adding a rowname as date
amazon$Date = rownames(amazon)
amazon$Date = as.Date(amazon$Date)
head(amazon)

#Create date column
mydates = seq.Date(from = as.Date(
  "2015-01-01"),
  to = as.Date("2016-01-01"),
  by = 1)

#convert to a df (required to merge)
mydates = data.frame(Date = mydates)
#padding with mydates
#View(mydata) - use in console 
mydata = merge(amazon, mydates, by = 
                 "Date", all.y = T)

#removing initial days to start on monday
mydata = mydata[5:366,]
#removing sundays
mydata = mydata[-(seq(from = 7, to = nrow(
  mydata), by = 7)),]
#removing saturdays
mydata = mydata[-(seq(from = 6, to = nrow(
  mydata), by = 6)),]

#removing holidays and other NA
mydata = na.locf(mydata)

#putting the close price into a weekly time series
highestprice = ts(as.numeric(
  mydata$AMZN.High), frequency = 5)

#plots for high
seasonplot(highestprice, season.labels = 
             c("Mon","Tue", "Wed", "Thu", "Fri"))
monthplot(highestprice)
monthplot(highestprice, base = median, col.base = "red")
#stl decompse
plot(stl(highestprice, s.window = "periodic"))

#plots for low
par(mfrow = c(1,2)) #gives me two plots
lowestprice = ts(as.numeric(mydata$AMZN.Low),
                 frequency = 5)
monthplot(lowestprice, base = median, col.base = "red")
monthplot(highestprice, base = median, col.base = "red")
par(mfrow = c(1,1)) #brings me back to one plot

