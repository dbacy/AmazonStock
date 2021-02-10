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

