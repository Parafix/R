# ------------------------------------------------------------------------
# Trend & Seasonality
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://www.rdocumentation.org/packages/googleAnalyticsR/versions/0.4.2/topics/filter_clause_ga4
# https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
#set writing directory
setwd('/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/')
#rm(list=ls())
#dev.off()

# ------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------
install.packages("install.load")
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","scales","plyr","lubridate","reshape2","TTR","forecast","tidyr","CausalImpact","dplyr","formatR")

# ------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)

#meta <- google_analytics_meta()

gaViewId <- "97928845"
gaDateRange <- c("2016-10-31","2018-10-31")
gaDimensions <- c("date","deviceCategory","channelGrouping")
gaMetrics <- c("sessions")
gaDelta <- order_type("date","ASCENDING", "DELTA")

#get data
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, anti_sample = TRUE)

#gaData <- gaData %>% select(date, medium, sessions) %>% spread(medium, sessions)
#colnames(gaData)
#gaData <- gaData %>% select(1,25)
#gaData <- gaData %>% select(25,2,9,22,26)

gaDataSelected <- gaData %>% 
  filter(deviceCategory == "desktop") %>% 
  select(date, channelGrouping, sessions) %>%
  spread(channelGrouping, sessions)

#rename columns
names(gaDataSelected)[2]<-"Other"
names(gaDataSelected)[6]<-"Organic"
names(gaDataSelected)[8]<-"Paid"

# - GENERAL STATISTICS -------------------------------------------------------

gaDataSelected$date <- as.Date(gaDataSelected$date)
gaDataSelected[is.na(gaDataSelected)] <- 0

pairs(gaDataSelected)

library(ggplot2)
gg <- ggplot(data = gaDataSelected) + 
  theme_minimal() + 
  ggtitle("Paid (blue) vs Direct (orange) search")
gg <- gg + 
  geom_line(aes(x = as.Date(date), y = `Paid`), col = "blue")
gg + geom_line(aes(x = as.Date(date), y = `Direct`), col = "orange")

ccf(gaDataSelected$Paid, gaDataSelected$Direct)

#gaDataSelected <- acast(gaData, year ~ month)
#frequency : 1=year, 4=quarter, 12=month, 
#gaDataSelectedTS <- ts(gaData$sessions,frequency=12)
#gaDataSelectedTS <- ts(gaData$sessions, start=c(2015,1), end=c(2018,9), frequency=12)
#gaDataSelectedTS <- ts(gaData$newusers, start=c(2015,1), end=c(2018,9), frequency=12)
#gaDataSelectedTS <- ts(gaData[-1], start=c(2016,1), end=c(2018,9), frequency=12)
#gaDataSelectedTS <- ts(gaData$goalCompletionsAll, start=c(2008,1), end=c(2017,12), frequency=365)
gaDataSelectedTS <- ts(gaDataSelected[-1], frequency = 365)

#raw view of trend
plot(gaDataSelectedTS, type="b")
plot(log(gaDataSelectedTS)) #if fluctuations are increasing per time index

#smoothened view of trend to see if it is cyclical or seasonal 
#based on 6 months into the timeseries from the start
#via simple moving average
#gaDataSelectedTSSMA = SMA(gaDataSelectedTS,n=3) 
#plot(gaDataSelectedTSSMA,type="b")

#smoothened view of trend to see if it is cyclical or seasonal 
#via exponential smoothening - see more detail if SMA is better or not? 
#gaDataSelectedTSEMA = EMA(gaDataSelectedTS,3,ratio=0.25)
#plot(gaDataSelectedTSEMA,type="b")

#decomposed view of trend
decompose(gaDataSelectedTS[, "Paid"])
plot(decompose(gaDataSelectedTS[, "Paid"]),type="b")

#decompose(gaDataSelectedTS)
#plot(decompose(gaDataSelectedTS),type="b") 

#fit <- ets(gaDataSelectedTS[, "cpc"])
#fc <- forecast(fit,h=12)
#plot(fc,type="b")

#subtract seasonal impact from time series
#gaDataSelectedDecomposedComponents <- decompose(gaDataSelectedTS)
gaDataSelectedDecomposedComponents <- decompose(gaDataSelectedTS[, "Paid"])
#plot(gaDataSelectedDecomposedComponents$seasonal)
#gaDataSelectedTSAdjusted <- gaDataSelectedTS - gaDataSelectedDecomposedComponents$seasonal #- gaDataSelectedDecomposedComponents$random
gaDataSelectedTSAdjusted <- gaDataSelectedTS[, "Paid"] - gaDataSelectedDecomposedComponents$seasonal
plot(gaDataSelectedTSAdjusted,type="b")

#check what forecasting technique should be picked
#gaDataSelectedForecast <- HoltWinters(gaDataSelectedTS,b=F,gamma=F)
#gaDataSelectedForecast <- HoltWinters(gaDataSelectedTS,gamma=F)
#adjust start value for forecasting -> l.start = first value of fitted values & b = slope, based on lagging forecasted trendline (approx guess)
#alpha high: estimate of the level is based on recent observations
#beta low: slope of the trend component is not updated over the time series
#gamma high: seasonal values taken from recent observations
#gaDataSelectedForecast <- HoltWinters(gaDataSelectedTS, l.start=8.00, b.start = 9)
gaDataSelectedForecast <- HoltWinters(gaDataSelectedTS[, "Direct"])
#gaDataSelectedForecast <- HoltWinters(gaDataSelectedTS)
plot(gaDataSelectedForecast)
#forecasted values
gaDataSelectedForecast$fitted
#check error rate
gaDataSelectedForecast$SSE
#check final forecast in next x data points (days, months, years, ...)
gaDataSelectedForecastFinal <- forecast(gaDataSelectedForecast, h=90)
plot(gaDataSelectedForecastFinal,type="b")

#check if the predictive model can be improved by checking the errors
acf(gaDataSelectedForecastFinal$residuals, lag.max=12, na.action = na.pass)
Box.test(gaDataSelectedForecastFinal$residuals, lag=12, type="Ljung-Box")
plot(gaDataSelectedForecastFinal$residuals,type="b")
hist(gaDataSelectedForecastFinal$residuals)

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(na.omit(gaDataSelectedForecastFinal$residuals))

#extra
#gaDataPerYear <- aggregate(gaData$sessions, by=list(gaData$year), FUN=sum)
#gaDataPerMonth <- aggregate(gaData$sessions, by=list(gaData$month), FUN=sum)
#gaDataPerWeek <- aggregate(gaData$sessions, by=list(gaData$week), FUN=sum)
#gaDataPerDay <- aggregate(gaData$sessions, by=list(gaData$day), FUN=sum)
#plot.ts(gaDataPerMonth)
#plot.ts(gaDataPerWeek)

library(xts)
gaDataSelectedXTS <- xts(gaDataSelected[-1], order.by = as.Date(gaDataSelected$date), frequency = 7)

library(CausalImpact)
pre.period <- as.Date(c("2018-02-01","2018-05-31"))
post.period <- as.Date(c("2018-06-01","2018-09-30"))

## data in order of response, predictor1, predictor2, etc.
ciModel <- gaDataSelectedXTS[, c("Direct","Paid","Organic")]

impact <- CausalImpact(ciModel, pre.period, post.period)
plot(impact)
summary(impact,"report")