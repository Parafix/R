# ------------------------------------------------------------------------
# Impact of the weather on online sales
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
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","scales","plyr","lubridate","reshape2","TTR","forecast","tidyr","CausalImpact","dplyr","formatR","corrplot")

# ------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)

#meta <- google_analytics_meta()

gaViewId <- "97928845"
gaDateRange <- c("2018-01-01","2018-10-31")
#gaDimensions <- c("date","deviceCategory","channelGrouping")
#gaDimensions <- c("date","deviceCategory","dayOfWeek")
gaDimensions <- c("date","dayOfWeek")
gaMetrics <- c("sessions","newUsers","transactions","transactionRevenue")
gaDelta <- order_type("date","ASCENDING", "DELTA")


#get data
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, anti_sample = TRUE)

#gaData <- gaData %>% select(date, medium, sessions) %>% spread(medium, sessions)
#colnames(gaData)
#gaData <- gaData %>% select(1,25)
#gaData <- gaData %>% select(25,2,9,22,26)

# gaDataSelectedNewUsers <- gaData %>% 
#   filter(deviceCategory == "desktop") %>% 
#   select(date, channelGrouping, newUsers) %>%
#   spread(channelGrouping, newUsers)

# gaDataSelectedRevenue <- gaData %>% 
#   select(date, deviceCategory, transactionRevenue, dayOfWeek) %>%
#   spread(deviceCategory, transactionRevenue)

gaDataSelectedRevenue <- gaData %>% 
  select(date, dayOfWeek, transactionRevenue) %>%
  spread(dayOfWeek, transactionRevenue)

gaDataSelectedRevenue[is.na(gaDataSelectedRevenue)] <- 0
gaDataSelectedRevenue$total <- gaDataSelectedRevenue$`0` + gaDataSelectedRevenue$`1` + gaDataSelectedRevenue$`2` + gaDataSelectedRevenue$`3` + gaDataSelectedRevenue$`4` + gaDataSelectedRevenue$`5` + gaDataSelectedRevenue$`6`
#gaDataSelectedRevenue$total <- gaDataSelectedRevenue$desktop + gaDataSelectedRevenue$tablet + gaDataSelectedRevenue$mobile

#rename columns
#names(gaDataSelected)[2]<-"Other"

gaDataSelectedRevenue$date <- as.Date(gaDataSelected$date)
#gaDataSelectedRevenue$dayOfWeek <- as.numeric(gaDataSelectedRevenue$dayOfWeek)


#define extra features
#write loop to shit to these items
feestdagen <- data.frame(date = c("2018-01-01","2018-04-02","2018-05-01","2018-05-10","2018-05-21","2018-07-21","2018-08-05","2018-11-01"), feestdag = c("1","1","1","1","1","1","1","1"))
feestdagen$date <- as.Date(feestdagen$date)
feestdagen$feestdag <- as.numeric(feestdagen$feestdag)

# waarde van die dag kopieren of niet? 

#sluitingsdagen <- data.frame(dayOfWeek = c("1","2"), sluitdag = c("1","1"))
#sluitingsdagen$dayOfWeek <- as.numeric(sluitingsdagen$dayOfWeek)
#sluitingsdagen$sluitdag <- as.numeric(sluitingsdagen$sluitdag)
brugdagen <- data.frame(date = c(""), brugdag = c(""))
schoolvakanties <- data.frame(date = c(""), schoolvakantie = c(""))
actiedagen <- data.frame(date = c(""), actiedag = c(""))


#merge extra features with existing dataframe
gaDataSelectedRevenue <- merge(gaDataSelectedRevenue, feestdagen, by = "date", incomparables = NA, all.x = TRUE)
gaDataSelectedRevenue$feestdag[is.na(gaDataSelectedRevenue$feestdag)] <- 0

#gaDataSelectedRevenue <- merge(gaDataSelectedRevenue, sluitingsdagen, by = "dayOfWeek", incomparables = NA, all.x = TRUE)
#gaDataSelectedRevenue$sluitdag[is.na(gaDataSelectedRevenue$sluitdag)] <- 0

# - GENERAL STATISTICS -------------------------------------------------------

gaDataSelectedRevenueCor <- gaDataSelectedRevenue[,-1]

#normalize or not? DO THE TEST
pairs(gaDataSelectedRevenueCor)
cor(gaDataSelectedRevenueCor)

col<- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(cor(gaDataSelectedRevenueCor),method="number",col=col,type="upper", order="alphabet")


library(ggplot2)
# gg <- ggplot(data = gaDataSelectedRevenue) + 
#   theme_minimal() + 
#   ggtitle("Desktop (blue) vs Mobile (orange) search vs Tablet (green)")
# gg <- gg + geom_line(aes(x = as.Date(date), y = `desktop`), col = "blue")
#   gg + geom_line(aes(x = as.Date(date), y = `mobile`), col = "orange")
#   gg + geom_line(aes(x = as.Date(date), y = `tablet`), col = "green")

#check cross lag between devices
#ccf(gaDataSelectedRevenue$mobile, gaDataSelectedRevenue$tablet)

gg <- ggplot(data = gaDataSelectedRevenue) + 
   theme_minimal() + 
   ggtitle("Totaal (blue) vs Vrijdag (orange)")
  gg <- gg + geom_line(aes(x = as.Date(date), y = `total`), col = "blue")
  gg + geom_line(aes(x = as.Date(date), y = `5`), col = "orange")
  

  
library(xts)
gaDataSelectedRevenueXTS <- xts(gaDataSelectedRevenue[-1], order.by = as.Date(gaDataSelectedRevenue$date), frequency = 7)

library(CausalImpact)
pre.period <- as.Date(c("2018-02-01","2018-05-31"))
post.period <- as.Date(c("2018-06-01","2018-09-30"))

## data in order of response, predictor1, predictor2, etc.
ciModel <- gaDataSelectedXTS[, c("total","feestdag","0")]

impact <- CausalImpact(ciModel, pre.period, post.period)
plot(impact)
summary(impact,"report")