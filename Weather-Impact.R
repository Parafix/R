# ------------------------------------------------------------------------
# Link between Business & Weather
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://github.com/BvHest/KNMIr/blob/master/vignettes/HowToUseKNMIr.Rmd
# Alternative:
# https://github.com/ropensci/nasapower
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
#set writing directory
setwd('/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/')
col<- colorRampPalette(c("red", "white", "blue"))(20)
#rm(list=ls())
#dev.off()

# ------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------
install.packages("install.load")
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","scales","plyr","lubridate","reshape2","TTR","forecast","tidyr","CausalImpact","dplyr","formatR","corrplot","Hmisc","PerformanceAnalytics", "caret")
devtools::install_github("BvHest/KNMIr", force=TRUE)
library(KNMIr)

# ------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------

colnames_to_numeric <- function(df) {
  for (i in colnames(df)){
    colname <- colnames(df[i])
    #if(!is.numeric(colname))
    df[[colname]] <- as.numeric(df[[colname]])
  }
}

# ------------------------------------------------------------------------
# Import Weather data
# ------------------------------------------------------------------------

#list_stations()
#plot(plot_stations)
#240 -> Schiphol
#319 -> Westdorpe (dicht bij Gent)

#ytd weer data
#weatherData <- get_climate_data_api(319)
#rename_columns_KNMI_data(weatherData)
#weatherData[is.na(weatherData)] <- 0

#ggplot(weatherData, aes(x = weatherData$YYYYMMDD, y = weatherData$gemTemp)) + geom_line()

weatherData <- get_climate_data_api(stationID = 319, from="20180101", to="20181215") %>% subset_KNMI_data(startyear = 2018) %>% rename_columns_KNMI_data()
weatherData[is.na(weatherData)] <- 0

# ------------------------------------------------------------------------
# Import Google Analytics data
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)
#meta <- google_analytics_meta()

gaViewId <- "97928845"
gaDateRange <- c("2018-01-01","2018-12-15")
gaDimensions <- c("date","month") #,"channelGrouping", sourceMedium, year
gaMetrics <- c("sessions","newUsers","goal2Completions", "transactions", "transactionRevenue")
gaDelta <- order_type("date","ASCENDING", "DELTA")
#gaDimFilterUsertype <- dim_filter("userType","REGEXP","returning")
#gaDimFilters <- filter_clause_ga4(list(gaDimFilterUsertype))

#get data
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, anti_sample = TRUE)

names(gaData)[5]<-"addToCart"

# ------------------------------------------------------------------------
# Merge weather & google analytics data
# ------------------------------------------------------------------------

weatherData <- weatherData[,-c(1,2,11)]
weatherData$date <- as.Date(weatherData$date)
gaData$date <- as.Date(gaData$date)

dataMerged <- merge(weatherData,gaData,by="date")
dataMerged <- dataMerged[,-c(10,11)]

# ------------------------------------------------------------------------
# Filter merged data
# ------------------------------------------------------------------------

data201808 <- dataMerged %>% filter(date > "2018-07-31", date < "2018-09-01")
data201804t06 <- dataMerged %>% filter(date > "2018-03-31", date < "2018-07-01")

# ------------------------------------------------------------------------
# Correlation total merged data
# ------------------------------------------------------------------------

#delete date
dataMerged <- dataMerged[,-1]

#make everything numeric
dataMerged <- colnames_to_numeric(dataMerged)

#correlation total period
corMerged <- cor(dataMerged)
corrplot(corMerged,method="number",col=col,type="upper", order="alphabet", title = "")

#visualize highlights in correlations to see impact
dataMergedTransactionsPerWeek <- dataMerged %>% group_by(week) %>% summarise(transactions = sum(transactions), transactionRevenue = sum(transactionRevenue), addToCart=sum(addToCart))
gg <- ggplot(dataMergedTransactionsPerWeek, aes(dataMergedTransactionsPerWeek$week, dataMergedTransactionsPerWeek$transactions,colour = dataMergedTransactionsPerWeek$transactionRevenue)) + geom_line(size=(dataMergedTransactionsPerWeek$transactionRevenue/50000))
gg + labs(x = "Week", y ="Transactions", title="Trend of transactions per week", subtitle="2018", caption = "(based on Google Analytics data)", color="transactionRevenue per ???50.000")

#cor.test(dataMerged$straling,dataMerged$addToCart)
#cor.test(dataMerged$straling,dataMerged$transactions)
#cor.test(dataMerged$straling,dataMerged$transactionRevenue)

#pairs(corMerged)
#cor.test(weatherData$percZon,weatherData$gemWind)
#chart.Correlation(dataMerged, histogram=TRUE, pch=19)

fit1 <- lm(addToCart ~ newUsers, dataMerged)
summary(fit1)

RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(fit1$residuals)

mae <- function(error) { mean(abs(error)) }
mae(fit1$residuals)

set.seed(23)
newNewUsers = data.frame(newUsers = c(927))
round(predict(fit1, newNewUsers , interval = 'prediction'))
round(predict(fit1, newNewUsers , interval = 'confidence'))

plot(dataMerged$addToCart,col="blue",type="l")
lines(fit1$fitted.values,col="red")

#opnieuw proberen met outliers eruit te halen!!!!!


# ------------------------------------------------------------------------
# Correlation August 2018
# ------------------------------------------------------------------------
#delete date & month value as timeframe is only one month
data201808 <- data201808[,-c(1,10)]

#make everything numeric
data201808 <- colnames_to_numeric(data201808)

corData201808 <- cor(data201808)
corrplot(corData201808,method="number",col=col,type="upper", order="alphabet", title = "")

#visualize highlights in correlations to see impact
data201808TransactionsPerWeek <- data201808 %>% group_by(week) %>% summarise(transactions = sum(transactions), transactionRevenue = sum(transactionRevenue), addToCart=sum(addToCart))
gg <- ggplot(data201808TransactionsPerWeek, aes(data201808TransactionsPerWeek$week, data201808TransactionsPerWeek$transactions,colour = data201808TransactionsPerWeek$transactionRevenue)) + geom_line(size=(data201808TransactionsPerWeek$transactionRevenue/50000))
gg + labs(x = "Week", y ="Transactions", title="Trend of transactions per week", subtitle="August 2018", caption = "(based on Google Analytics data)", color="transactionRevenue per ???50.000")
#plot(data201808TransactionsPerWeek$week,data201808TransactionsPerWeek$transactions, type="l", col="blue", lwd=1, pch=13, xlab="Week", ylab="Transactions", main="Decay transactions per week")
#lines(data201808TransactionsPerWeek$week,data201808TransactionsPerWeek$transactionRevenue,col="red")

#test significance of features -> if valuable to work with
cor.test(data201808$week,data201808$addToCart) #significant, middelmatige afwijking (22)
cor.test(data201808$maxTemp,data201808$addToCart) #significant, grote afwijking (36)
cor.test(data201808$gemTemp,data201808$addToCart) #significant, heel grote afwijking (40)
cor.test(data201808$straling,data201808$addToCart) #significant, heel grote afwijking (40)
cor.test(data201808$percZon,data201808$addToCart) #niet significant, heel grote afwijking (50)
cor.test(data201808$newUsers,data201808$addToCart) #significant, kleine afwijking (7)

# ------------------------------------------------------------------------
# Build model - August 2018
# ------------------------------------------------------------------------

#split in train & test data ?

#fit1 <- lm(data201808$addToCart ~ data201808$week, data201808)
#fit2 <- lm(data201808$addToCart ~ data201808$week + data201808$newUsers, data201808)
#fit3 <- lm(data201808$addToCart ~ data201808$week + data201808$maxTemp, data201808)
#fit3 <- lm(data201808$addToCart ~ data201808$week + data201808$maxTemp + data201808$gemTemp + data201808$straling + data201808$percZon, data201808)
#fit4 <- lm(data201808$addToCart ~ data201808$week + data201808$maxTemp + data201808$straling + data201808$percZon, data201808)
#fit5 <- lm(data201808$addToCart ~ data201808$week + data201808$maxTemp + data201808$percZon, data201808)
#fit6 <- lm(data201808$addToCart ~ data201808$week + data201808$maxTemp^2 + data201808$gemTemp^2 + data201808$straling^2 + data201808$percZon^2, data201808)

fit1 <- lm(addToCart ~ newUsers, data201808)
fit2 <- lm(addToCart ~ maxTemp, data201808)
fit3 <- lm(addToCart ~ maxTemp + gemTemp + straling + percZon, data201808)
fit4 <- lm(addToCart ~ maxTemp + straling + percZon, data201808)
fit5 <- lm(addToCart ~ maxTemp + percZon, data201808)
fit6 <- lm(addToCart ~ week, data201808)
fit7 <- lm(addToCart ~ ., data201808)

anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7)
summary(fit3)

RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(fit7$residuals)

mae <- function(error) { mean(abs(error)) }
mae(fit7$residuals)

#confint(fit2, level=0.95)
#coefficients(fit1)
#fitted(fit1)
#residuals(fit1)
#vcov(fit1)
#influence(fit1)
#plot(fit1)

# ------------------------------------------------------------------------
# Prediction - August 2018
# ------------------------------------------------------------------------

set.seed(3)
#unique_clicks.c = scale(fbDataAdsetsLINK_CLICKS$unique_clicks, center=TRUE, scale=TRUE)
#fit1$residuals
#fit1$fitted.values

newWeek = data.frame(week = c(31,32,33,34,35))
predict(fit6, newWeek , interval = 'prediction')
predict(fit6, newWeek , interval = 'confidence')
prediction <- round(predict(fit6, newWeek, interval = "confidence"))

data201808TransactionsPerWeek$addToCart
dfPrediction <- as.data.frame(prediction)
dfPrediction$week <- index(dfPrediction)
dfPrediction <- colnames_to_numeric(dfPrediction)
dfPrediction$currentAddToCart <- data201808TransactionsPerWeek$addToCart 

ggplot(data201808TransactionsPerWeek, aes(x=week, y=addToCart)) + geom_point(color="red", size = 3) + geom_smooth(method=lm, color="blue")

ggplot(dfPrediction, aes(x=week, y=prediction)) + geom_point(color="red", size = 3) + geom_smooth(method=lm, color="blue")

gg <- ggplot(dfPrediction) + 
  theme_minimal() + 
  ggtitle("Predicted (red) vs Current (blue)")
gg + geom_line(aes(x = week, y = prediction), col = "red") + geom_line(aes(x = week, y = currentAddToCart), col = "blue")


newNewUsers = data.frame(newUsers = c(3204,3023,888, 999, 0))
#prediction <- round(predict(fit1, newNewUsers, interval = "prediction"))
prediction <- round(predict(fit1, newNewUsers))
prediction

# ------------------------------------------------------------------------
# Correlation April - Mei - Juni 2018
# ------------------------------------------------------------------------

data201804t06 <- data201804t06[,-c(1,10)]

#make everything numeric
data201804t06 <- colnames_to_numeric(data201804t06)

corData201804t06 <- cor(data201804t06)
corrplot(corData201804t06,method="number",col=col,type="upper", order="alphabet", title = "")

data201804t06TransactionsPerWeek <- data201804t06 %>% group_by(week) %>% summarise(transactions = sum(transactions), transactionRevenue = sum(transactionRevenue), addToCart=sum(addToCart))
gg <- ggplot(data201804t06TransactionsPerWeek, aes(data201804t06TransactionsPerWeek$week, data201804t06TransactionsPerWeek$transactions,colour = data201804t06TransactionsPerWeek$transactionRevenue)) + geom_line(size=(data201804t06TransactionsPerWeek$transactionRevenue/50000))
gg + labs(x = "Week", y ="Transactions", title="Trend of transactions per week", subtitle="2018", caption = "(based on Google Analytics data)", color="transactionRevenue per ???50.000")

cor.test(data201804t06$newUsers,data201804t06$addToCart)
cor.test(data201804t06$transactions,data201804t06$addToCart)

fit1 <- lm(addToCart ~ newUsers, data201804t06)
summary(fit1)
fit2 <- lm(transactions ~ addToCart, data201804t06)
summary(fit2)

set.seed(3)
prediction <- round(predict(fit2, data.frame(addToCart = c(500, 3000, 5000))))

# ------------------------------------------------------------------------
# Forecasting scenario's - extrapolation (per dag dat het straalt ...)
# x dagen in januari
# Forecasting timeseries -> welk weer kan je verwachten?
# ------------------------------------------------------------------------

#Prediction is the more general statement. A prediction (predicted value) is an expectation for a combination of predictors (predictor values). The prediction is calculated based on a model, and the model is usually obtained by a fit to some available data.
#The most simple example is the mean of a sample. The model is most simple - there is no particular predictor involved (you only have some values for some response variable but you do not have any information on what these values may depend). The mean is the model that maximizes the likilihood (of the data under the model). It gives the expected value for the response. This means: if you had to make a guess abut any new value that is obtained in a similar way how the sample was obtained, the sample mean is your best guess.
#If you know that the data depends on the membership to a group (e.g. "controls" and "treated"), then you have a (categorical) predictor (the group) and a model using this predictor gives you an expected value for each group (group means). Thus, based on a given group membership you can calculate what value to expect.
#The predictor may be continuous as well (like "time"). In this case the model gives you an expected value for any possible value of this predictor. Here it is possible to predict value at time points for which no data exists. When such a prediction is done for times within the range of observed time points it is called an interpolation. When the prediction is done outside of the range of observed time points it is called an extrapolation. So far this all applies to any kind of continuous predictor (like concentration, mass, distance, speed, ...). The "time" is somewhat special because it has a natural "direction" (from past to future), and time-series analyses are usually performed to make predictions of future observations - this is called a forecast.


