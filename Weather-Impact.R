# ------------------------------------------------------------------------
# Link between Business & Weather
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://github.com/BvHest/KNMIr/blob/master/vignettes/HowToUseKNMIr.Rmd
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
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","scales","plyr","lubridate","reshape2","TTR","forecast","tidyr","CausalImpact","dplyr","formatR","corrplot","Hmisc","PerformanceAnalytics")
devtools::install_github("BvHest/KNMIr", force=TRUE)
library(KNMIr)

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

weatherData <- weatherData[,-c(1:2)]
weatherData$date <- as.Date(weatherData$date)
gaData$date <- as.Date(gaData$date)

dataMerged <- merge(weatherData,gaData,by="date")
dataMerged <- dataMerged[,-1]
dataMerged <- dataMerged[,-c(9,11)]

for (i in colnames(dataMerged)){
  colname <- colnames(dataMerged[i])
  #if(!is.numeric(colname))
    dataMerged[[colname]] <- as.numeric(dataMerged[[colname]])
}

# ------------------------------------------------------------------------
# Filter merged data
# ------------------------------------------------------------------------



# ------------------------------------------------------------------------
# Correlation
# ------------------------------------------------------------------------

corMerged <- cor(dataMerged)
corrplot(corMerged,method="number",col=col,type="upper", order="alphabet", title = "Link tussen features - 2018-01-01 tot 2018-12-01")

cor.test(dataMerged$straling,dataMerged$addToCart)
cor.test(dataMerged$straling,dataMerged$transactions)
cor.test(dataMerged$straling,dataMerged$transactionRevenue)

#pairs(corMerged)
#cor.test(weatherData$percZon,weatherData$gemWind)
#chart.Correlation(dataMerged, histogram=TRUE, pch=19)

# ------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------

#split in train & test data
#simpel ????nlijning model bouwen
#test model accuraatheid
#voorspel waarden op basis van model


# ------------------------------------------------------------------------
# Forecasting scenario's - extrapolation (per dag dat het straalt ...)
# x dagen in januari
# Forecasting timeseries -> welk weer kan je verwachten?
# ------------------------------------------------------------------------


#Prediction is the more general statement. A prediction (predicted value) is an expectation for a combination of predictors (predictor values). The prediction is calculated based on a model, and the model is usually obtained by a fit to some available data.
#The most simple example is the mean of a sample. The model is most simple - there is no particular predictor involved (you only have some values for some response variable but you do not have any information on what these values may depend). The mean is the model that maximizes the likilihood (of the data under the model). It gives the expected value for the response. This means: if you had to make a guess abut any new value that is obtained in a similar way how the sample was obtained, the sample mean is your best guess.
#If you know that the data depends on the membership to a group (e.g. "controls" and "treated"), then you have a (categorical) predictor (the group) and a model using this predictor gives you an expected value for each group (group means). Thus, based on a given group membership you can calculate what value to expect.
#The predictor may be continuous as well (like "time"). In this case the model gives you an expected value for any possible value of this predictor. Here it is possible to predict value at time points for which no data exists. When such a prediction is done for times within the range of observed time points it is called an interpolation. When the prediction is done outside of the range of observed time points it is called an extrapolation. So far this all applies to any kind of continuous predictor (like concentration, mass, distance, speed, ...). The "time" is somewhat special because it has a natural "direction" (from past to future), and time-series analyses are usually performed to make predictions of future observations - this is called a forecast.


