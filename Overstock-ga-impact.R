# ------------------------------------------------------------------------
# SCENARIO 002
# Change impact on ecommerce conversion rate by period (date)
# Can be used with filters: source, medium, campaign, etc.
#
# R Statistics & Data Science for OM
# Language: English
# Code by Dries Bultynck
# Questions & bugs or requests: Dries.bultynck@wijs.be
# ------------------------------------------------------------------------

# - SETTINGS -------------------------------------------------------
#empty & clean global environment
#rm(list=ls())
#set writing directory
setwd('/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/')

# - PACKAGES -------------------------------------------------------

install.packages("install.load")
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","scales","plyr","lubridate","reshape2","TTR","forecast","tidyr","CausalImpact","dplyr","formatR","ggplot2")


# - IMPORT DATA -------------------------------------------------------

#as.Date("16-07-2018","%d-%m-%Y") - as.Date("17-04-2018","%d-%m-%Y")

#ga_auth("auth/test.oauth")
ga_auth(new_user = TRUE)

#account_list <- ga_account_list()
#account_list$viewId

#meta <- google_analytics_meta()

#https://www.rdocumentation.org/packages/googleAnalyticsR/versions/0.4.2/topics/filter_clause_ga4
gaViewId <- "2897692"
gaDateRange <- c("2019-01-05","2019-05-05")
gaDimensions <- c("date","week","channelGrouping")
gaMetrics <- c("sessions","transactions","transactionRevenue","bounceRate")
gaDelta <- order_type("date","ASCENDING", "DELTA")
#gaDimFilterCountry <- dim_filter("medium","REGEXP","cpc")
#gaMetFilterPageviews <- met_filter("pageviews","EQUAL","10")
#gaMetFilters <- filter_clause_ga4(list(gaMetFilterPageviews))
#gaDimFilters <- filter_clause_ga4(list(gaDimFilterCountry))
#gaFiltersExpressions <- "ga:medium!=cpc"

#define custom segment
#googleAnalyticsR:::gadget_GASegment()

#get data
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, anti_sample = TRUE)
#gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)
#gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, order = gaDelta, filtersExpression = gaFiltersExpressions, anti_sample = TRUE)
#gaData <- google_analytics_4(gaViewId, date_range = gaDateRange, metrics = gaMetrics, met_filters = gaMetFilters, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)
#gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)


# - GENERAL STATISTICS -------------------------------------------------------
head(gaData)
summary(gaData)
tail(gaData)

#gaData$Datum = ymd(gaData$date)
gaData$Wkd = as.factor(weekdays(gaData$date))

gaData <- na.omit(gaData)

newsite <- function(date){
  if(date >= "2019-04-15")
    return("Google Ads - New")
  if(date <= "2019-04-14")
    return("Google Ads - Old")
  else
    return(NA)
}

gaData$Switch <- as.factor(sapply(gaData$date, newsite))

ggplot(gaData, aes(gaData$date, gaData$sessions,colour = gaData$Switch)) + geom_point() + stat_smooth()
ggplot(gaData, aes(gaData$date, gaData$transactions,colour = gaData$Switch)) + geom_point() + stat_smooth()
ggplot(gaData, aes(gaData$date, gaData$transactionRevenue,colour = gaData$Switch)) + geom_point() + stat_smooth()
ggplot(gaData, aes(gaData$date, gaData$bounceRate,colour = gaData$Switch)) + geom_point() + stat_smooth()

ggplot(gaData, aes(gaData$Switch, gaData$sessions, fill = gaData$Switch)) + geom_boxplot()
ggplot(gaData, aes(gaData$Switch, gaData$transactions, fill = gaData$Switch)) + geom_boxplot()
ggplot(gaData, aes(gaData$Switch, gaData$transactionRevenue, fill = gaData$Switch)) + geom_boxplot()
ggplot(gaData, aes(gaData$Switch, gaData$bounceRate, fill = gaData$Switch)) + geom_boxplot()

# ggplot(gaData, aes(gaData$date, gaData$transactions,colour = gaData$Switch)) +
#   geom_point() +
#   stat_smooth() +
#   ggtitle("Seasonality & Transactions \nFeb-Jul 2018") +
#   xlab("Datum") +
#   ylab("Transactions") +
#   scale_y_continuous(labels = numeric, limits=c(0,1))

t.test(gaData$sessions~gaData$Switch, gaData, var.equal=TRUE)
t.test(gaData$transactions~gaData$Switch, gaData, var.equal=TRUE)
t.test(gaData$transactionRevenue~gaData$Switch, gaData, var.equal=TRUE)
t.test(gaData$bounceRate~gaData$Switch, gaData, var.equal=TRUE)

