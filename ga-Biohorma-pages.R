# ------------------------------------------------------------------------
# Biohorma GA export page details + page path analysis
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
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","tidyverse","scales","plyr","lubridate","reshape2","TTR","forecast","tidyr","CausalImpact","dplyr","formatR","corrplot","Hmisc","openxlsx")


colnames_to_numeric <- function(df) {
  for (i in colnames(df)){
    colname <- colnames(df[i])
    #if(!is.numeric(colname))
    df[[colname]] <- as.numeric(df[[colname]])
  }
}

colnames_to_character <- function(df) {
  for (i in colnames(df)){
    colname <- colnames(df[i])
    #if(!is.numeric(colname))
    df[[colname]] <- as.character(df[[colname]])
  }
}


# ------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)

meta <- google_analytics_meta()

gaViewId <- "2897692"
gaDateRange <- c("2018-01-01","2018-12-31")
gaDimensions <- c("year","pagePath") #,"deviceCategory", sourceMedium
#gaDimensions <- c("date","month","year")
gaMetrics <- c("pageviews","entrances", "exitRate","exits","bounces") #exitPagePath
#gaMetrics <- c("sessions","newUsers","goal2Completions", "transactions")
gaDelta <- order_type("date","ASCENDING", "DELTA")
gaDimFilterSourceMedium <- dim_filter("sourceMedium","REGEXP","organic|direct")
gaDimFilters <- filter_clause_ga4(list(gaDimFilterSourceMedium))

#get data
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)

#gaData$source = "GA"

#inlezen screaming frog data
dfSFPages = read.csv("sf-avogel-seodirect-page-clusters.csv", sep=";", header = TRUE)
#naam kolom aanpassen
names(dfSFPages)[1]<-"pagePath"

#factors naar char
dfSFPages <- data.frame(lapply(dfSFPages, as.character), stringsAsFactors=FALSE)

dfSFPages$pagePath <- tolower(dfSFPages$pagePath)
gaData$pagePath <- tolower(gaData$pagePath)

dfSFPages$pagePath <- unlist(sapply(strsplit(dfSFPages$pagePath, "\\www.avogel.be"), tail, 1),use.names=FALSE)

#join alles op pagePath
dfMerged <- merge(x = gaData, y = dfSFPages, by = "pagePath", all = TRUE)


#colnames(gaData)
#gaData <- gaData %>% select(1,25)
#gaData <- gaData %>% select(25,2,9,22,26)
#names(gaData)[5]<-"addToCart"

#gaData <- gaData %>% select(date, channelGrouping, transactions) %>% spread(channelGrouping, transactions)
#gaData <- gaData %>% select(date, channelGrouping, sessions) %>% spread(channelGrouping, sessions)
#gaData <- gaData %>% group_by(channelGrouping) %>% summarise_all(sum)???

#gaData$date <- as.Date(gaData$date)
#gaData[is.na(gaData)] <- 0

#gaDataCor <- gaData[,-1]
#pairs(gaDataCor)