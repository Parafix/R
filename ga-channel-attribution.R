# ------------------------------------------------------------------------
# Channel attribution
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References: 
# https://stuifbergen.com/2016/10/multi-channel-funnel-conversion-rate/
# https://stuifbergen.com/2016/11/conversion-attribution-markov-model-r/
# https://www.bounteous.com/insights/2016/06/30/marketing-channel-attribution-markov-models-r/?ns=l
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
setwd(directory)

# ------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------
install.packages("install.load")
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR","ChannelAttribution")
install_load("dplyr","plyr","lubridate","reshape2","tidyr","tidyverse","dlookr")
install_load("ggplot2","ggthemes","ggrepel","scales")
#install_load("TTR","forecast","CausalImpact","formatR","corrplot","Hmisc")

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

colname_to_numeric <- function(df) {
    df <- as.numeric(df)
}

# ------------------------------------------------------------------------
# Import Google Analytics data
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)
#meta <- google_analytics_meta()

gaViewId <- "107861182"
gaDateRange <- c("2018-01-01","2018-12-23")
gaDimensions <- c("basicChannelGroupingPath") #sourceMediumPath
gaMetrics <- c("conversionGoal002","conversionGoal001","totalConversions")
#gaDelta <- order_type("date","ASCENDING", "DELTA")
#gaDimFilterUsertype <- dim_filter("userType","REGEXP","returning")
#gaDimFilters <- filter_clause_ga4(list(gaDimFilterUsertype))mcf:conversionGoalNumber==012

trans_filter <- "mcf:conversionType==Transaction"
visit_filter <- "mcf:conversionGoalNumber==002"

#get data
#gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, type = gaType, anti_sample = TRUE)

get_data <- function(vid, from, to, filters, dim) {
  df <- google_analytics_3(id = vid, 
                           start = from, end = to, 
                           metrics = c("totalConversions"), #,"totalConversionValue"
                           dimensions = dim,
                           filters = filters,
                           type="mcf",
                           max_results = 5000)
  # clean up and set class
  df[,1] <- gsub(" / ", "/", df[,1])              # remove spacing
  df[,1] <- gsub(":?(NA|CLICK|NA):?", "", df[,1]) # remove CLICK and NA
  df[,2] <- as.numeric(df[,2])                    # conversion column is character :-/
  
  # return the dataframe
  df
}

transactions <- get_data(vid=gaViewId, from="2018-01-22", to="2018-12-23", dim=gaDimensions, filters=trans_filter)
colnames(transactions) <- c("path", "transactions", "revenue")
visits <- get_data(vid=gaViewId, from="2018-01-22", to="2018-12-23", dim=gaDimensions, filters=visit_filter)
colnames(visits) <- c("path", "visits","revenue")

# ------------------------------------------------------------------------
# Merge & handle data
# ------------------------------------------------------------------------

gaDataMerged <- merge.data.frame(visits, transactions, by = "path", all=T)

gaDataMerged[is.na(gaDataMerged$transactions), "transactions"] <- 0
gaDataMerged[is.na(gaDataMerged$visits), "visits"] <- 0

gaDataMerged$transactions <- colname_to_numeric(gaDataMerged$transactions)
gaDataMerged$visits <- colname_to_numeric(gaDataMerged$visits)

gaDataMerged$tsr <- gaDataMerged$transactions/gaDataMerged$visits
gaDataMerged$null <- gaDataMerged$visits - gaDataMerged$transactions

gaDataMerged[is.infinite(gaDataMerged$tsr), "tsr"] <- 0

# ------------------------------------------------------------------------
# Build Markov model
# ------------------------------------------------------------------------
mm <- markov_model(gaDataMerged, var_path = "path", var_conv = "transactions", 
                   #var_value = "",
                   var_null = "null",
                   order=2, 
                   nsim=NULL, 
                   max_step=NULL, 
                   out_more=FALSE)

hm <- heuristic_models(gaDataMerged, var_path = "path",
                       #var_value = "value",
                       var_conv = "transactions")

gaDataModeled <- merge.data.frame(hm, mm, all=T, by="channel_name")

#View(gaDataMerged)
View(gaDataModeled)
