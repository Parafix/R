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
# https://rpubs.com/melike/corrplot
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
#set writing directory
setwd('/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/')
#rm(list=ls())
#dev.off()

# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR")
install_load("ggplot2","ggthemes","scales")
install_load("plyr","lubridate","reshape2","tidyr","dplyr")
install_load("TTR","forecast","CausalImpact","formatR","corrplot","Hmisc")
install_load("openxlsx","readxl")

# ------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

# ------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)

#meta <- google_analytics_meta()

gaClient <- "DVV"
gaViewName <- "2015"
gaViewId <- "95326124" #DVV
gaDateRange <- c("2019-01-01","2019-05-12")
gaDimensions <- c("date","channelGrouping") #,"deviceCategory", channelGrouping
#gaDimensions <- c("date","sourceMedium") #,"deviceCategory", channelGrouping
#gaDimensions <- c("date","month","year")
#gaMetrics <- c("sessions","newUsers", "transactions","transactionRevenue")
gaMetrics <- c("sessions","newUsers","goal4Completions")
gaDelta <- order_type("date","ASCENDING", "DELTA")
#gaDimFilterUsertype <- dim_filter("userType","REGEXP","returning")
#gaDimFilters <- filter_clause_ga4(list(gaDimFilterUsertype))
#gaDimFilterSourceMedium <- dim_filter("sourceMedium","REGEXP","cpc|organic|direct|email|e-mail|newsletter")
#gaDimFilters <- filter_clause_ga4(list(gaDimFilterSourceMedium))

#get data
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, anti_sample = TRUE)
#gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)

colnames(gaData)
#gaData <- gaData %>% select(1,25)
#gaData <- gaData %>% select(25,2,9,22,26)
#names(gaData)[5]<-"addToCart"

#werkt niet
# for(i in colnames(gaData)){
#   count <- 1
#   colName <- i
#   if(c("email") %in% colName){
#     colName <- gsub("email", "E", colName)
#   }else if(c("organic") %in% colName){
#     colName <- gsub("organic", "O", colName)
#   }else if(c("cpc") %in% colName){
#     colName <- gsub("cpc", "P", colName)
#   }else if(c("direct") %in% colName){
#     colName <- gsub("direct", "D", colName)
#   }
#   count <- count + 1
#   names(gaData[count]) <- colName
# }

#gaData <- gaData %>% select(date, channelGrouping, transactions) %>% spread(channelGrouping, transactions)
gaData <- gaData %>% dplyr::select(date, channelGrouping, sessions) %>% spread(channelGrouping, sessions)
#gaData <- gaData %>% dplyr::select(date, sourceMedium, sessions) %>% spread(sourceMedium, sessions)
#gaData <- gaData %>% dplyr::select(date, campaign, sessions) %>% spread(campaign, sessions)
  
#gaData <- gaData %>% group_by(channelGrouping) %>% summarise_all(sum)???

gaData$date <- as.Date(gaData$date)
gaData[is.na(gaData)] <- 0

gaDataCor <- gaData[,-1]
#pairs(gaDataCor)
cor(gaDataCor)

plot.new()
dev.off()
col<- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(cor(gaData[,-1]),method="number",col=col,type="upper", order="alphabet")
#dev.copy(png, paste(gaClient,"-COR-SourceMedium-plot",".png", sep=""),width=1024,height=768,res=72)
#dev.off()

# corrplot(cor(gaDataCor), method="color", col=col,  
#          type="upper", order="hclust", 
#          addCoef.col = "black", # Add coefficient of correlation
#          # Combine with significance
#          p.mat = p.mat, sig.level = 0.01, insig = "blank", 
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE 
# )
# 
corGaData <- cor.mtest(gaDataCor,0.95)
corrplot(cor(gaDataCor), method="color", col=col,
         type="upper", order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         # Combine with significance
         p.mat = corGaData[[1]], sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

# dev.off()
# corrplot(cor(gaDataCor), method = "color", outline = T, addgrid.col = "darkgray", order="hclust", 
#          addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", 
#          tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", 
#          number.digits = 2, number.cex = 0.75, col = colorRampPalette(c("darkred","white","midnightblue"))(20))

#write.xlsx(cor(gaDataCor), paste(gaClient,"-",gaViewName,"-COR-Campaigns-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""))


cor.test(gaData$`Organic Search`,gaData$Direct)
cor.test(gaData$`Paid Search`,gaData$Direct)
cor.test(gaData$`Paid Search`,gaData$`Organic Search`)


#plot(ecdf(gaData$`Transactions NL+FR`),xlab="sample of transactions")
plot(gaData$date,cumsum(gaData$Display))
plot(gaData$date,cumsum(gaData$`Organic Search`))
plot(gaData$date,cumsum(gaData$Email))
plot(gaData$date,cumsum(gaData$`Paid Search`), xlab="Jaar", ylab="Totaal aantal sessies via Paid Search")
plot(gaData$date,cumsum(gaData$newUsers))
plot(gaData$date,cumsum(gaData$transactions),type="l")

ggplot(gaData, aes(x = gaData$date, y = sum(gaData$`Paid Search`) - cumsum(gaData$`Paid Search`))) + geom_line(aes(size=gaData$Direct,col=gaData$Direct))
ggplot(gaData, aes(x = gaData$date, y = sum(gaData$Direct) - cumsum(gaData$Direct))) + geom_line(aes(size=gaData$`Paid Search`,col=gaData$`Paid Search`))
ggplot(gaData, aes(x = gaData$date, y = sum(gaData$`Organic Search`) - cumsum(gaData$`Organic Search`))) + geom_line(aes(size=gaData$`Paid Search`,col=gaData$`Paid Search`))

#ggplot() + geom_line(aes(x=gaData$years,y=cumsum(gaData$`Transactions NL+FR`),colour='red'))
#+ geom_line(aes(x=gaData$years,y=cumsum(gaData$`Newsletter NL`),colour='blue')) 

#ggplot(gaData, aes(x = gaData$week, y = cumsum(gaData$transactionRevenue), color=gaData$channelGrouping, size=gaData$transactionRevenue)) + geom_point()

#gaDataSelected <- gaData
#gaDataSelected <- gaData %>% 
  #filter(deviceCategory == "tablet") %>% 
  #select(date, channelGrouping, sessions) %>%
  #spread(channelGrouping, sessions)

#rename columns
#names(gaDataSelected)[2]<-"Other"
#names(gaDataSelected)[6]<-"Organic"
#names(gaDataSelected)[8]<-"Paid"

names(gaData)[2]<-"Other"
names(gaData)[6]<-"Organic"
names(gaData)[8]<-"Paid"

# - GENERAL STATISTICS -------------------------------------------------------

#gaDataSelected$date <- as.Date(gaDataSelected$date)
#gaDataSelected[is.na(gaDataSelected)] <- 0

#pairs(gaDataSelected)

library(ggplot2)
gg <- ggplot(data = gaData) + 
  theme_minimal() + 
  ggtitle("Organic (orange) vs Paid (red)")
gg <- gg + geom_line(aes(x = as.Date(date), y = `Organic Search`), col = "orange")
      #gg + geom_line(aes(x = as.Date(date), y = `Direct`), col = "blue")
      gg + geom_line(aes(x = as.Date(date), y = `Paid Search`), col = "red")

ccf(gaDataSelected$Organic, gaDataSelected$Direct)


set.seed(3)
#unique_clicks.c = scale(fbDataAdsetsLINK_CLICKS$unique_clicks, )
organic = gaData$`Organic Search` #scale(gaData$`Organic Search`,center=TRUE, scale=TRUE)
direct = gaData$Direct #scale(gaData$Direct,center=TRUE, scale=TRUE)
lmData <- lm(direct~organic,gaData)
lmData2 <- lm(organic~direct,gaData)
#plot(lmData)
summary(lmData)
summary(lmData2)

#predict minimum direct sessies on organic sessies
#round(predict(lmData, data.frame(organic = c(5000, 3000, 1000))))
prediction <- round(predict(lmData, organic = c(700000)))
prediction2 <- round(predict(lmData2, direct = c(sum(prediction$prediction))))

prediction <- as.data.frame(prediction)
prediction$day <- index(prediction)
prediction$day <- as.numeric(prediction$day)
prediction$current <- gaData$Direct

prediction2 <- as.data.frame(prediction2)
prediction2$day <- index(prediction2)
prediction2$day <- as.numeric(prediction2$day)

prediction$organicCurrent <- gaData$`Organic Search`
prediction$organicPrediction <- prediction2$prediction2

#plot(prediction$day,prediction$prediction)

  gg <- ggplot(data = prediction) + 
  theme_minimal() + 
  ggtitle("Predicted (red) vs Current (blue)")
  gg + geom_line(aes(x = day, y = cumsum(prediction)), col = "red") + geom_line(aes(x = day, y = cumsum(current)), col = "blue") + geom_line(aes(x = day, y = cumsum(organicCurrent)), col = "blue") #+ geom_line(aes(x = day, y = cumsum(organicPrediction)), col = "red")
  
  gg <- ggplot(prediction) + 
    theme_minimal() + 
    ggtitle("Predicted Direct traffic (red) vs Current Direct traffic (blue)")
  gg + geom_col(aes(day,prediction,col="blue")) + geom_col(aes(day,current,col="orange"))
  
  
  gg <- ggplot(prediction, aes(x=day, y=organicCurrent)) + geom_bar(stat="identity")
  
  prediction2$prediction2

  sum(prediction$prediction)
  sum(gaData$Direct)
  sum(prediction2$prediction2)
  sum(gaData$`Organic Search`)
  
  #bereken verder met (predicted) value * CPM * conversion ratio
  #breng scenario's in een pdf ? 
  
  
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
plot(decompose(gaDataSelectedTS[-1]))
#decompose(gaDataSelectedTS[, "Direct"])
#plot(decompose(gaDataSelectedTS[, "Direct"]),type="b")

decompose(gaDataSelectedTS[, "newUsers"])
plot(decompose(gaDataSelectedTS[, "newUsers"]),type="b")

#decompose(gaDataSelectedTS)
#plot(decompose(gaDataSelectedTS),type="b") 

#fit <- ets(gaDataSelectedTS[, "Newsletter NL"])
#fc <- forecast(fit,h=12)
#plot(fc,type="b")

#subtract seasonal impact from time series
#gaDataSelectedDecomposedComponents <- decompose(gaDataSelectedTS)
#gaDataSelectedDecomposedComponents <- decompose(gaDataSelectedTS[, "Paid"])
gaDataSelectedDecomposedComponents <- decompose(gaDataSelectedTS[, "newUsers"])
#plot(gaDataSelectedDecomposedComponents$seasonal)
#gaDataSelectedTSAdjusted <- gaDataSelectedTS - gaDataSelectedDecomposedComponents$seasonal #- gaDataSelectedDecomposedComponents$random
#gaDataSelectedTSAdjusted <- gaDataSelectedTS[, "Paid"] - gaDataSelectedDecomposedComponents$seasonal
gaDataSelectedTSAdjusted <- gaDataSelectedTS[, "newUsers"] - gaDataSelectedDecomposedComponents$seasonal
gaDataSelectedTSAdjusted <- gaDataSelectedDecomposedComponents$trend - gaDataSelectedDecomposedComponents$seasonal
plot(gaDataSelectedTSAdjusted,type="b")

#check what forecasting technique should be picked
#gaDataSelectedForecast <- HoltWinters(gaDataSelectedTS,b=F,gamma=F)
#gaDataSelectedForecast <- HoltWinters(gaDataSelectedTS,gamma=F)
#adjust start value for forecasting -> l.start = first value of fitted values & b = slope, based on lagging forecasted trendline (approx guess)
#alpha high: estimate of the level is based on recent observations
#beta low: slope of the trend component is not updated over the time series
#gamma high: seasonal values taken from recent observations
#gaDataSelectedForecast <- HoltWinters(gaDataSelectedTS, l.start=8.00, b.start = 9)
gaDataSelectedForecast <- HoltWinters(gaDataSelectedTS[, "newUsers"])
#gaDataSelectedForecast <- HoltWinters(gaDataSelectedTS)
plot(gaDataSelectedForecast)
#forecasted values
gaDataSelectedForecast$fitted
#check error rate
gaDataSelectedForecast$SSE
#check final forecast in next x data points (days, months, years, ...)
gaDataSelectedForecastFinal <- forecast(gaDataSelectedForecast, h=365)
plot(gaDataSelectedForecastFinal,type="b")

#check if the predictive model can be improved by checking the errors
(acf(gaDataSelectedForecastFinal$residuals, lag.max=20, na.action = na.pass))
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
pre.period <- as.Date(c("2018-02-17","2018-04-16"))
post.period <- as.Date(c("2018-04-17","2018-06-17"))

## data in order of response, predictor1, predictor2, etc.
ciModel <- gaDataSelectedXTS[, c("Organic","Paid","Direct")]

impact <- CausalImpact(ciModel, pre.period, post.period)
plot(impact)
summary(impact,"report")



#https://www.youtube.com/watch?v=GTgZfCltMm8

library(xts)
gaDataXTS <- xts(gaData[-1], order.by = as.Date(gaData$date), frequency = 365)

library(CausalImpact)
pre.period <- as.Date(c("2018-07-01","2018-09-30"))
post.period <- as.Date(c("2018-10-01","2018-12-31"))

## data in order of response, predictor1, predictor2, etc.
ciModel <- gaDataXTS[, c("Paid","Organic","Direct")]

impact <- CausalImpact(ciModel, pre.period, post.period)
#impact <- CausalImpact(gaDataXTS, pre.period, post.period)
plot(impact)

print(impact)
