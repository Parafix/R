# ------------------------------------------------------------------------
# Descriptive analytics - Paid (Media) campaigns
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://cran.r-project.org/web/packages/dlookr/vignettes/diagonosis.html
# https://stackoverflow.com/questions/47667994/ggplot-x-axis-labels-with-all-x-axis-values
# https://stackoverflow.com/questions/26950045/group-by-and-scale-normalize-a-column-in-r
# https://stackoverflow.com/questions/29287614/r-normalize-then-plot-two-histograms-together-in-r
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library(install.load)
install_load("devtools","googleAuthR","googleAnalyticsR","tidyverse","kableExtra","scales","plotly","ggthemes","dplyr","lubridate","openxlsx","corrplot","dlookr","Hmisc","SDMTools")

# ------------------------------------------------------------------------
# VARIABLES & SETTINGS
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
setwd(directory)

gaClient <- "Overstock Garden BENL"
gaViewName <- "Master"
gaViewId <- "97928845"
gaDateRange <- c("2018-01-01","2018-12-31")
gaDimensions <- c("date","campaign","sourceMedium")
gaMetrics <- c("adClicks","impressions","sessions","users","newUsers","CPC","CPM","adCost","transactions","transactionRevenue")
gaDelta <- order_type("date","ASCENDING", "DELTA")
gaDimFilterSourceMedium <- dim_filter("sourceMedium","REGEXP","cpc")
gaDimFilters <- filter_clause_ga4(list(gaDimFilterSourceMedium))

decAfterComma <- 3
reportingCyclus <- "" #in weeks/months etc... to calc weights accordingly > get data per x terms ...
  
# ------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------

normalizeIt <- function(m){
  (m - min(m))/(max(m)-min(m))
}

# ------------------------------------------------------------------------
# THEMES & PALETTES
# ------------------------------------------------------------------------

theme_wijs <- theme(
  plot.title = element_text(colour=c("#1B31CC"),face="bold",size=rel(1.5),hjust = 0.1,vjust=-20),
  plot.subtitle = element_text(colour=c("#1C31CC"),face="plain",size=rel(1.2),hjust = 0.1,vjust=-20),
  plot.caption = element_text(colour=c("#1C31CC"),face="plain",size=rel(1)),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  axis.title.x = element_text(colour=c("#1C31CC")),
  axis.title.y = element_text(colour=c("#1C31CC")),
  axis.text.x = element_text(colour=c("#1C31CC")),
  axis.text.y = element_text(colour=c("#1C31CC")),
  axis.line = element_line(colour=c("#1B31CC")),
  axis.ticks = element_line(colour=c("#1B31CC")),
  legend.title = element_text(colour=c("#1B31CC"),face="plain",size=rel(1.2)),
  legend.text = element_text(colour=c("#1C31CC")),
  legend.position = "right",
  legend.box = "vertical"
  #DEECFF lichtblauw
  #1B31CC donkerblauw
  #1C31CC donkerblauw 2
  #FF4040 rood
)
options(scipen=999)  # turn-off scientific notation like 1e+48

palette_wijs <- colorRampPalette(c("#FF4040", "#DEECFF", "#1C31CC"))(20)

col_wijs_lb = c("#DEECFF")
col_wijs_db = c("#1B31CC")
col_wij_db2 = c("#1C31CC")
col_wijs_r = c("#FF4040")

# ------------------------------------------------------------------------
# IMPORT DATA
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)
#meta <- google_analytics_meta()
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)

#assisted can't be linked on a daily basis (misses the point of attribution) or can't be linked per campaign -> G UI shows aggragate <> API possibilities 
#gaDataAssisted <- google_analytics_3(gaViewId,start = gaDateRange[1], end = gaDateRange[2], metrics = c("totalConversions","totalConversionValue"), dimensions = c("nthDay","campaignPath"), filters = "mcf:conversionType==Transaction", type="mcf")


# ------------------------------------------------------------------------
# DATA HANDLING
# ------------------------------------------------------------------------

#subset to exclude 'not set'
gaDataCampaigns <- subset(gaData, gaData$campaign!="(not set)")
#gaDataCampaigns <- gaData

#add labels
gaDataCampaigns = gaDataCampaigns %>% 
  mutate(label = ifelse(grepl("branded",campaign,ignore.case=TRUE),"branded-alwayson", ifelse(grepl("shopping",campaign,ignore.case=TRUE),"shopping-alwayson","non-branded-temp")))

#create dataset per day
gaDataCampaignsPerDay = gaDataCampaigns %>%
  mutate(
    day = weekdays(date),
    dayofweek = wday(date),
    dayofquarter = qday(date),
    weekofyear = week(date),
    month = month(date),
    daysinmonth = days_in_month(date),
    quarter = quarter(date),
    year = year(date),
    leap = leap_year(date),
    sessionShare = round((sessions/sum(sessions))*100,decAfterComma),
    newUsersShare = round((newUsers/sum(newUsers))*100,decAfterComma),
    salesShare = round((transactions/sum(transactions))*100,decAfterComma),
    revenueShare = round((transactionRevenue/sum(transactionRevenue))*100,decAfterComma)
  ) %>%
  arrange(-revenueShare) %>%
  transmute(
    date,
    day,
    dayofweek,
    dayofquarter,
    weekofyear,
    month,
    daysinmonth,
    quarter,
    year,
    leap,
    label,
    sourceMedium,
    campaign,
    clicks = adClicks,
    impressions,
    CPC,
    CPM,
    cost = adCost,
    sessions,
    users,
    newUsers,
    newUsersShare,
    sales = transactions,
    revenue = transactionRevenue,
    sessionShare,
    sessionAddup = round(cumsum(sessionShare),decAfterComma),
    salesShare,
    salesAddup = round(cumsum(salesShare),decAfterComma),
    revenueShare,
    revenueAddup = round(cumsum(revenueShare),decAfterComma),
    crSessions = round((transactions/sessions)*100,decAfterComma),
    crUsers = round((transactions/users)*100,decAfterComma),
    crNewUsers = round((transactions/newUsers)*100,decAfterComma),
    rps = round((revenue/sessions)*100,decAfterComma),
    rpu = round((revenue/users)*100,decAfterComma),
    rpnu = round((revenue/newUsers)*100,decAfterComma),
    roas = round((revenue/adCost)*100,decAfterComma)
  )

#all NAN to 0
gaDataCampaignsPerDay[is.na(gaDataCampaignsPerDay)] <- 0

#create dataset per week
gaDataCampaignsPerWeek <- gaDataCampaigns %>% 
  group_by(week(date),label,sourceMedium,campaign) %>% 
  summarise(adClicks = sum(adClicks), impressions = sum(impressions), sessions = sum(sessions), users = sum(users),newUsers = sum(newUsers),CPC = mean(CPC),CPM = mean(CPC), adCost = sum(adCost), transactions = sum(transactions), transactionRevenue = sum(transactionRevenue))

names(gaDataCampaignsPerWeek)[1] <- "weekofyear"
gaDataCampaignsPerWeek <- as.data.frame(gaDataCampaignsPerWeek)

gaDataCampaignsPerWeek = gaDataCampaignsPerWeek %>%
  mutate(
    sessionShare = round((sessions/sum(sessions))*100,decAfterComma),
    newUsersShare = round((newUsers/sum(newUsers))*100,decAfterComma),
    salesShare = round((transactions/sum(transactions))*100,decAfterComma),
    revenueShare = round((transactionRevenue/sum(transactionRevenue))*100,decAfterComma)
  ) %>%
  arrange(-revenueShare) %>%
  transmute(
    weekofyear,
    label,
    sourceMedium,
    campaign,
    clicks = adClicks,
    impressions,
    CPC,
    CPM,
    cost = adCost,
    sessions,
    users,
    newUsers,
    newUsersShare,
    sales = transactions,
    revenue = transactionRevenue,
    sessionShare,
    sessionAddup = round(cumsum(sessionShare),decAfterComma),
    salesShare,
    salesAddup = round(cumsum(salesShare),decAfterComma),
    revenueShare,
    revenueAddup = round(cumsum(revenueShare),decAfterComma),
    crSessions = round((transactions/sessions)*100,decAfterComma),
    crUsers = round((transactions/users)*100,decAfterComma),
    crNewUsers = round((transactions/newUsers)*100,decAfterComma),
    rps = round((revenue/sessions)*100,decAfterComma),
    rpu = round((revenue/users)*100,decAfterComma),
    rpnu = round((revenue/newUsers)*100,decAfterComma),
    roas = round((revenue/adCost)*100,decAfterComma)
  )

#all NAN to 0
gaDataCampaignsPerWeek[is.na(gaDataCampaignsPerWeek)] <- 0


#create dataset per month
gaDataCampaignsPerMonth <- gaDataCampaigns %>% 
  group_by(month(date),label,sourceMedium,campaign) %>% 
  summarise(adClicks = sum(adClicks), impressions = sum(impressions), sessions = sum(sessions), users = sum(users),newUsers = sum(newUsers),CPC = mean(CPC),CPM = mean(CPC), adCost = sum(adCost), transactions = sum(transactions), transactionRevenue = sum(transactionRevenue))

names(gaDataCampaignsPerMonth)[1] <- "month"
gaDataCampaignsPerMonth <- as.data.frame(gaDataCampaignsPerMonth)

gaDataCampaignsPerMonth = gaDataCampaignsPerMonth %>%
  mutate(
    sessionShare = round((sessions/sum(sessions))*100,decAfterComma),
    newUsersShare = round((newUsers/sum(newUsers))*100,decAfterComma),
    salesShare = round((transactions/sum(transactions))*100,decAfterComma),
    revenueShare = round((transactionRevenue/sum(transactionRevenue))*100,decAfterComma)
  ) %>%
  arrange(-revenueShare) %>%
  transmute(
    month,
    label,
    sourceMedium,
    campaign,
    clicks = adClicks,
    impressions,
    CPC,
    CPM,
    cost = adCost,
    sessions,
    users,
    newUsers,
    newUsersShare,
    sales = transactions,
    revenue = transactionRevenue,
    sessionShare,
    sessionAddup = round(cumsum(sessionShare),decAfterComma),
    salesShare,
    salesAddup = round(cumsum(salesShare),decAfterComma),
    revenueShare,
    revenueAddup = round(cumsum(revenueShare),decAfterComma),
    crSessions = round((transactions/sessions)*100,decAfterComma),
    crUsers = round((transactions/users)*100,decAfterComma),
    crNewUsers = round((transactions/newUsers)*100,decAfterComma),
    rps = round((revenue/sessions)*100,decAfterComma),
    rpu = round((revenue/users)*100,decAfterComma),
    rpnu = round((revenue/newUsers)*100,decAfterComma),
    roas = round((revenue/adCost)*100,decAfterComma)
  )

#all NAN to 0
gaDataCampaignsPerMonth[is.na(gaDataCampaignsPerMonth)] <- 0

#create dataset per quarter
gaDataCampaignsPerQuarter <- gaDataCampaigns %>% 
  group_by(quarter(date),label,sourceMedium,campaign) %>% 
  summarise(adClicks = sum(adClicks), impressions = sum(impressions), sessions = sum(sessions), users = sum(users),newUsers = sum(newUsers),CPC = mean(CPC),CPM = mean(CPC), adCost = sum(adCost), transactions = sum(transactions), transactionRevenue = sum(transactionRevenue))

names(gaDataCampaignsPerQuarter)[1] <- "quarter"
gaDataCampaignsPerQuarter <- as.data.frame(gaDataCampaignsPerQuarter)

gaDataCampaignsPerQuarter = gaDataCampaignsPerQuarter %>%
  mutate(
    sessionShare = round((sessions/sum(sessions))*100,decAfterComma),
    newUsersShare = round((newUsers/sum(newUsers))*100,decAfterComma),
    salesShare = round((transactions/sum(transactions))*100,decAfterComma),
    revenueShare = round((transactionRevenue/sum(transactionRevenue))*100,decAfterComma)
  ) %>%
  arrange(-revenueShare) %>%
  transmute(
    quarter,
    label,
    sourceMedium,
    campaign,
    clicks = adClicks,
    impressions,
    CPC,
    CPM,
    cost = adCost,
    sessions,
    users,
    newUsers,
    newUsersShare,
    sales = transactions,
    revenue = transactionRevenue,
    sessionShare,
    sessionAddup = round(cumsum(sessionShare),decAfterComma),
    salesShare,
    salesAddup = round(cumsum(salesShare),decAfterComma),
    revenueShare,
    revenueAddup = round(cumsum(revenueShare),decAfterComma),
    crSessions = round((transactions/sessions)*100,decAfterComma),
    crUsers = round((transactions/users)*100,decAfterComma),
    crNewUsers = round((transactions/newUsers)*100,decAfterComma),
    rps = round((revenue/sessions)*100,decAfterComma),
    rpu = round((revenue/users)*100,decAfterComma),
    rpnu = round((revenue/newUsers)*100,decAfterComma),
    roas = round((revenue/adCost)*100,decAfterComma)
  )

#all NAN to 0
gaDataCampaignsPerQuarter[is.na(gaDataCampaignsPerQuarter)] <- 0

#create dataset per year
gaDataCampaignsPerYear <- gaDataCampaigns %>% 
  group_by(year(date),label,sourceMedium,campaign) %>% 
  summarise(adClicks = sum(adClicks), impressions = sum(impressions), sessions = sum(sessions), users = sum(users),newUsers = sum(newUsers),CPC = mean(CPC),CPM = mean(CPC), adCost = sum(adCost), transactions = sum(transactions), transactionRevenue = sum(transactionRevenue))

names(gaDataCampaignsPerYear)[1] <- "year"
gaDataCampaignsPerYear <- as.data.frame(gaDataCampaignsPerYear)

gaDataCampaignsPerYear = gaDataCampaignsPerYear %>%
  mutate(
    sessionShare = round((sessions/sum(sessions))*100,decAfterComma),
    newUsersShare = round((newUsers/sum(newUsers))*100,decAfterComma),
    salesShare = round((transactions/sum(transactions))*100,decAfterComma),
    revenueShare = round((transactionRevenue/sum(transactionRevenue))*100,decAfterComma)
  ) %>%
  arrange(-revenueShare) %>%
  transmute(
    year,
    label,
    sourceMedium,
    campaign,
    clicks = adClicks,
    impressions,
    CPC,
    CPM,
    cost = adCost,
    sessions,
    users,
    newUsers,
    newUsersShare,
    sales = transactions,
    revenue = transactionRevenue,
    sessionShare,
    sessionAddup = round(cumsum(sessionShare),decAfterComma),
    salesShare,
    salesAddup = round(cumsum(salesShare),decAfterComma),
    revenueShare,
    revenueAddup = round(cumsum(revenueShare),decAfterComma),
    crSessions = round((transactions/sessions)*100,decAfterComma),
    crUsers = round((transactions/users)*100,decAfterComma),
    crNewUsers = round((transactions/newUsers)*100,decAfterComma),
    rps = round((revenue/sessions)*100,decAfterComma),
    rpu = round((revenue/users)*100,decAfterComma),
    rpnu = round((revenue/newUsers)*100,decAfterComma),
    roas = round((revenue/adCost)*100,decAfterComma)
  )

gaDataCampaignsPerYear[is.na(gaDataCampaignsPerYear)] <- 0


# ------------------------------------------------------------------------
# DATA HANDLING - WEIGHTED BY LABEL FOR TOTAL PERIOD OF TIME
# ------------------------------------------------------------------------

#create dataset per label only for the period set
gaDataCampaignsByLabel <- gaDataCampaigns %>% 
  group_by(label, sourceMedium) %>% 
  summarise(adClicks = sum(adClicks), impressions = sum(impressions), sessions = sum(sessions), users = sum(users),newUsers = sum(newUsers),CPC = mean(CPC),CPM = mean(CPC), adCost = sum(adCost), transactions = sum(transactions), transactionRevenue = sum(transactionRevenue))

gaDataCampaignsByLabel <- as.data.frame(gaDataCampaignsByLabel)

weightedNewUsersToUsers <- (gaDataCampaignsByLabel$newUsers/max(gaDataCampaignsByLabel$newUsers)*gaDataCampaignsByLabel$users) + ((1-(gaDataCampaignsByLabel$newUsers/max(gaDataCampaignsByLabel$newUsers)*mean(gaDataCampaignsByLabel$users))))
weightedSalesToUsers <- (gaDataCampaignsByLabel$transactions/max(gaDataCampaignsByLabel$transactions)*gaDataCampaignsByLabel$users) + ((1-(gaDataCampaignsByLabel$transactions/max(gaDataCampaignsByLabel$transactions)*mean(gaDataCampaignsByLabel$users))))

gaDataCampaignsByLabel = gaDataCampaignsByLabel %>%
  mutate(
    sessionShare = round((sessions/sum(sessions))*100,decAfterComma),
    newUsersShare = round((newUsers/sum(newUsers))*100,decAfterComma),
    salesShare = round((transactions/sum(transactions))*100,decAfterComma),
    revenueShare = round((transactionRevenue/sum(transactionRevenue))*100,decAfterComma)
  ) %>%
  arrange(-revenueShare) %>%
  transmute(
    label,
    sourceMedium,
    clicks = adClicks,
    impressions,
    CPC,
    CPM,
    cost = adCost,
    sessions,
    users,
    newUsers,
    newUsersShare,
    sales = transactions,
    revenue = transactionRevenue,
    sessionShare,
    sessionAddup = round(cumsum(sessionShare),decAfterComma),
    salesShare,
    salesAddup = round(cumsum(salesShare),decAfterComma),
    revenueShare,
    revenueAddup = round(cumsum(revenueShare),decAfterComma),
    crSessions = round((transactions/sessions)*100,decAfterComma),
    crUsers = round((transactions/users)*100,decAfterComma),
    crNewUsers = round((transactions/newUsers)*100,decAfterComma),
    rps = round((revenue/sessions)*100,decAfterComma),
    rpu = round((revenue/users)*100,decAfterComma),
    rpnu = round((revenue/newUsers)*100,decAfterComma),
    roas = round((revenue/adCost)*100,decAfterComma),
    wnutu = weightedNewUsersToUsers,
    wstu = weightedSalesToUsers,
    nwnutu = normalizeIt(weightedNewUsersToUsers),
    nwstu = normalizeIt(weightedSalesToUsers)
  )

#all NAN to 0
gaDataCampaignsByLabel[is.na(gaDataCampaignsByLabel)] <- 0

write.xlsx(gaDataCampaignsByLabel, paste(gaClient,"-",gaViewName,"-Per-Label-Weighted-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""))


# ------------------------------------------------------------------------
# DATA HANDLING - WEIGHTED PER REPORTING CYCLUS
# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# DIAGNOSE DATA QUALITY
# ------------------------------------------------------------------------

#diagnose_numeric(gaDataCampaignsPerDay)
#gaDataCampaignsPerQuarter %>%
#  group_by(label) %>%
#  correlate(gaDataCampaignsPerQuarter)

test <- gaDataCampaignsPerQuarter %>%
  group_by(label) %>%
  filter(label == "non-branded-temp") %>%
  describe()

# test <- as.character(test)
# 
# for (i in length(test)){
#   write.xlsx(data.frame(test[[i]]), paste(gaClient,"-",gaViewName,"-Test-Diagnose-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""), sheetName=paste(i), append=T)
# }
  
#diagnose_category(gaDataCampaignsPerDay)
#diagnose_outlier(gaDataCampaignsPerDay)
#plot_outlier(gaDataCampaignsPerWeek)

#correlate(gaDataCampaignsPerQuarter)

# gaDataCampaignsPerQuarter %>%
#   group_by(label) %>%
#   diagnose_report(output_format = c("html"), output_file = paste(gaClient,"-",gaViewName,"-Diagnose-",gaDateRange[1],"-",gaDateRange[2], sep=""))


# ------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------



# ------------------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------------------

listOfDatasets <- list("Media per day" = gaDataCampaignsPerDay, "Media per week" = gaDataCampaignsPerWeek,"Media per month" = gaDataCampaignsPerMonth,"Media per quarter" = gaDataCampaignsPerQuarter,"Media per year" = gaDataCampaignsPerYear)
write.xlsx(listOfDatasets, paste(gaClient,"-",gaViewName,"-Media-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""))


# ------------------------------------------------------------------------
# EXPERIMENT DATA FILTERING
# ------------------------------------------------------------------------

#create dataset per week per label only
gaDataCampaignsPerWeekByLabel <- gaDataCampaigns %>% 
  group_by(week(date),label) %>% 
  summarise(adClicks = sum(adClicks), impressions = sum(impressions), sessions = sum(sessions), users = sum(users),newUsers = sum(newUsers),CPC = mean(CPC),CPM = mean(CPC), adCost = sum(adCost), transactions = sum(transactions), transactionRevenue = sum(transactionRevenue))

names(gaDataCampaignsPerWeekByLabel)[1] <- "weekofyear"
gaDataCampaignsPerWeekByLabel <- as.data.frame(gaDataCampaignsPerWeekByLabel)

weightedNewUsersToUsers <- (gaDataCampaignsPerWeekByLabel$newUsers/max(gaDataCampaignsPerWeekByLabel$newUsers)*gaDataCampaignsPerWeekByLabel$users) + ((1-(gaDataCampaignsPerWeekByLabel$newUsers/max(gaDataCampaignsPerWeekByLabel$newUsers)*mean(gaDataCampaignsPerWeekByLabel$users))))
weightedSalesToUsers <- (gaDataCampaignsPerWeekByLabel$transactions/max(gaDataCampaignsPerWeekByLabel$transactions)*gaDataCampaignsPerWeekByLabel$users) + ((1-(gaDataCampaignsPerWeekByLabel$transactions/max(gaDataCampaignsPerWeekByLabel$transactions)*mean(gaDataCampaignsPerWeekByLabel$users))))

gaDataCampaignsPerWeekByLabel = gaDataCampaignsPerWeekByLabel %>%
  mutate(
    sessionShare = round((sessions/sum(sessions))*100,decAfterComma),
    newUsersShare = round((newUsers/sum(newUsers))*100,decAfterComma),
    salesShare = round((transactions/sum(transactions))*100,decAfterComma),
    revenueShare = round((transactionRevenue/sum(transactionRevenue))*100,decAfterComma)
  ) %>%
  arrange(-revenueShare) %>%
  transmute(
    weekofyear,
    label,
    clicks = adClicks,
    impressions,
    CPC,
    CPM,
    cost = adCost,
    sessions,
    users,
    newUsers,
    newUsersShare,
    sales = transactions,
    revenue = transactionRevenue,
    sessionShare,
    sessionAddup = round(cumsum(sessionShare),decAfterComma),
    salesShare,
    salesAddup = round(cumsum(salesShare),decAfterComma),
    revenueShare,
    revenueAddup = round(cumsum(revenueShare),decAfterComma),
    crSessions = round((transactions/sessions)*100,decAfterComma),
    crUsers = round((transactions/users)*100,decAfterComma),
    crNewUsers = round((transactions/newUsers)*100,decAfterComma),
    rps = round((revenue/sessions)*100,decAfterComma),
    rpu = round((revenue/users)*100,decAfterComma),
    rpnu = round((revenue/newUsers)*100,decAfterComma),
    roas = round((revenue/adCost)*100,decAfterComma),
    wnutu = weightedNewUsersToUsers,
    wstu = weightedSalesToUsers,
    nwnutu = normalizeIt(weightedNewUsersToUsers),
    nwstu = normalizeIt(weightedSalesToUsers)
  )

#all NAN to 0
gaDataCampaignsPerWeekByLabel[is.na(gaDataCampaignsPerWeekByLabel)] <- 0

write.xlsx(gaDataCampaignsPerWeekByLabel, paste(gaClient,"-",gaViewName,"-Per-Week-By-Label-Weighted-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""))


# ------------------------------------------------------------------------
# EXPERIMENT VIZ
# ------------------------------------------------------------------------


ggplot(gaDataCampaignsPerWeekByLabel, aes(x = factor(weekofyear) , y = sessions, group=label, colour = factor(label))) + 
  geom_point(stat='summary', fun.y=sum) +
  stat_summary(fun.y=sum, geom="line") +
  labs(title = "Sessions per week, by label", x = "Week of the year", y = "Sessions", subtitle=paste0("As of ", gaDateRange[1]," till ",gaDateRange[2]), colour = "Label", caption="based on unsampled data from Google Analytics", tag="") +
  theme_wijs

ggplot(gaDataCampaignsPerWeekByLabel, aes(x = factor(weekofyear) , y = sales, group=label, colour = factor(label))) + 
  geom_point(stat='summary', fun.y=sum) +
  stat_summary(fun.y=sum, geom="line") +
  labs(title = "Sales per week, by label", x = "Week of the year", y = "Sales", subtitle=paste0("As of ", gaDateRange[1]," till ",gaDateRange[2]), colour = "Label", caption="based on unsampled data from Google Analytics", tag="") +
  theme_wijs

ggplot(gaDataCampaignsPerWeekByLabel, aes(x = factor(weekofyear) , y = revenue, group=label, colour = factor(label))) + 
  geom_point(stat='summary', fun.y=sum) +
  stat_summary(fun.y=sum, geom="line") +
  labs(title = "Revenue per week, by label", x = "Week of the year", y = "Revenue", subtitle=paste0("As of ", gaDateRange[1]," till ",gaDateRange[2]), colour = "Label", caption="based on unsampled data from Google Analytics", tag="") +
  theme_wijs

ggplot(gaDataCampaignsPerWeekByLabel, aes(x = factor(weekofyear) , y = rpnu, group=label, colour = factor(label))) + 
  geom_point(stat='summary', fun.y=sum) +
  stat_summary(fun.y=sum, geom="line") +
  labs(title = "Sales per new user, per week, by label", x = "Week of the year", y = "Sales per new user", subtitle=paste0("As of ", gaDateRange[1]," till ",gaDateRange[2]), colour = "Label", caption="based on unsampled data from Google Analytics", tag="") +
  theme_wijs


# par(mfrow = c(4, ncol(gaDataCampaignsPerWeekByLabel)/4))
# for (i in ncol(gaDataCampaignsPerWeekByLabel)) {
#   x <- gaDataCampaignsPerWeekByLabel[,i]
#   hist(x,
#        main = paste("Metric", i),
#        xlab = "Scores")
# }

# ------------------------------------------------------------------------
# EXPERIMENT CORRELATION by LABELS & WEEK OF YEAR
# ------------------------------------------------------------------------

gaDataCampaignsPerWeekByLabelCor <- gaDataCampaignsPerWeekByLabel %>% 
  select(weekofyear, label, sessions) %>%
  spread(label,sessions)

gaDataCampaignsPerWeekByLabelCor[is.na(gaDataCampaignsPerWeekByLabelCor)] <- 0

pairs(gaDataCampaignsPerWeekByLabelCor)
cor(gaDataCampaignsPerWeekByLabelCor)

corrplot(cor(gaDataCampaignsPerWeekByLabelCor),method="number",col=palette_wijs, type="upper", order="alphabet")


# ------------------------------------------------------------------------
# EXPERIMENT WEIGHTED TREND by MONTH OF YEAR
# ------------------------------------------------------------------------

gaDataCampaignsWeightedTrendPerMonth <- gaDataCampaigns %>% 
  group_by(month(date)) %>% #,label -> filteren op label om weighted per type label te krijgen > wanneer beste maand is voor wat ...
  summarise(adClicks = sum(adClicks), impressions = sum(impressions), sessions = sum(sessions), users = sum(users),newUsers = sum(newUsers),CPC = mean(CPC),CPM = mean(CPC), adCost = sum(adCost), transactions = sum(transactions), transactionRevenue = sum(transactionRevenue))

names(gaDataCampaignsWeightedTrendPerMonth)[1] <- "monthofyear"
gaDataCampaignsWeightedTrendPerMonth <- as.data.frame(gaDataCampaignsWeightedTrendPerMonth)

weightedNewUsersToUsers <- ((gaDataCampaignsWeightedTrendPerMonth$newUsers/max(gaDataCampaignsWeightedTrendPerMonth$newUsers))*gaDataCampaignsWeightedTrendPerMonth$users) + ((1-((gaDataCampaignsWeightedTrendPerMonth$newUsers/max(gaDataCampaignsWeightedTrendPerMonth$newUsers))*mean(gaDataCampaignsWeightedTrendPerMonth$users))))
weightedSalesToUsers <- ((gaDataCampaignsWeightedTrendPerMonth$transactions/max(gaDataCampaignsWeightedTrendPerMonth$transactions))*gaDataCampaignsWeightedTrendPerMonth$users) + ((1-((gaDataCampaignsWeightedTrendPerMonth$transactions/max(gaDataCampaignsWeightedTrendPerMonth$transactions))*mean(gaDataCampaignsWeightedTrendPerMonth$users))))

gaDataCampaignsWeightedTrendPerMonth = gaDataCampaignsWeightedTrendPerMonth %>%
  # mutate(
  #   sessionShare = round((sessions/sum(sessions))*100,decAfterComma),
  #   newUsersShare = round((newUsers/sum(newUsers))*100,decAfterComma),
  #   salesShare = round((transactions/sum(transactions))*100,decAfterComma),
  #   revenueShare = round((transactionRevenue/sum(transactionRevenue))*100,decAfterComma)
  # ) %>%
  #arrange(-transactionRevenue) %>% #geen arrange doen: weighted user berekening is op basis van index, index niet aanpassen. of opvangen
  transmute(
    monthofyear,
    #label,
    clicks = adClicks,
    impressions,
    CPC,
    CPM,
    cost = adCost,
    sessions,
    users,
    newUsers,
    #newUsersShare,
    sales = transactions,
    revenue = transactionRevenue,
    #sessionShare,
    #sessionAddup = round(cumsum(sessionShare),decAfterComma),
    #salesShare,
    #salesAddup = round(cumsum(salesShare),decAfterComma),
    #revenueShare,
    #revenueAddup = round(cumsum(revenueShare),decAfterComma),
    crSessions = round((transactions/sessions)*100,decAfterComma),
    crUsers = round((transactions/users)*100,decAfterComma),
    crNewUsers = round((transactions/newUsers)*100,decAfterComma),
    rps = round((revenue/sessions)*100,decAfterComma),
    rpu = round((revenue/users)*100,decAfterComma),
    rpnu = round((revenue/newUsers)*100,decAfterComma),
    roas = round((revenue/adCost)*100,decAfterComma),
    wnutu = weightedNewUsersToUsers,
    wstu = weightedSalesToUsers,
    nwnutu = normalizeIt(weightedNewUsersToUsers),
    nwstu = normalizeIt(weightedSalesToUsers)
  )

#all NAN to 0
gaDataCampaignsWeightedTrendPerMonth[is.na(gaDataCampaignsWeightedTrendPerMonth)] <- 0

# ------------------------------------------------------------------------
# EXPERIMENT CORRELATION by month of year
# ------------------------------------------------------------------------

gaDataCampaignsWeightedTrendPerMonthCor <- gaDataCampaignsWeightedTrendPerMonth %>% 
  select(monthofyear, sessions)

#pairs(gaDataCampaignsWeightedTrendPerMonthCor)
#cor(gaDataCampaignsWeightedTrendPerMonthCor)
#corrplot(cor(gaDataCampaignsWeightedTrendPerMonthCor),method="number",col=palette_wijs, type="upper", order="alphabet")


write.xlsx(gaDataCampaignsWeightedTrendPerMonth, paste(gaClient,"-",gaViewName,"-Weighted-Trend-Per-Month-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""))
