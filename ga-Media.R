# ------------------------------------------------------------------------
# Descriptive analytics - Adwords campaigns
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://cran.r-project.org/web/packages/dlookr/vignettes/diagonosis.html
# https://stackoverflow.com/questions/47667994/ggplot-x-axis-labels-with-all-x-axis-values
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# VARIABLES & SETTINGS
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
setwd(directory)

gaClient <- "Bel-bo"
gaViewName <- "Master"
gaViewId <- "36649569"
gaDateRange <- c("2018-09-01","2018-11-30")
gaDimensions <- c("date","campaign","sourceMedium")
gaMetrics <- c("adClicks","impressions","sessions","users","newUsers","CPC","CPM","adCost","transactions","transactionRevenue")
gaDelta <- order_type("date","ASCENDING", "DELTA")
gaDimFilterSourceMedium <- dim_filter("sourceMedium","REGEXP","cpc")
gaDimFilters <- filter_clause_ga4(list(gaDimFilterSourceMedium))

decAfterComma <- 3

# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library(install.load)
install_load("devtools","googleAuthR","googleAnalyticsR","tidyverse","kableExtra","scales","plotly","ggthemes","dplyr","lubridate","openxlsx","corrplot","dlookr","TeX")

# ------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------


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

# ------------------------------------------------------------------------
# IMPORT DATA
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)
#meta <- google_analytics_meta()
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)


# ------------------------------------------------------------------------
# DATA HANDLING
# ------------------------------------------------------------------------

#subset to exclude 'not set'
gaDataCampaigns <- subset(gaData, gaData$campaign!="(not set)")
#gaDataCampaigns <- gaData

#add labels -> hoofdletter gevoelig
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
# DIAGNOSE DATA QUALITY
# ------------------------------------------------------------------------

#diagnose_numeric(gaDataCampaignsPerWeek)
#diagnose_category(gaDataCampaignsPerWeek)
#diagnose_outlier(gaDataCampaignsPerWeek)
#plot_outlier(gaDataCampaignsPerWeek)

gaDataCampaignsPerDay %>%
  diagnose_report(output_format = "html", output_file = paste(gaClient,"-",gaViewName,"-Diagnose-",gaDateRange[1],"-",gaDateRange[2], sep=""))


# ------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------



# ------------------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------------------

listOfDatasets <- list("Media per day" = gaDataCampaignsPerDay, "Media per week" = gaDataCampaignsPerWeek,"Media per month" = gaDataCampaignsPerMonth,"Media per quarter" = gaDataCampaignsPerQuarter,"Media per year" = gaDataCampaignsPerYear)
write.xlsx(listOfDatasets, paste(gaClient,"-",gaViewName,"-Media-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""))


# ------------------------------------------------------------------------
# EXPERIMENT
# ------------------------------------------------------------------------

#create dataset per week per label only
gaDataCampaignsPerWeekByLabel <- gaDataCampaigns %>% 
  group_by(week(date),label) %>% 
  summarise(adClicks = sum(adClicks), impressions = sum(impressions), sessions = sum(sessions), users = sum(users),newUsers = sum(newUsers),CPC = mean(CPC),CPM = mean(CPC), adCost = sum(adCost), transactions = sum(transactions), transactionRevenue = sum(transactionRevenue), weightedMeanRps = weighted.mean(sessions,transactions))

names(gaDataCampaignsPerWeekByLabel)[1] <- "weekofyear"
gaDataCampaignsPerWeekByLabel <- as.data.frame(gaDataCampaignsPerWeekByLabel)


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
    weightedMeanRps
  )

#all NAN to 0
gaDataCampaignsPerWeekByLabel[is.na(gaDataCampaignsPerWeekByLabel)] <- 0



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



gaDataCampaignsPerWeekByLabelCor <- gaDataCampaignsPerWeekByLabel %>% 
  select(weekofyear, label, sessions) %>%
  spread(label,sessions)

pairs(gaDataCampaignsPerWeekByLabelCor)
cor(gaDataCampaignsPerWeekByLabelCor)

corrplot(cor(gaDataCampaignsPerWeekByLabelCor),method="number",col=palette_wijs, type="upper", order="alphabet")


