# ------------------------------------------------------------------------
# Descriptive analytics - Source/Mediums
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
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

gaClient <- "Partena"
gaViewName <- "Ziekenfond-Conversies"
gaViewId <- "112504157"
gaDateRange <- c("2018-01-01","2018-12-31")
gaDimensions <- c("date","campaign","sourceMedium","channelGrouping")
gaMetrics <- c("sessions","users","newUsers","goal10Completions","goal20Completions","goal15Completions","goal3Completions","goal2Completions","goal12Completions")
#"goal8Completions","goal18Completions","goal7Completions","goal17Completions","goal4Completions","goal14Completions","goal6Completions","goal16Completions"
gaDelta <- order_type("date","ASCENDING", "DELTA")
#gaDimFilterSourceMedium <- dim_filter("sourceMedium","REGEXP","cpc")
#gaDimFilters <- filter_clause_ga4(list(gaDimFilterSourceMedium))

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
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, anti_sample = TRUE)

gaTemp <- gaData

names(gaTemp)[8]<-"Bereken bijdrage NL"
names(gaTemp)[9]<-"Bereken bijdrage FR"
names(gaTemp)[10]<-"Bereken voordeel NL"
names(gaTemp)[11]<-"Bereken voordeel FR"
names(gaTemp)[12]<-"Informatie aanvragen NL"
names(gaTemp)[13]<-"Informatie aanvragen FR"


# all traffic
gaPerMonth <- gaTemp %>% 
  group_by(month(date)) %>% 
  summarise(sessions = sum(sessions), users = sum(users), newUsers = sum(newUsers), `Bereken bijdrage NL` = sum(`Bereken bijdrage NL`), `Bereken bijdrage FR` = sum(`Bereken bijdrage FR`), `Bereken voordeel NL` = sum(`Bereken voordeel NL`), `Bereken voordeel FR` = sum(`Bereken voordeel FR`), `Informatie aanvragen NL` = sum(`Informatie aanvragen NL`),`Informatie aanvragen FR`=sum(`Informatie aanvragen FR`))

names(gaPerMonth)[1] <- "month"
gaPerMonth <- as.data.frame(gaPerMonth)

gaDataPerMonth <- gaTemp %>% 
  group_by(month(date),sourceMedium,campaign) %>% 
  summarise(sessions = sum(sessions), users = sum(users), newUsers = sum(newUsers), `Bereken bijdrage NL` = sum(`Bereken bijdrage NL`), `Bereken bijdrage FR` = sum(`Bereken bijdrage FR`), `Bereken voordeel NL` = sum(`Bereken voordeel NL`), `Bereken voordeel FR` = sum(`Bereken voordeel FR`), `Informatie aanvragen NL` = sum(`Informatie aanvragen NL`),`Informatie aanvragen FR`=sum(`Informatie aanvragen FR`))

names(gaDataPerMonth)[1] <- "month"
gaDataPerMonth <- as.data.frame(gaDataPerMonth)

gaPerWeek <- gaTemp %>% 
  group_by(week(date)) %>% 
  summarise(sessions = sum(sessions), users = sum(users), newUsers = sum(newUsers), `Bereken bijdrage NL` = sum(`Bereken bijdrage NL`), `Bereken bijdrage FR` = sum(`Bereken bijdrage FR`), `Bereken voordeel NL` = sum(`Bereken voordeel NL`), `Bereken voordeel FR` = sum(`Bereken voordeel FR`), `Informatie aanvragen NL` = sum(`Informatie aanvragen NL`),`Informatie aanvragen FR`=sum(`Informatie aanvragen FR`))

names(gaPerWeek)[1] <- "week"
gaPerWeek <- as.data.frame(gaPerWeek)

gaDataPerWeek <- gaTemp %>% 
  group_by(week(date),sourceMedium,campaign) %>% 
  summarise(sessions = sum(sessions), users = sum(users), newUsers = sum(newUsers), `Bereken bijdrage NL` = sum(`Bereken bijdrage NL`), `Bereken bijdrage FR` = sum(`Bereken bijdrage FR`), `Bereken voordeel NL` = sum(`Bereken voordeel NL`), `Bereken voordeel FR` = sum(`Bereken voordeel FR`), `Informatie aanvragen NL` = sum(`Informatie aanvragen NL`),`Informatie aanvragen FR`=sum(`Informatie aanvragen FR`))

names(gaDataPerWeek)[1] <- "week"
gaDataPerWeek <- as.data.frame(gaDataPerWeek)


# channel grouping
gaDataChannelsPerMonth <- gaTemp %>% 
  group_by(month(date),channelGrouping) %>% 
  summarise(sessions = sum(sessions), users = sum(users), newUsers = sum(newUsers), `Bereken bijdrage NL` = sum(`Bereken bijdrage NL`), `Bereken bijdrage FR` = sum(`Bereken bijdrage FR`), `Bereken voordeel NL` = sum(`Bereken voordeel NL`), `Bereken voordeel FR` = sum(`Bereken voordeel FR`), `Informatie aanvragen NL` = sum(`Informatie aanvragen NL`),`Informatie aanvragen FR`=sum(`Informatie aanvragen FR`))

names(gaDataChannelsPerMonth)[1] <- "month"
gaDataChannelsPerMonth <- as.data.frame(gaDataChannelsPerMonth)

gaDataChannelsPerWeek <- gaTemp %>% 
  group_by(week(date),channelGrouping) %>% 
  summarise(sessions = sum(sessions), users = sum(users), newUsers = sum(newUsers), `Bereken bijdrage NL` = sum(`Bereken bijdrage NL`), `Bereken bijdrage FR` = sum(`Bereken bijdrage FR`), `Bereken voordeel NL` = sum(`Bereken voordeel NL`), `Bereken voordeel FR` = sum(`Bereken voordeel FR`), `Informatie aanvragen NL` = sum(`Informatie aanvragen NL`),`Informatie aanvragen FR`=sum(`Informatie aanvragen FR`))

names(gaDataChannelsPerWeek)[1] <- "week"
gaDataChannelsPerWeek <- as.data.frame(gaDataChannelsPerWeek)


# campaigns only
gaDataCampaignsPerMonth <- gaTemp %>% 
  group_by(month(date),campaign) %>% 
  summarise(sessions = sum(sessions), users = sum(users), newUsers = sum(newUsers), `Bereken bijdrage NL` = sum(`Bereken bijdrage NL`), `Bereken bijdrage FR` = sum(`Bereken bijdrage FR`), `Bereken voordeel NL` = sum(`Bereken voordeel NL`), `Bereken voordeel FR` = sum(`Bereken voordeel FR`), `Informatie aanvragen NL` = sum(`Informatie aanvragen NL`),`Informatie aanvragen FR`=sum(`Informatie aanvragen FR`))

names(gaDataCampaignsPerMonth)[1] <- "month"
gaDataCampaignsPerMonth <- as.data.frame(gaDataCampaignsPerMonth)

gaDataCampaignsPerWeek <- gaTemp %>% 
  group_by(week(date),campaign) %>% 
  summarise(sessions = sum(sessions), users = sum(users), newUsers = sum(newUsers), `Bereken bijdrage NL` = sum(`Bereken bijdrage NL`), `Bereken bijdrage FR` = sum(`Bereken bijdrage FR`), `Bereken voordeel NL` = sum(`Bereken voordeel NL`), `Bereken voordeel FR` = sum(`Bereken voordeel FR`), `Informatie aanvragen NL` = sum(`Informatie aanvragen NL`),`Informatie aanvragen FR`=sum(`Informatie aanvragen FR`))

names(gaDataCampaignsPerWeek)[1] <- "week"
gaDataCampaignsPerWeek <- as.data.frame(gaDataCampaignsPerWeek)



listOfDatasets <- list("Trend per month" = gaPerMonth,"Trend per week" = gaPerWeek,"Channels per month" = gaDataChannelsPerMonth,"Channels per week" = gaDataChannelsPerWeek,"Campaigns per month" = gaDataCampaignsPerMonth, "All traffic per month" = gaDataPerMonth, "Campaigns per week" = gaDataCampaignsPerWeek, "Campaigns per week" = gaDataPerWeek)
write.xlsx(listOfDatasets, paste(gaClient,"-",gaViewName,"-Effie-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""))


#gaPerWeekCor <- gaPerWeek[,-1]
#pairs(gaDataCor)
cor(gaPerWeek)
corrplot(cor(gaPerWeek),method="number",col=palette_wijs,type="upper", order="alphabet")
corrplot(cor(gaPerMonth),method="number",col=palette_wijs,type="upper", order="alphabet")

gaChannelSessionsPerMonthCor <- gaDataChannelsPerMonth %>% dplyr::select(month, channelGrouping, sessions) %>% spread(channelGrouping, sessions)
gaChannelBBNLPerMonthCor <- gaDataChannelsPerMonth %>% dplyr::select(month, channelGrouping, `Bereken bijdrage NL`) %>% spread(channelGrouping, `Bereken bijdrage NL`)

corrplot(cor(gaChannelBBNLPerMonthCor),method="number",col=palette_wijs,type="upper", order="alphabet")

