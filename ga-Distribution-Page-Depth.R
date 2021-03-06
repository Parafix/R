# ------------------------------------------------------------------------
# Descriptive analytics - Distribution of page depth
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://flowingdata.com/2012/05/15/how-to-visualize-and-compare-distributions/
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library(install.load)
install_load("devtools","googleAuthR","googleAnalyticsR","tidyverse","kableExtra","scales","plotly","ggthemes","dplyr","lubridate","openxlsx","corrplot","dlookr","Hmisc","SDMTools","ggridges")

# ------------------------------------------------------------------------
# VARIABLES & SETTINGS
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
setwd(directory)

gaClient <- "DVV"
gaViewName <- "2015"
gaViewId <- "95326124"
gaDateRange <- c("2019-01-01","2019-05-12")
gaDimensions <- c("channelGrouping","pageDepth")
gaMetrics <- c("sessions","goal4Completions")
#gaDelta <- order_type("date","ASCENDING", "DELTA")
#gaDimFilterSourceMedium <- dim_filter("sourceMedium","REGEXP","cpc")
#gaDimFilters <- filter_clause_ga4(list(gaDimFilterSourceMedium))
gaMetFilterGoal <- met_filter("goal13Completions", "GREATER", 0)
#gaMetFilterGoal <- met_filter("goal4Completions", "EQUAL", 0)
gaMetFilters <- filter_clause_ga4(list(gaMetFilterGoal))


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
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, met_filters = gaMetFilters, anti_sample = TRUE)

#assisted can't be linked on a daily basis (misses the point of attribution) or can't be linked per campaign -> G UI shows aggragate <> API possibilities 
#gaDataAssisted <- google_analytics_3(gaViewId,start = gaDateRange[1], end = gaDateRange[2], metrics = c("totalConversions","totalConversionValue"), dimensions = c("nthDay","campaignPath"), filters = "mcf:conversionType==Transaction", type="mcf")



# ------------------------------------------------------------------------
# DATA HANDLING
# ------------------------------------------------------------------------

gaDataDistri <- as.data.frame(gaData)

gaDataDistriDepth <- gaDataDistri %>% 
  select(channelGrouping, pageDepth, sessions) %>%
  spread(channelGrouping,sessions) %>%
  arrange(as.numeric(pageDepth)) %>%
  subset(as.numeric(pageDepth) < 101)

gaDataDistriDepth <- as.data.frame(gaDataDistriDepth)
gaDataDistriDepth[is.na(gaDataDistriDepth)] <- 0

gaDataDistriDepth <- gaDataDistriDepth %>%
  group_by(pageDepth) %>%
  mutate(All = sum(`1. AdWords`,`2. Affiliate`,`3. Organisch`,`4. Direct`,`5. Bannering`, REF))
  #mutate(All = sum(`1. AdWords`,`2. Affiliate`,`3. Organisch`,`4. Direct`,`5. Bannering`,`6. INT_Dropout_mail`,`7. SOCIAL_Advertentie`,`8. INT_nieuwsbrief`, INT_Email, REF))
  #mutate(All = sum(Affiliate, Direct, Display, Referral, Social,`Organic Search`,`Paid Search`,`(Other)`))

par(mfrow=c(1,1))
#plot(gaDataDistriDepth$pageDepth,gaDataDistriDepth$All, type="l",col=c("#FF4040"),lwd=2, pch=19)
#plot(gaDataDistriDepth$pageDepth,gaDataDistriDepth$Affiliate)
#plot(gaDataDistriDepth$pageDepth,gaDataDistriDepth$Direct)

#no leads
barplot(gaDataDistriDepth$All,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth - No Auto lead", xlab="pageDepth", ylab="Sessions")
barplot(gaDataDistriDepth$`1. AdWords`,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth (Adwords) - No Auto lead", xlab="pageDepth", ylab="Sessions")
barplot(gaDataDistriDepth$`2. Affiliate`,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth (Affiliate) - No Auto lead", xlab="pageDepth", ylab="Sessions")
barplot(gaDataDistriDepth$`3. Organisch`,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth (Organic) - No Auto lead", xlab="pageDepth", ylab="Sessions")
barplot(gaDataDistriDepth$`4. Direct`,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth (Direct) - No Auto lead", xlab="pageDepth", ylab="Sessions")

#wel leads
barplot(gaDataDistriDepth$All,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth - Auto lead", xlab="pageDepth", ylab="Sessions")
barplot(gaDataDistriDepth$`1. AdWords`,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth (Adwords) - Auto lead", xlab="pageDepth", ylab="Sessions")
barplot(gaDataDistriDepth$`2. Affiliate`,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth (Affiliate) - Auto lead", xlab="pageDepth", ylab="Sessions")
barplot(gaDataDistriDepth$`3. Organisch`,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth (Organic) - Auto lead", xlab="pageDepth", ylab="Sessions")
barplot(gaDataDistriDepth$`4. Direct`,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth (Direct) - Auto lead", xlab="pageDepth", ylab="Sessions")

# ------------------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------------------

write.xlsx(meta, "meta.xls", sep="")