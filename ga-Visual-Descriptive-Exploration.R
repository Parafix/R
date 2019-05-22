# ------------------------------------------------------------------------
# Descriptive analytics
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# 
# https://flowingdata.com/2012/05/15/how-to-visualize-and-compare-distributions/
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library("install.load")
install_load("devtools")
install_load("dlookr")
install_github('jburkhardt/RAdwords')
install_load("googleAuthR","googleAnalyticsR","RAdwords")
install_load("ggplot2","ggthemes","scales","ggridges","plotly")
install_load("plyr","lubridate","reshape2","tidyr","dplyr","kableExtra")
install_load("TTR","forecast","CausalImpact","formatR","corrplot","Hmisc","SDMTools")
install_load("fpc","cluster","tm","wordcloud")
install_load("openxlsx","readxl")

# ------------------------------------------------------------------------
# VARIABLES & SETTINGS
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
setwd(directory)

gaClient <- "AVogel"
gaViewName <- "Master"
gaViewId <- "2897692"
gaDateRange <- c("2018-01-01","2018-12-31")
gaDimensions <- c("pageDepth")
gaMetrics <- c("sessions","percentNewSessions","pageviews","entrances","exitRate","exits","bounces","bounceRate","pageValue","transactions")
#gaDelta <- order_type("date","ASCENDING", "DELTA")
#gaDimFilterSourceMedium <- dim_filter("sourceMedium","REGEXP","cpc")
#gaDimFilters <- filter_clause_ga4(list(gaDimFilterSourceMedium))
#gaMetFilterGoal <- met_filter("goal13Completions", "GREATER", 0)
#gaMetFilterGoal <- met_filter("goal13Completions", "EQUAL", 0)
#gaMetFilters <- filter_clause_ga4(list(gaMetFilterGoal))


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

col_wijs_lb = c("#DEECFF")
col_wijs_db = c("#1B31CC")
col_wijs_db2 = c("#1C31CC")
col_wijs_r = c("#FF4040")

# ------------------------------------------------------------------------
# IMPORT DATA
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)
#meta <- google_analytics_meta()
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, anti_sample = TRUE)

# ------------------------------------------------------------------------
# DATA HANDLING
# ------------------------------------------------------------------------

gaDataDistri <- as.data.frame(gaData)

gaDataDistriDepth <- gaDataDistri %>% 
  select(channelGrouping, pageDepth, sessions) %>%
  spread(channelGrouping,sessions) %>%
  arrange(as.numeric(pageDepth)) %>%
  subset(as.numeric(pageDepth) < 101)

gaDataDistriDepth[is.na(gaDataDistriDepth)] <- 0

gaDataDistriDepth <- gaDataDistriDepth %>%
  group_by(pageDepth) %>%
  mutate(All = sum(Affiliate, Direct, Display, Referral, Social,`Organic Search`,`Paid Search`,`(Other)`))

par(mfrow=c(1,1))
#plot(gaDataDistriDepth$pageDepth,gaDataDistriDepth$All, type="l",col=c("#FF4040"),lwd=2, pch=19)
#plot(gaDataDistriDepth$pageDepth,gaDataDistriDepth$Affiliate)
#plot(gaDataDistriDepth$pageDepth,gaDataDistriDepth$Direct)

# ------------------------------------------------------------------------
# SCATTERPLOT
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# STATISTICS
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# DISTRIBUTION
# ------------------------------------------------------------------------

dev.off()
par(mfrow=c(5,2))
for(i in 1:11){
  hist(gaData[,i])
  print(i)
}

#viz
barplot(gaDataDistriDepth$All,col=c("#FF4040"),names.arg=gaDataDistriDepth$pageDepth, main="Sessions per pageDepth - Not Account Completed", xlab="pageDepth", ylab="Sessions")

# ------------------------------------------------------------------------
# MACHINE LEARNING - CLASSIFICATION TREE
# ------------------------------------------------------------------------



# ------------------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------------------

write.xlsx(dfMerged, paste(gaClient,"-",gaViewName,"-Descriptive-Exploration-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""))

dev.copy(png, paste(gaClient,"-",gaViewName,"-Scatterplot.png", sep=""),width=2048,height=1536,res=72)
dev.off()