# ------------------------------------------------------------------------
# Descriptive analytics - E-commerce Behavioural Segments
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://www.tatvic.com/blog/visualizing-your-websites-ecommerce-performance-with-r/
# ------------------------------------------------------------------------

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
# VARIABLES & SETTINGS
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
setwd(directory)

gaClient <- "OVSH BE"
gaViewName <- "A. Master View"
gaViewId <- "97926189"
gaDateRange <- c("2018-10-01","2019-05-16")
gaDelta <- order_type("date","ASCENDING", "DELTA")

#products NL
gaDimensions <- c("sessionsToTransaction","daysToTransaction")
gaMetrics <- c("transactions","transactionRevenue")


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

gaEcom <- gaData
gaEcom <- as.data.frame(gaEcom)

#gaEcom.aov <- aov(gaEcom$sessionsToTransaction ~ gaEcom$daysToTransaction)
#gaEcom.lm <- lm(gaEcom$sessionsToTransaction ~ gaEcom$daysToTransaction)

#plot(gaEcom.aov)
#plot(gaEcom.lm)
