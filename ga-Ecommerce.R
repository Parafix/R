# ------------------------------------------------------------------------
# Descriptive analytics - E-commerce product performance
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
gaDimensions <- c("productSku","productName")#,"productCategory")
gaMetrics <- c("itemQuantity","itemRevenue","revenuePerItem","itemsPerPurchase","uniquePurchases","productRevenuePerPurchase","buyToDetailRate","cartToDetailRate","productAddsToCart","productDetailViews") 
gaDimFilterPagePath <- dim_filter("pagePath","REGEXP","nl")
gaDimFilterProductSku <- dim_filter("productSku","REGEXP","(not set)",not = TRUE)
gaDimFilters <- filter_clause_ga4(list(gaDimFilterPagePath, gaDimFilterProductSku), operator = "AND")

#filter toevoegen > SKU is not (not set)

#products FR
gaDimensionsB <- c("productSku","productName")#,"productCategory")
gaMetricsB <- c("itemQuantity","itemRevenue","revenuePerItem","itemsPerPurchase","uniquePurchases","productRevenuePerPurchase","buyToDetailRate","cartToDetailRate","productAddsToCart","productDetailViews") 
gaDimFilterPagePathB <- dim_filter("pagePath","REGEXP","fr")
gaDimFilterProductSkuB <- dim_filter("productSku","REGEXP","(not set)",not = TRUE)
gaDimFiltersB <- filter_clause_ga4(list(gaDimFilterPagePathB, gaDimFilterProductSkuB), operator = "AND")

#productCategory data NL
gaDimensions2 <- c("productSku","productName","productCategoryLevel1")
gaMetrics2 <- c("uniquePurchases") 
gaDimFilterPagePath2 <- dim_filter("pagePath","REGEXP","nl")
gaDimFilterProductSku2 <- dim_filter("productSku","REGEXP","(not set)",not = TRUE)
gaDimFilters2 <- filter_clause_ga4(list(gaDimFilterPagePath2,gaDimFilterProductSku2), operator = "AND")

#productCategory data FR
gaDimensions3 <- c("productSku","productName","productCategoryLevel1")
gaMetrics3 <- c("uniquePurchases") 
gaDimFilterPagePath3 <- dim_filter("pagePath","REGEXP","fr")
gaDimFilterProductSku3 <- dim_filter("productSku","REGEXP","(not set)",not = TRUE)
gaDimFilters3 <- filter_clause_ga4(list(gaDimFilterPagePath3, gaDimFilterProductSku3), operator="AND")

decAfterComma <- 3

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
gaDataProductsNL <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)
gaDataProductsFR <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetricsB, dimensions = gaDimensionsB, dim_filters = gaDimFiltersB, anti_sample = TRUE)
gaDataCategoryNL <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics2, dimensions = gaDimensions2, dim_filters = gaDimFilters2, anti_sample = TRUE)
gaDataCategoryFR <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics3, dimensions = gaDimensions3, dim_filters = gaDimFilters3, anti_sample = TRUE)

dfProductsNL <- as.data.frame(gaDataProductsNL)
dfCategoryNL <- as.data.frame(gaDataCategoryNL)
dfProductsFR <- as.data.frame(gaDataProductsFR)
dfCategoryFR <- as.data.frame(gaDataCategoryFR)

dfAllProductsNL <- full_join(dfProductsNL,dfCategoryNL, by=c("productSku"))
dfAllProductsFR <- full_join(dfProductsFR,dfCategoryFR, by=c("productSku"))

#categories NL
gaDimensions <- c("month","productSku","productCategory")
gaMetrics <- c("revenue") 
#gaDimFilterPagePath <- dim_filter("pagePath","REGEXP","nl")
gaDimFilterProductSku <- dim_filter("productSku","REGEXP","(not set)",not = TRUE)
gaDimFilters <- filter_clause_ga4(list(gaDimFilterProductSku))

ga_auth(new_user = TRUE)
#meta <- google_analytics_meta()
gaDataCategoriesAll <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)
