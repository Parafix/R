# ------------------------------------------------------------------------
# GIS
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# Dependencies:
# https://console.cloud.google.com/google/maps-apis/apis/geocoding-backend.googleapis.com/credentials?project=export-ga-data-1369&duration=PT1H
# References:
# https://github.com/jief/zipcode-belgium
# https://www.jessesadler.com/post/gis-with-r-intro/
# https://mhermans.net/files/r/be_geomapping/20170226_example_mapping_postcode.html
# http://www.bnosac.be/index.php/blog/55-belgiummaps-statbel-r-package-with-administrative-boundaries-of-belgium
# https://www.r-bloggers.com/belgiummaps-statbel-r-package-with-administrative-boundaries-of-belgium/
# https://pakillo.github.io/R-GIS-tutorial/#spatial
# https://geocompr.robinlovelace.net/spatial-class.html
# https://geocompr.robinlovelace.net/adv-map.html
# https://www.jessesadler.com/post/geocoding-with-r/
# https://github.com/dkahle/ggmap
# https://developers.google.com/analytics/devguides/reporting/realtime/dimsmets/geonetwork <- !!!
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","scales","plyr","lubridate","reshape2","TTR","forecast","tidyr","CausalImpact","dplyr","formatR","corrplot","Hmisc", "openxlsx")
install_load("magrittr","leaflet","rworldmap","sp","leaflet")
install.packages("BelgiumMaps.StatBel", repos = "http://www.datatailor.be/rcube", type = "source")
library(BelgiumMaps.StatBel)
install_github("dkahle/ggmap", force=TRUE)
library("ggmap")


# ------------------------------------------------------------------------
# VARIABLES & SETTINGS
# ------------------------------------------------------------------------
#set writing directory
setwd('/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/')
#rm(list=ls())
#dev.off()

gaClient <- "AVogel"
gaViewName <- "Master"
gaViewId <- "2897692"
gaDateRange <- c("2018-01-01","2018-12-31")
gaDimensions <- c("month","city","country","longitude","latitude") #,"deviceCategory", sourceMedium, pagePath, month, year
gaMetrics <- c("itemRevenue") 
#for visit stuff -> "sessions","percentNewSessions","pageviews","entrances","exitRate","exits","bounces","bounceRate","pageValue","transactions"
gaDelta <- order_type("date","ASCENDING", "DELTA")
gaDimFilterCountry <- dim_filter("country","EXACT","Belgium")
gaDimFilterProducts <- dim_filter("productName","REGEXP","Echinaforce|Dormeasan|Oogdruppels|Herbamare"))
gaDimFilters <- filter_clause_ga4(list(gaDimFilterCountry,gaDimFilterProducts))

gaMetFilterItemRevenue <- met_filter("itemRevenue", "GREATER", 0)
gaMetFilters <- filter_clause_ga4(list(gaMetFilterItemRevenue)) #gaMetFilterTransactions
#gaMetFilterTransactions <- met_filter("transactions", "GREATER", 0) #for visit stuff

filter_d <- filter_clause_ga4(list(
  dim_filter("country", "EXACT", "Belgium"),
  dim_filter("productName", "REGEXP", 'Hemoclin|Multi-gyn|Aciforce') #Echinaforce|Dormeasan|Oogdruppels|Herbamare
),operator = "AND"
)

filter_m <- filter_clause_ga4(list(
  met_filter("itemRevenue", "GREATER", 0)
)
)

decAfterComma <- 3

#list of focus products
#list of other products
#make weights + buckets in quantiles for better plots
#plots
#exports in excel

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

#Google Analytics
ga_auth(new_user = TRUE)
#meta <- google_analytics_meta()
#gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters , met_filters = gaMetFilters, anti_sample = TRUE)
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = filter_d , met_filters = filter_m, anti_sample = TRUE)

#Excel
#dfPABWL = read.xlsx("2018-postcodes-of-Wallonian-pharmacists.xlsx")
#dfPABWLE = read.xlsx("2018-postcodes-of-Wallonian-pharmacists-Echinaforce.xlsx")
dfBelgium = read.csv("zipcode-belgium.csv", header = FALSE)
dfPAB = read.xlsx("2018-postcodes-of-biorhorma-pharmacists.xlsx")


# ------------------------------------------------------------------------
# DATA HANDLING
# ------------------------------------------------------------------------
#dfPABWLE[is.na(dfPABWLE)] <- 0
#dfPABWL[is.na(dfPABWL)] <- 0

dfPAB$postcode <- as.numeric(dfPAB$postcode)
dfPAB[is.na(dfPAB)] <- 0

dfLocations <- dfBelgium %>% dplyr::select(V2,V1,V3,V4) %>% filter(V1 %in% dfPAB$postcode)
colnames(dfPAB)[1] <- "V1"
dfOffline <- inner_join(dfLocations,dfPAB, by=c("V1"))

#visit stuff
gaCities <- gaData %>%
  filter(country == "Belgium") %>%
  group_by(city) %>%
  summarise(longitude = max(longitude), latitude= max(latitude), sessions = sum(sessions), pageviews=sum(pageviews), entrances = sum(entrances), bounces = sum(bounces), pageValue = mean(pageValue), transactions=sum(transactions, percentNewSessions =  mean(percentNewSessions), exitRate = mean(exitRate), bounceRate=mean(bounceRate))) %>%
  filter(city!="(not set)") %>%
  mutate(
    quartile = ntile(sessions, 4),
    decentile = ntile(sessions, 10),
    percentile = ntile(sessions, 100),
    percent = sessions/sum(sessions)
  )

#itemRev stuff
gaCities <- gaData %>%
  filter(country == "Belgium") %>%
  group_by(city) %>%
  summarise(longitude = max(longitude), latitude= max(latitude), itemRevenue = sum(itemRevenue)) %>%
  filter(city!="(not set)") %>% 
  mutate(
    quartile = ntile(itemRevenue, 4),
    decentile = ntile(itemRevenue, 10),
    percentile = ntile(itemRevenue, 100),
    percent = itemRevenue/sum(itemRevenue),
    addUp = cumsum(percent)
    )

dfOnline <- as.data.frame(gaCities)
colnames(dfOnline)[2] <- "V3"
colnames(dfOnline)[3] <- "V4"
dfOnline$V3 <- as.numeric(dfOnline$V3)
dfOnline$V4 <- as.numeric(dfOnline$V4)

#joinen kan niet omwillen van verschillende long & lat cijfers!
#GA long & lat niet zelfde als GIS
#multichannel <- inner_join(offline,gaCities,by=c("V3","V4"))

# ------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------

# data(BE_ADMIN_BELGIUM)
# data(BE_ADMIN_PROVINCE)
# data(BE_ADMIN_REGION)
# plot(BE_ADMIN_REGION, lwd = 1)
# plot(BE_ADMIN_PROVINCE, lwd = 1)
# plot(BE_ADMIN_BELGIUM, lwd = 1)

#leaflet() %>% addTiles() %>% addMarkers(dfLocations, lng=~V3, lat=~V4)

#WORLD
#newmap <- getMap(resolution = "low")
#plot(newmap, xlim = c(20, 59), ylim = c(35, 71), asp = 1)
#points(dfLocations$V3, dfLocations$V4, col = "red", cex = .6)

#us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
#map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
#ggmap(map)

#map <- get_map("Belgium", zoom = 4)
#mapPoints <- ggmap(map) + geom_point(aes(x = V3, y = V4, size = sqrt(dfLocations)), data = dfLocations, alpha = .5)

#plot locations
qmplot(V3,V4,data=dfLocations, maptype = "toner-background", color = I("red"), geom = "density2d")

#plot density of biohorma pharmacists (based on cities)
qmplot(V3,V4,data=dfOffline, geom = "blank", zoom = 9, maptype = "toner-lite", legend = "topleft") + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) + 
  scale_fill_gradient2("Biohorma Propensity", low = "white", mid = "yellow", high = "red", midpoint = 0)

#plot density of orders by biohorma pharmacists
qmplot(V3,V4,data=dfOffline, maptype = "toner", color=I("red2"), geom = "density2d")
qmplot(V3,V4,data=dfOffline, maptype = "toner", color=I("red2"), geom = "density2d") + facet_wrap(~ pharmacists) + theme(panel.spacing = unit(1, "lines"))
qmplot(V3,V4,data=dfOffline, maptype = "toner", color=I("red2"), size=pharmacists, zoom=10) + facet_wrap(~ pharmacists) + theme(panel.spacing = unit(1, "lines"))

#plot density of biohorma visitors (based on cities)
qmplot(V3,V4,data=dfOnline, geom = "blank", zoom = 9, maptype = "toner-lite", legend = "topleft") + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) + 
  scale_fill_gradient2("Biohorma Propensity", low = "white", mid = "yellow", high = "red", midpoint = 0)

#plot density of sessions by visitors avogel.be
qmplot(V3,V4,data=dfOnline, maptype = "toner", color=I("red2"), geom = "density2d")
qmplot(V3,V4,data=dfOnline, maptype = "toner", color=I("red2"), geom = "density2d") + facet_wrap(~ decentile) + theme(panel.spacing = unit(1, "lines"))
qmplot(V3,V4,data=dfOnline, maptype = "toner", color=I("red2"), size=itemRevenue, zoom=8) + facet_wrap(~ decentile) + theme(panel.spacing = unit(1, "lines"))


# tempZonderO <- temp %>% filter(Bestelden > 0)
# qmplot(V3,V4,data=tempZonderO, maptype = "toner", color=Bestelden, size=Bestelden.Echinaforce) 
# qmplot(V3,V4,data=tempZonderO, maptype = "toner", color=Bestelden, size=Bestelden.Echinaforce) + facet_wrap(~ Bestelden.Echinaforce) + theme(panel.spacing = unit(1, "lines"))
# qmplot(V3,V4,data=tempZonderO, maptype = "toner", color=I("red2"), geom = "density2d")
# qmplot(V3,V4,data=tempZonderO, maptype = "toner", color=I("red2"), geom = "density2d") + facet_wrap(~ Bestelden.Echinaforce) + theme(panel.spacing = unit(1, "lines"))
#   
# qmplot(V3,V4,data=temp, maptype = "toner-background", color = I("red"), geom = "density2d") + facet_wrap(~ Bestelden.Echinaforce) + theme(panel.spacing = unit(1, "lines"))
# qmplot(V3,V4,data=temp, maptype = "toner-background", color = Bestelden) + facet_wrap(~ Bestelden.Echinaforce)
# qmplot(V3,V4,data=temp, maptype = "toner-background", color = Bestelden.Echinaforce, Zoom=6) + facet_wrap(~ Bestelden.Echinaforce) + theme(panel.spacing = unit(1, "lines"))

#robberies <- violent_crimes %>% filter(offense == "robbery")
# tempEnkelBestelden <- temp %>% filter(Bestelden > 0)
# tempEnkelBestelden <- tempEnkelBestelden[,-c(6,7)]
# qmplot(V3,V4,data=tempEnkelBestelden, geom = "blank", zoom = 9, maptype = "toner-lite", legend = "topleft") + 
#   stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) + 
#   scale_fill_gradient2("Biohorma Propensity", low = "white", mid = "yellow", high = "red", midpoint = 0)
# 
# tempBezocht <- temp %>% filter(Bezocht > 0)
# tempBezocht <-tempBezocht[,-c(5,7)] 
# qmplot(V3,V4,data=tempBezocht, geom = "blank", zoom = 9, maptype = "toner-lite", legend = "topleft") + 
#   stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) + 
#   scale_fill_gradient2("Biohorma Bezocht Propensity", low = "white", mid = "yellow", high = "red", midpoint = 0)
# 
# tempZonder0Echinaforce <- temp %>% filter(Bestelden.Echinaforce > 0)
# qmplot(V3,V4,data=tempZonder0Echinaforce, geom = "blank", zoom = 9, maptype = "toner-lite", legend = "topleft") + 
#   stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) + 
#   scale_fill_gradient2("Echinaforce Propensity", low = "white", mid = "yellow", high = "red", midpoint = 0)
# 
# tempEnkelEchinaforce <- temp %>% filter(Bestelden.Echinaforce > 0)
# tempEnkelEchinaforce <- tempEnkelEchinaforce[,-c(5,6)]
# qmplot(V3,V4,data=tempEnkelEchinaforce, geom = "blank", zoom = 9, maptype = "toner-lite", legend = "topleft") + 
#   stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) + 
#   scale_fill_gradient2("Echinaforce Propensity", low = "white", mid = "yellow", high = "red", midpoint = 0)


#register_google(key = "AIzaSyBd8_0R0g41ZeHlh4KL71IYFXV1xqzuxKg", account_type = "free", day_limit = 2500)
#ggmap_credentials()
#mapdist(c("houston, texas", "dallas"), "waco, texas")

--------------------------------------------------------------------------

#lijst met polygons etc van Belgi??
#data("BE_ADMIN_MUNTY")
#lijst met gemeentes & nis_codes
#dfBelgium = read.csv("20170224_mapping_municipalities.csv", header = TRUE)
#Postcodes apotheken biohorma walloni??
#dfPABWL = read.xlsx("2018-postcodes-of-Wallonian-pharmacists.xlsx")
#Postcodes apotheken biohorma walloni?? Echinaforce in stock
#dfPABWLE = read.xlsx("2018-postcodes-of-Wallonian-pharmacists-Echinaforce.xlsx")
#dfPABWLE[is.na(dfPABWLE)] <- 0
#dfPABWLE$Postcode <- as.integer(dfPABWLE$Postcode)
#dfLocations <- dfBelgium %>% select(municipality_nis_label_nl, postcode) %>% filter(postcode %in% dfPABWLE$Postcode)
#dfPABWLE$Postcode <- paste(dfPABWLE$Postcode, "Belgium", sep=", ")
#dfLonLat <- geocode(paste(dfLocations$municipality_nis_label_nl, dfLocations$postcode), output="latlon", source="google", client="AIzaSyBd8_0R0g41ZeHlh4KL71IYFXV1xqzuxKg", signature = "AIzaSyBd8_0R0g41ZeHlh4KL71IYFXV1xqzuxKg")
#geocodeQueryCheck(userType = "free")
#str(geocode("Baylor University", output = "all"))

# ------------------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------------------

write.xlsx(dfOnline, paste(gaClient,"-",gaViewName,"-GIS-TABOO-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""))
#dev.copy(png, paste(gaClient,"-",gaViewName,"-SE0-Plot-Cluster-",firstPrio,".png", sep=""),width=2048,height=1536,res=72)
#dev.off()
