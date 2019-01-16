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
# https://pakillo.github.io/R-GIS-tutorial/#spatial
# https://geocompr.robinlovelace.net/spatial-class.html
# https://geocompr.robinlovelace.net/adv-map.html
# https://www.jessesadler.com/post/geocoding-with-r/
# https://github.com/dkahle/ggmap
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
#set writing directory
setwd('/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/')
#rm(list=ls())
#dev.off()

# ------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------
install.packages("install.load")
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","scales","plyr","lubridate","reshape2","TTR","forecast","tidyr","CausalImpact","dplyr","formatR","corrplot","Hmisc", "openxlsx")
install_load("magrittr","leaflet","rworldmap")
install.packages("BelgiumMaps.StatBel", repos = "http://www.datatailor.be/rcube", type = "source")
library(BelgiumMaps.StatBel)
install_github("dkahle/ggmap", force=TRUE)
library("ggmap")

# ------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------

dfBelgium = read.csv("zipcode-belgium.csv", header = FALSE)
dfPABWL = read.xlsx("2018-postcodes-of-Wallonian-pharmacists.xlsx")
dfPABWLE = read.xlsx("2018-postcodes-of-Wallonian-pharmacists-Echinaforce.xlsx")

dfPABWLE[is.na(dfPABWLE)] <- 0
dfPABWL[is.na(dfPABWL)] <- 0

dfLocations <- dfBelgium %>% select(V2,V1,V3,V4) %>% filter(V1 %in% dfPABWLE$Postcode)
colnames(dfPABWLE)[1] <- "V1"
temp <- inner_join(dfLocations,dfPABWLE, by=c("V1"))

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


qmplot(V3,V4,data=dfLocations, maptype = "toner-background", color = I("red"), geom = "density2d")

tempZonderO <- temp %>% filter(Bestelden > 0)
qmplot(V3,V4,data=tempZonderO, maptype = "toner", color=Bestelden, size=Bestelden.Echinaforce) 
qmplot(V3,V4,data=tempZonderO, maptype = "toner", color=Bestelden, size=Bestelden.Echinaforce) + facet_wrap(~ Bestelden.Echinaforce) + theme(panel.spacing = unit(1, "lines"))
qmplot(V3,V4,data=tempZonderO, maptype = "toner", color=I("red2"), geom = "density2d")
qmplot(V3,V4,data=tempZonderO, maptype = "toner", color=I("red2"), geom = "density2d") + facet_wrap(~ Bestelden.Echinaforce) + theme(panel.spacing = unit(1, "lines"))
  
qmplot(V3,V4,data=temp, maptype = "toner-background", color = I("red"), geom = "density2d") + facet_wrap(~ Bestelden.Echinaforce) + theme(panel.spacing = unit(1, "lines"))
qmplot(V3,V4,data=temp, maptype = "toner-background", color = Bestelden) + facet_wrap(~ Bestelden.Echinaforce)
qmplot(V3,V4,data=temp, maptype = "toner-background", color = Bestelden.Echinaforce, Zoom=6) + facet_wrap(~ Bestelden.Echinaforce) + theme(panel.spacing = unit(1, "lines"))


#robberies <- violent_crimes %>% filter(offense == "robbery")
tempEnkelBestelden <- temp %>% filter(Bestelden > 0)
tempEnkelBestelden <- tempEnkelBestelden[,-c(6,7)]
qmplot(V3,V4,data=tempEnkelBestelden, geom = "blank", zoom = 9, maptype = "toner-lite", legend = "topleft") + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) + 
  scale_fill_gradient2("Biohorma Propensity", low = "white", mid = "yellow", high = "red", midpoint = 0)

tempBezocht <- temp %>% filter(Bezocht > 0)
tempBezocht <-tempBezocht[,-c(5,7)] 
qmplot(V3,V4,data=tempBezocht, geom = "blank", zoom = 9, maptype = "toner-lite", legend = "topleft") + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) + 
  scale_fill_gradient2("Biohorma Bezocht Propensity", low = "white", mid = "yellow", high = "red", midpoint = 0)

tempZonder0Echinaforce <- temp %>% filter(Bestelden.Echinaforce > 0)
qmplot(V3,V4,data=tempZonder0Echinaforce, geom = "blank", zoom = 9, maptype = "toner-lite", legend = "topleft") + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) + 
  scale_fill_gradient2("Echinaforce Propensity", low = "white", mid = "yellow", high = "red", midpoint = 0)

tempEnkelEchinaforce <- temp %>% filter(Bestelden.Echinaforce > 0)
tempEnkelEchinaforce <- tempEnkelEchinaforce[,-c(5,6)]
qmplot(V3,V4,data=tempEnkelEchinaforce, geom = "blank", zoom = 9, maptype = "toner-lite", legend = "topleft") + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) + 
  scale_fill_gradient2("Echinaforce Propensity", low = "white", mid = "yellow", high = "red", midpoint = 0)


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