# ------------------------------------------------------------------------
# FACEBOOK REACH PREDICTION
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# Referenceq: 
# https://www.deducive.com/blog/2018/1/24/fbinsightsr-get-facebook-marketing-insights-into-r-easily
# https://developers.facebook.com/tools/accesstoken
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
fbKey = "EAAIfIYjc6WUBAEBsvFBn9BkZBNAlEZAxsYaQrijsE79tAlAmsMWuZBCdzdQBjh61qPtqbQTms838PONCBURAds2qnemz5jtfXZBPI54cMolO859YZCrjonksaUaPL5sZANu7B7J2tUjGyghifIv9iwckVgFEi11IT0ZAZAC58hc0z89o7zSh5IUYBAoPASpSNrIZD"
fbAccount = "act_1523559664405168"
#rm(list=ls())
#dev.off()
setwd(directory)

# ------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------
install.packages("install.load")
library(install.load)
install_load("devtools","dplyr","reshape2","xlsx")
install_github("Deducive/FBinsightsR")
library(FBinsightsR)

# ------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------
opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

# ------------------------------------------------------------------------
# Calculations
# ------------------------------------------------------------------------

#help
?fbins_summ
?fbins_ag

#get facebook adsets
fbDataAdsets <- fbins_summ("2018-02-04","2018-09-03","adset","",fbKey,fbAccount)

#change char variables to numeric
fbDataAdsets$impressions <- as.numeric(fbDataAdsets$impressions)
fbDataAdsets$cpm <- as.numeric(fbDataAdsets$cpm)
fbDataAdsets$reach <- as.numeric(fbDataAdsets$reach)
fbDataAdsets$frequency <- as.numeric(fbDataAdsets$frequency)
fbDataAdsets$clicks <- as.numeric(fbDataAdsets$clicks)
fbDataAdsets$unique_clicks <- as.numeric(fbDataAdsets$unique_clicks)
fbDataAdsets$ctr <- as.numeric(fbDataAdsets$ctr)
fbDataAdsets$cpc <- as.numeric(fbDataAdsets$cpc)
fbDataAdsets$unique_ctr <- as.numeric(fbDataAdsets$unique_ctr)
fbDataAdsets$cost_per_unique_click <- as.numeric(fbDataAdsets$cost_per_unique_click)
fbDataAdsets$spend <- as.numeric(fbDataAdsets$spend)
fbDataAdsets$canvas_avg_view_time <- as.numeric(fbDataAdsets$canvas_avg_view_time)
fbDataAdsets$canvas_avg_view_percent <- as.numeric(fbDataAdsets$canvas_avg_view_percent)

#split campaigns per algorithm
for (i in c("REACH", "LINK_CLICKS", "CONVERSIONS")) {
  assign(paste("fbDataAdsets", i, sep = ""), fbDataAdsets %>% filter(objective == i))
}

#check variable types
str(fbDataAdsetsREACH)

#select columns for correlation check
fbDataAdsetsREACHcor = fbDataAdsetsREACH %>% select(impressions, cpm, reach, frequency, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, spend, canvas_avg_view_time, canvas_avg_view_percent)
#Clear NA values
fbDataAdsetsREACHcor = na.omit(fbDataAdsetsREACHcor) 
#calc correlation of all variables
cor(fbDataAdsetsREACHcor)
#plot correlation of all variables
pairs(fbDataAdsetsREACHcor)

#calc correlation
round(cor(fbDataAdsetsREACH$reach, fbDataAdsetsREACH$unique_clicks),2)
#correlation test
cor.test(fbDataAdsetsREACH$reach, fbDataAdsetsREACH$unique_clicks)


## MODEL NEEDS TO MATCH ALGORITHM > BASED ON VARIABLES YOU CAN STEER
## REMODEL TO TRAFFIC

#build linear model where reach is modelled by unique clicks
linearModREACH <- lm(reach~unique_clicks,fbDataAdsetsREACH)
sum=summary(linearModREACH)
#predict reach based on unique clicks
predict(linearModREACH, data.frame(unique_clicks = c(5000, 3000, 1000)))
#plot reach & unique clicks
plot(fbDataAdsetsREACH$reach,fbDataAdsetsREACH$unique_clicks)
#plot full model
plot(linearModREACH)

#build linear model where reach is modelled by unique clicks & frequency
linearModREACH2 <- lm(reach~unique_clicks+frequency,fbDataAdsetsREACH)
sum=summary(linearModREACH2)
#predict unique clicks based on reach & frequency
predict(linearModREACH2, data.frame(unique_clicks = c(1000, 1000, 1000), frequency=c(3,7,14)))
#plot full model
plot(linearModREACH2)

# ------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------

#export to xls
export <- fbDataAds
write.xlsx(export,"export.xls")

#open directory with export file
opendir(getwd())
