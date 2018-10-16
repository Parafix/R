# ------------------------------------------------------------------------
# FACEBOOK REACH PREDICTION
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References: 
# https://www.deducive.com/blog/2018/1/24/fbinsightsr-get-facebook-marketing-insights-into-r-easily
# https://developers.facebook.com/tools/accesstoken
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
fbKey = ""
fbAccount = "act_"
#rm(list=ls())
#dev.off()
setwd(directory)

# ------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------
install.packages("devtools")
library(devtools)
install_github("Deducive/FBinsightsR")
library(FBinsightsR)
install.packages("dplyr")
library(dplyr)
install.packages("reshape2")
library(reshape2)
install.packages('xlsx')
library(xlsx)

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
# Code
# ------------------------------------------------------------------------

#help
?fbins_summ
?fbins_ag

#get adsets
fbDataAdsets <- fbins_summ("2018-06-22","2018-06-28","adset","",fbKey,fbAccount)
#get ads
fbDataAds <- fbins_summ("2018-06-22","2018-06-28","ad","",fbKey,fbAccount)

#glimpse(fbDataAds)
#glimpse(fbAge)

#check dataframe
str(fbDataAdsets)

#char to numeric
fbDataAdsets$unique_clicks <- as.numeric(fbDataAdsets$unique_clicks)
fbDataAdsets$reach <- as.numeric(fbDataAdsets$reach)

#calc correlation
round(cor(fbDataAdsets$reach, fbDataAdsets$unique_clicks),2)
#correlation test
cor.test(fbDataAdsets$reach, fbDataAdsets$unique_clicks)

#build linear model
linearMod <- lm(reach~unique_clicks,fbDataAdsets)
sum=summary(linearMod)

#predict reach based on unique clicks with model
predict(linearMod, data.frame(unique_clicks = c(5000, 3000, 1000)))

#plot reach & unique clicks
plot(fbDataAdsets$reach,fbDataAdsets$unique_clicks)
#plot full model
plot(linearMod)

#export to xls
export <- fbDataAds
write.xlsx(export,"export.xls")

#open directory with export file
opendir(getwd())
