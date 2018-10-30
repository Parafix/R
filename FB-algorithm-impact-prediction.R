# ------------------------------------------------------------------------
# FACEBOOK AlGORITHM IMPACT PREDICTION
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://developers.facebook.com/tools/accesstoken
# https://www.deducive.com/blog/2018/1/24/fbinsightsr-get-facebook-marketing-insights-into-r-easily
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
fbKey = "EAAIfIYjc6WUBALu88PqfkigIsmDzsvN7ZAPge8aNlTn31qGlDjM2ZBI6FRNTZBEszVYxMC6LfWkwr8ml7eb2qpeNZCybrXAHq7mY4xt43P9ZAIfkXfDTKkOc3ZCI2BmdkQaSY81APckKZBI2kqZBnytZCnhmlnZB4AxUBKAzZCZBcUwQ6qNri3SZAIPXLXO6bomovDcIZD"
fbAccount = "act_1523559664405168"
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#rm(list=ls())
#dev.off()
setwd(directory)


# ------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------
install.packages("install.load")
library(install.load)
install_load("devtools","dlookr","dplyr","corrplot","reshape2","xlsx")
install_github("Deducive/FBinsightsR")
library(FBinsightsR)

# ------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------
#open directory
opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}


# ------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------

#help
?fbins_summ
?fbins_ag

#get facebook adsets
fbDataAdsets <- fbins_summ("2018-02-04","2018-09-03","adset","",fbKey,fbAccount)

# ------------------------------------------------------------------------
# Clean & structure data
# ------------------------------------------------------------------------

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
  assign(paste("fbDataAdsets", i, sep = ""), fbDataAdsets %>% filter(objective == i) %>% select(impressions, cpm, reach, frequency, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, spend))
}

# ------------------------------------------------------------------------
# Correlation
# ------------------------------------------------------------------------

#calc correlation of all variables
fbDataAdsetsREACHcor <- cor(fbDataAdsetsREACH)
fbDataAdsetsLINK_CLICKScor <- cor(fbDataAdsetsLINK_CLICKS)
fbDataAdsetsCONVERSIONScor <- cor(fbDataAdsetsCONVERSIONS)
#plot correlation of all variables
#pairs(fbDataAdsetsREACH)

corTestREACH <- cor.mtest(fbDataAdsetsREACHcor,0.95)
corrplot(fbDataAdsetsREACHcor, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = corTestREACH[[1]], sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

corTestLINK_CLICKS <- cor.mtest(fbDataAdsetsLINK_CLICKScor,0.95)
corrplot(fbDataAdsetsREACHcor, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = corTestLINK_CLICKS[[1]], sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

corTestCONVERSIONS <- cor.mtest(fbDataAdsetsCONVERSIONScor,0.95)
corrplot(fbDataAdsetsREACHcor, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = corTestCONVERSIONS[[1]], sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

#calc correlation
#round(cor(fbDataAdsetsREACH$reach, fbDataAdsetsREACH$unique_clicks),2)
#correlation test
cor.test(fbDataAdsetsLINK_CLICKS$reach, fbDataAdsetsLINK_CLICKS$unique_clicks)
cor.test(fbDataAdsetsLINK_CLICKS$reach, fbDataAdsetsLINK_CLICKS$cpm)

# ------------------------------------------------------------------------
# Build & evalute model
# ------------------------------------------------------------------------

## MODEL NEEDS TO MATCH ALGORITHM > BASED ON VARIABLES YOU CAN STEER
## REMODEL TO TRAFFIC

#build linear model where reach is modelled by unique clicks
#boxplot.stats(fbDataAdsetsLINK_CLICKS$reach)$out
#fbDataAdsetsLINK_CLICKSscaled <- scale(fbDataAdsetsLINK_CLICKS)

#fbDataAdsetsLINK_CLICKS$unique_clicks <- fbDataAdsetsLINK_CLICKS$unique_clicks - boxplot.stats(fbDataAdsetsLINK_CLICKS$unique_clicks)$out
#fbDataAdsetsLINK_CLICKS$reach <- fbDataAdsetsLINK_CLICKS$reach - boxplot.stats(fbDataAdsetsLINK_CLICKS$reach)$out

set.seed(3)
unique_clicks.c = scale(fbDataAdsetsLINK_CLICKS$unique_clicks, center=TRUE, scale=FALSE)
linearModLINK_CLICKS <- lm(reach~unique_clicks.c,fbDataAdsetsLINK_CLICKS)
plot(linearModLINK_CLICKS)
summary(linearModLINK_CLICKS)

#predict reach based on unique clicks
predict(linearModLINK_CLICKS, data.frame(unique_clicks = c(5000, 3000, 1000)))

#build linear model where reach is modelled by CPM
linearModREACHCPM <- lm(reach~cpm,fbDataAdsetsLINK_CLICKS)
plot(linearModREACHCPM)
summary(linearModREACHCPM)
#predict unique clicks based on reach & frequency
predict(linearModREACHCPM, data.frame(cpm = c(3, 4, 10)))


# ------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------

#export to xls
export <- 
write.xlsx(export,"export.xls")

#open directory with export file
opendir(getwd())
