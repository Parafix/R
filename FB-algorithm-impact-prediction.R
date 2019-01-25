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
# https://cran.r-project.org/web/packages/dataPreparation/vignettes/train_test_prep.html
# https://www.dataquest.io/blog/statistical-learning-for-predictive-modeling-r/
# https://www.r-bloggers.com/outlier-detection-and-treatment-with-r/
# https://cloud.r-project.org/web/packages/olsrr/vignettes/influence_measures.html
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
fbKey = "EAAIfIYjc6WUBAB578SOSwZBeifhXpz81cI7GUp0aUYZCkzfkdmWT2CejKhKXYmW19WhZAr6hwQh1tMkvG7hwVbgcXiBa0mZAbZB3CMF8jjLoVJBu6DBeL3ezFglFXudLKsw498JhmhhxG7CcvuQTPY9Yre0rDLZABMHKbZCBbEJvic1kakKEjdkVWbvOZBgxIGoZD"
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
install_load("devtools","dlookr","dplyr","corrplot","reshape2","openxlsx","dataPreparation","ggplot2","olsrr")
install_github("Deducive/FBinsightsR", force=TRUE)
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
fbDataAdsets <- fbins_summ("2018-02-04","2018-12-31","adset","",fbKey,fbAccount)

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
for (i in c("REACH", "LINK_CLICKS", "CONVERSIONS","VIDEO_VIEWS")) {
  assign(paste("fbDataAdsets", i, sep = ""), fbDataAdsets %>% filter(objective == i) %>% select(impressions, cpm, reach, frequency, clicks, unique_clicks, ctr, cpc, unique_ctr, cost_per_unique_click, spend))
}

# ------------------------------------------------------------------------
# Correlation
# ------------------------------------------------------------------------

#calc correlation of all variables
fbDataAdsetsREACHcor <- cor(fbDataAdsetsREACH)
fbDataAdsetsLINK_CLICKScor <- cor(fbDataAdsetsLINK_CLICKS)
fbDataAdsetsCONVERSIONScor <- cor(fbDataAdsetsCONVERSIONS)
fbDataAdsetsVIDEO_VIEWScor <- cor(fbDataAdsetsVIDEO_VIEWS)
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
corrplot(fbDataAdsetsLINK_CLICKScor, method="color", col=col(255),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = corTestLINK_CLICKS[[1]], sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

# corTestCONVERSIONS <- cor.mtest(fbDataAdsetsCONVERSIONScor,0.95)
# corrplot(fbDataAdsetsCONVERSIONScor, method="color", col=col(200),  
#          type="upper", order="hclust", 
#          addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, #Text label color and rotation
#          # Combine with significance
#          p.mat = corTestCONVERSIONS[[1]], sig.level = 0.05, insig = "blank", 
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE)

#calc correlation
#round(cor(fbDataAdsetsREACH$reach, fbDataAdsetsREACH$unique_clicks),2)
#correlation test
cor.test(fbDataAdsetsLINK_CLICKS$reach, fbDataAdsetsLINK_CLICKS$unique_clicks)
cor.test(fbDataAdsetsLINK_CLICKS$reach, fbDataAdsetsLINK_CLICKS$cpm)
cor.test(fbDataAdsetsLINK_CLICKS$cpm, fbDataAdsetsLINK_CLICKS$unique_clicks)

# ------------------------------------------------------------------------
# Build & evalute model
# ------------------------------------------------------------------------

## MODEL NEEDS TO MATCH ALGORITHM > BASED ON VARIABLES YOU CAN STEER
## REMODEL TO TRAFFIC

#boxplot.stats(fbDataAdsetsLINK_CLICKS$reach)$out

#fbDataAdsetsLINK_CLICKS$unique_clicks <- fbDataAdsetsLINK_CLICKS$unique_clicks - boxplot.stats(fbDataAdsetsLINK_CLICKS$unique_clicks)$out
#fbDataAdsetsLINK_CLICKS$reach <- fbDataAdsetsLINK_CLICKS$reach - boxplot.stats(fbDataAdsetsLINK_CLICKS$reach)$out

#hist(fbDataAdsetsLINK_CLICKS)

#outlier/anomaly detection based on target
mod <- lm(reach ~ .,fbDataAdsetsLINK_CLICKS)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

model <- lm(reach ~ .,fbDataAdsetsLINK_CLICKS)
ols_plot_cooksd_bar(model)
ols_plot_dfbetas(model)
ols_plot_resid_lev(model)

ggplot(data = fbDataAdsetsLINK_CLICKS, aes(x = reach, y = impressions)) + geom_point()  +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

#CAT TOEVOEGEN -> ANOMALY OF ANDERE LABELS
#DAARNA FILTEREN EN TRAINEN

#TRAINING AND TEST SET

set.seed(1)
X_train_index <- sample(1:nrow(fbDataAdsetsLINK_CLICKS), 0.7 * nrow(fbDataAdsetsLINK_CLICKS))
X_test_index <- setdiff(1:nrow(fbDataAdsetsLINK_CLICKS), X_train_index)

X_train <- fbDataAdsetsLINK_CLICKS[X_train_index, -15]
y_train <- fbDataAdsetsLINK_CLICKS[X_train_index, "reach"]
X_test <- fbDataAdsetsLINK_CLICKS[X_test_index, -15]
y_test <- fbDataAdsetsLINK_CLICKS[X_test_index, "reach"]

whichAreConstant(fbDataAdsetsLINK_CLICKS)
whichAreInDouble(fbDataAdsetsLINK_CLICKS)
whichAreBijection(fbDataAdsetsLINK_CLICKS)

#X_train$impressions = NULL
#X_test$impressions = NULL

scales <- build_scales(X_train, verbose = TRUE) #cols = c("capital_gain", "capital_loss")
X_train <- fastScale(X_train, scales = scales, verbose = TRUE)
X_test <- fastScale(X_test, scales = scales, verbose = TRUE)

print(dim(X_train))
print(dim(X_test))

X_train <- sameShape(X_train, referenceSet = X_train, verbose = TRUE)
X_test <- sameShape(X_test, referenceSet = X_test, verbose = TRUE)

set.seed(2)
lmX_train <- lm(reach~unique_clicks,X_train)
summary(lmX_train) 

set.seed(3)
lmX_test <- lm(reach~unique_clicks,X_test)
summary(lmX_test)
round(predict(lmX_test, data.frame(unique_clicks = c(13188, 23008, 10940))))

hist(lmX_train$residuals)

round(predict(lmX_train, data.frame(unique_clicks = c(13188, 23008, 10940))))



#PREDICT REACH NEEDED BASED ON CLICKS WANTED FOR LINK CLICK CAMPAIGNS WITH IMPACT ALL OHTER VARS
set.seed(3)
linearModLINK_CLICKS3 <- lm(reach~.,fbDataAdsetsLINK_CLICKS)
summary(linearModLINK_CLICKS3) 
round(predict(linearModLINK_CLICKS3, data.frame(impressions = c(478989),cpm = c(7.970705),frequency=c(3.081821),clicks=c(20459),unique_clicks = c(13188),ctr=c(4.271288),cpc=c(0.186611),unique_ctr=c(8.485176),cost_per_unique_click=c(0.289497),spend=c(3817.88))))
#(150344/155424)*100

#PREDICT REACH NEEDED BASED ON CLICKS WANTED FOR LINK CLICK CAMPAIGNS WITH IMPACT HYPERPARAMETERS
unique_clicks2 <- fbDataAdsetsREACH$unique_clicks*fbDataAdsetsREACH$unique_clicks
unique_clicks3 <- fbDataAdsetsREACH$unique_clicks*fbDataAdsetsREACH$unique_clicks*fbDataAdsetsREACH$unique_clicks
linearModREACHLINK_CLICKS <- lm(reach~unique_clicks+unique_clicks2+unique_clicks3,fbDataAdsetsREACH)
summary(linearModREACHLINK_CLICKS) 
round(predict(linearModREACHLINK_CLICKS, data.frame(unique_clicks = c(1000, 500, 100),unique_clicks2 = c(1000, 500, 100),unique_clicks3 = c(1000, 500, 100))))


# ------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------

#export to xls
export <- df
write.xlsx(export,"export.xls")

#open directory with export file
opendir(getwd())
