# ------------------------------------------------------------------------
# NEXT BEST PAGE/CONTENT ANALYSIS
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# http://r-statistics.co/Association-Mining-With-R.html
# https://discourse.snowplowanalytics.com/t/market-basket-analysis-identifying-products-and-content-that-go-well-together/1132
# Dependencies: xcode-select --install via terminal
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
#rm(list=ls())
#dev.off()
setwd('/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/')

# ------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------
install.packages("install.load")
library(install.load)
install_load("devtools","googleAuthR","googleAnalyticsR","dlookr","dplyr","corrplot")
devtools::install_github("mhahsler/arules")
library(arules)
install.packages('arulesViz')
library(arulesViz)

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

# ------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------

ga_auth(new_user=TRUE)

gaViewId <- "167755651"
gaDateRange <- c("2018-06-01","2018-08-31")
gaDimensions <- c("ga:dimension3","pagePath")
gaMetrics <- c("pageviews")
gaDelta <- order_type("ga:dimension3","ASCENDING", "DELTA")
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, anti_sample = TRUE)

# ------------------------------------------------------------------------
# Clean & structure data
# ------------------------------------------------------------------------

head(gaData)

#trans_prepared <- trans %>% group_by(`Transaction ID`) %>% arrange(Product) %>% summarise(Product = paste(Product, collapse =","), times = length(Product)) %>% arrange(desc(times), Product)
gaDataPrepared <- na.omit(gaData %>% group_by(`dimension3`) %>% arrange(pagePath) %>% summarise(pagePath = paste(pagePath, collapse =",")))
write.csv(gaDataPrepared, "content.csv", row.names = FALSE)
gaContentPrepared = read.transactions("content.csv",format="basket",sep=",",encoding = "UTF-8") #rm.duplicates=TRUE

#plot item frequencies
itemFrequencyPlot(gaContentPrepared,topN=10,type="absolute")
itemFrequencyPlot(gaContentPrepared,topN=50)
itemFrequencyPlot(gaContentPrepared, support =0.05)
#itemFrequencyPlot(gaContentPrepared, support =0.01)

#inspect first 10
inspect(gaContentPrepared[1:10])
#ctc <- crossTable(gaContentPrepared,measure="count",sort=TRUE)
#ctc[1:10, 1:10]
#cts <- crossTable(gaContentPrepared,measure="support",sort=TRUE)
#cts[1:10, 1:10]

#rules <- apriori(gaContentPrepared)
rules <- apriori(gaContentPrepared, parameter = list(supp = 0.001, conf = 0.001))
#rules <- apriori(gaContentPrepared, parameter = list(supp = 0.001, conf = 0.95, minlen=3))
#rules <- apriori(gaContentPrepared, parameter = list(supp = 0.001, conf = 0.95, minlen=2, maxlen=3))
#rules <- apriori(gaContentPrepared, parameter=list(supp=0.001,conf = 0.001), appearance = list(default="rhs", lhs="/es/"), control = list(verbose=F))
#rules <- apriori(gaContentPrepared, parameter=list(supp=0.001,conf = 0.001), appearance = list(default="lhs", rhs="/es/"), control = list(verbose=F))

summary(rules)

# remove redundant rules
subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)
rules <- rules[-subsetRules] # remove subset rules. 

rules <- sort(rules, decreasing=TRUE,by="confidence")
rules_lift <- sort (rules, by="lift", decreasing=TRUE)
rules_count <- sort (rules, by="count", decreasing=TRUE)
rules_support <- sort (rules, by="support", decreasing=TRUE)

inspect(rules)
inspect(rules[1:10])
inspect(rules_lift)
inspect(rules_count)
inspect(rules_support)
#inspect(rules[1:10])
#inspect(rules_lift[1:10])

plot(rules)
plot(rules,method="graph")

# ------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------
#export to csv
write(rules, "rules_content3.csv", row.names = TRUE)

#open directory with export file
opendir(getwd())