# ------------------------------------------------------------------------
# BASKET ANALYSIS
# Can be used with filters: source, medium, campaign, etc.
# Dependencies: xcode-select --install via terminal
#
# R Statistics & Data Science for OM
# Language: English
# Code by Dries Bultynck
# Questions & bugs or requests: Dries.bultynck@wijs.be
# http://r-statistics.co/Association-Mining-With-R.html
# https://discourse.snowplowanalytics.com/t/market-basket-analysis-identifying-products-and-content-that-go-well-together/1132
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
# https://www.slideshare.net/dobryweb/how-to-start-with-crosssell-analysis
# ------------------------------------------------------------------------

#rm(list=ls())
#dev.off()
setwd('/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/')

install.packages('dplyr')
install.packages('devtools')
library(dplyr)
library(devtools)
devtools::install_github("mhahsler/arules")
library(arules)
install.packages('arulesViz')
library(arulesViz)

#trans_prepared <- trans %>% group_by(`Transaction ID`) %>% arrange(Product) %>% summarise(Product = paste(Product, collapse =","), times = length(Product)) %>% arrange(desc(times), Product)
trans_prepared <- na.omit(trans %>% group_by(`Transaction ID`) %>% arrange(Product) %>% summarise(Product = paste(Product, collapse =",")))
write.csv(trans_prepared$Product, "transactions.csv", row.names = FALSE)
#trans_prepared = read.transactions("transactions.csv",format="basket",sep=",",encoding = "UTF-8", rm.duplicates= TRUE)
trans_prepared = read.transactions("transactions.csv",format="basket",sep=",",encoding = "UTF-8")
summary(trans_prepared)

itemFrequencyPlot(trans_prepared,topN=10,type="absolute")
itemFrequencyPlot(trans_prepared,topN=10)
itemFrequencyPlot(trans_prepared,topN=50)
itemFrequencyPlot(trans_prepared, support =0.05)
itemFrequencyPlot(trans_prepared, support =0.01)

inspect(trans_prepared[1:10])
ctc <- crossTable(trans_prepared,measure="count",sort=TRUE)
ctc[1:10, 1:10]
cts <- crossTable(trans_prepared,measure="support",sort=TRUE)
cts[1:10, 1:10]

rules <- apriori(trans_prepared)
rules <- apriori(trans_prepared, parameter = list(supp = 0.001, conf = 0.001))
rules <- apriori(trans_prepared, parameter = list(supp = 0.001, conf = 0.001, minlen=2))
rules <- apriori(trans_prepared, parameter = list(supp = 0.001, conf = 0.95, minlen=3))
rules <- apriori(trans_prepared, parameter = list(supp = 0.001, conf = 0.95, minlen=2, maxlen=3))
rules <- apriori(trans_prepared, parameter=list(supp=0.001,conf = 0.001), appearance = list(default="rhs", lhs="Bristol natuursteen beschermer"), control = list(verbose=F))
rules <- apriori(trans_prepared, parameter=list(supp=0.001,conf = 0.001), appearance = list(default="lhs", rhs="Bristol natuursteen beschermer"), control = list(verbose=F))
rules <- apriori(trans_prepared, parameter=list(supp=0.001,conf = 0.001), appearance = list(default="rhs", lhs=c("Gio loungeset")), control = list(verbose=F))
rules <- apriori(trans_prepared, parameter=list(supp=0.001,conf = 0.01), appearance = list(default="lhs", rhs="Gio lounge 1-zit"), control = list(verbose=F))
rules <- apriori(trans_prepared, parameter=list(supp=0.001,conf = 0.01), appearance = list(default="lhs", rhs="Avola parasolvoet"), control = list(verbose=F))
rules <- apriori(trans_prepared, parameter=list(supp=0.001,conf = 0.01), appearance = list(default="lhs", rhs="Caravel tafelset"), control = list(verbose=F))

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
inspect(rules_lift)
inspect(rules_count)
inspect(rules_support)
#inspect(rules[1:10])
#inspect(rules_lift[1:10])

plot(rules)
plot(rules, method="graph", interactive = TRUE)
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, method = "two-key plot")
plot(rules, measure=c("support", "lift"), shading = "confidence", interactive = TRUE)

plot(rules, method = "grouped")
plot(rules, method = "grouped", control = list(k = 5))

subrules2 <- head(rules, n = 10, by = "lift")
plot(subrules2, method = "paracoord")

write(rules, "rules.csv", row.names = TRUE)
