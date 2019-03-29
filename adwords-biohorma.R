# ------------------------------------------------------------------------
# Adwords screening
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://github.com/jburkhardt/RAdwords
# https://purevisibility.com/google-adwords-r/
# https://www.chipoglesby.com/2016/11/analyzing-adwords-with-r-2/
# https://netpeak.net/blog/how-to-measure-lost-revenue-in-google-adwords-with-the-help-of-language-r/
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library("install.load")
install_load("devtools")
install_github('jburkhardt/RAdwords')
install_load("googleAuthR","googleAnalyticsR","RAdwords")
install_load("ggplot2","ggthemes","scales")
install_load("plyr","lubridate","reshape2","tidyr","dplyr")
install_load("TTR","forecast","CausalImpact","formatR","corrplot","Hmisc")
install_load("fpc","cluster","tm","wordcloud")
install_load("openxlsx","readxl")

# ------------------------------------------------------------------------
# VARIABLES & SETTINGS
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
setwd(directory)

gaClient <- "Biohorma - AVogel.be"
adwID <- "616-420-8439"
adwDevToken <- "hZtZF7aZHBb_VCJtSjq0Hw"
adwDateRange <- c("2018-01-01","2018-12-31")
#yesterday <- gsub("-","",format(Sys.Date()-1,"%Y-%m-%d"))
#thirtydays <- gsub("-","",format(Sys.Date()-29,"%Y-%m-%d"))

decAfterComma <- 3

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

googleAuth <- doAuth()

#reports()

#loop through metrics
#maak metric dataframe van
#metricsACP <- as.data.frame(metrics(report = 'CAMPAIGN_PERFORMANCE_REPORT'))
#maak functie van
#metricsACP <- metricsACP %>% 
#  mutate_if(is.factor, as.character) %>% 
#  glimpse()
#names(metricsACP)[1] <- "metrics"
#maak char list van

#Account
adwAccountStatement <- statement(select = c('AdNetworkType1','AdNetworkType2','Clicks','Impressions', 'Cost', 'Ctr', 'AveragePosition','Conversions',
                                            'ConversionRate','Month','SearchRankLostImpressionShare','SearchBudgetLostImpressionShare','SearchExactMatchImpressionShare',
                                            'SearchImpressionShare'),
                                 report = "ACCOUNT_PERFORMANCE_REPORT",
                                 start = adwDateRange[1],
                                 end = adwDateRange[2])
adwAccountData <- getData(clientCustomerId=adwID, google_auth=googleAuth, statement=adwAccountStatement)

#Campaigns
adwCampaignStatement <- statement(select=c('CampaignId','CampaignName','Impressions','Clicks','Cost','Ctr','Conversions','Month','SearchBudgetLostImpressionShare','SearchRankLostImpressionShare',
                                           'ContentBudgetLostImpressionShare','ContentRankLostImpressionShare'),
                                  report="CAMPAIGN_PERFORMANCE_REPORT",
                                  start = adwDateRange[1],
                                  end = adwDateRange[2])
adwCampaignData <- getData(clientCustomerId=adwID, google_auth=googleAuth, statement=adwCampaignStatement)

#Keywords
adwKeywordStatement <- statement(select = c("CampaignName","AdGroupName","Criteria",'Status',"KeywordMatchType","QualityScore","Impressions","Clicks","Ctr","AverageCpc","Cost","Month"),
                  report = "KEYWORDS_PERFORMANCE_REPORT",
                  where = "AdNetworkType1 = SEARCH",
                  start = adwDateRange[1],
                  end = adwDateRange[2])
adwKeywordData <- getData(clientCustomerId=adwID, google_auth=googleAuth, statement=adwKeywordStatement)

# ------------------------------------------------------------------------
# DATA HANDLING
# ------------------------------------------------------------------------

# ACCOUNT
adwAccountData[is.na(adwAccountData)] <- 0

ggplot(adwAccountData, aes(x=factor(month(adwAccountData$Month)), y=Clicks) +
  geom_bar(stat="identity") +
  labs(title = "Clicks, per month", x = "Month", y = "Clicks", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

ggplot(adwAccountData, aes(x=factor(month(adwAccountData$Month)), y=(100*adwAccountData$`SearchLostIS(budget)`))) +
  geom_bar(stat="identity") +
  labs(title = "Lost share of search by budget, per month", x = "Month", y = "SearchBudgetLostImpr.share", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")


#ACCOUNT -> NETWORKS

#adwNetworks <- adwAccountData %>%
#  group_by(month(Month), `Network(withsearchpartners)`) %>%
#  summarise(Clicks = sum(Clicks), Impressions = sum(Impressions), Cost = sum(Cost), CTR = mean(CTR), Conv.rate = mean(Conv.rate), Conversions = sum(Conversions), `SearchLostIS(rank)` = mean(`SearchLostIS(rank)`), `SearchLostIS(budget)` = mean(`SearchLostIS(budget)`), SearchExactmatchIS = mean(SearchExactmatchIS), SearchImpr.share = mean(SearchImpr.share))

adwNetworks <- adwAccountData %>%
  group_by(month(Month), Network) %>%
  summarise(Clicks = sum(Clicks), Impressions = sum(Impressions), Cost = sum(Cost), CTR = mean(CTR), Conv.rate = mean(Conv.rate), Conversions = sum(Conversions), `SearchLostIS(rank)` = mean(`SearchLostIS(rank)`), `SearchLostIS(budget)` = mean(`SearchLostIS(budget)`), SearchExactmatchIS = mean(SearchExactmatchIS), SearchImpr.share = mean(SearchImpr.share))

adwNetworks <- adwNetworks %>%
  mutate(
    Click.share = round((Clicks/sum(Clicks))*100,decAfterComma),
    Impr.share = round((Clicks/sum(Clicks))*100,decAfterComma),
    Conv.share = round((Conversions/sum(Conversions))*100,decAfterComma),
    Cost.share = round((Cost/sum(Cost))*100,decAfterComma),
    CostClick.share = round((Clicks/sum(Cost))*100,decAfterComma),
    CostConv.share = round((Conversions/sum(Cost))*100,decAfterComma),
    CostImpr.share = round((Impressions/sum(Cost))/1000,decAfterComma),
    Impr.lost = round((100-(100*SearchImpr.share)))
  )

adwNetworks <- as.data.frame(adwNetworks)

ggplot(adwNetworks, aes(x=factor(adwNetworks$`month(Month)`), y=(100*Conv.rate), fill=Network)) +
  geom_bar(stat="identity") +
  labs(title = "Conversion rate per month", x = "Month", y = "Conversion rate", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

ggplot(adwNetworks, aes(x=factor(adwNetworks$`month(Month)`), y=CTR, fill=Network)) +
  geom_bar(stat="identity") +
  labs(title = "per month", x = "Month", y = "", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

ggplot(adwNetworks, aes(x=factor(adwNetworks$`month(Month)`), y=Click.share, fill=Network)) +
  geom_bar(stat="identity") +
  labs(title = "Click share per month", x = "Month", y = "", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

ggplot(adwNetworks, aes(x=factor(adwNetworks$`month(Month)`), y=Conv.share, fill=Network)) +
  geom_bar(stat="identity") +
  labs(title = "Conversion share per month", x = "Month", y = "", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

ggplot(adwNetworks, aes(x=factor(adwNetworks$`month(Month)`), y=Cost.share, fill=Network)) +
  geom_bar(stat="identity") +
  labs(title = "Cost share per month", x = "Month", y = "", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

ggplot(adwNetworks, aes(x=factor(adwNetworks$`month(Month)`), y=CostClick.share, fill=Network)) +
  geom_bar(stat="identity") +
  labs(title = "Click share over adSpend/year, per month", x = "Month", y = "", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

ggplot(adwNetworks, aes(x=factor(adwNetworks$`month(Month)`), y=(100*`SearchLostIS(budget)`), fill=Network)) +
  geom_bar(stat="identity") +
  labs(title = "`SearchLostIS(budget)` per month", x = "Month", y = "", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

ggplot(adwNetworks, aes(x=factor(adwNetworks$`month(Month)`), y=(100*`SearchLostIS(rank)`), fill=Network)) +
  geom_bar(stat="identity") +
  labs(title = "`SearchLostIS(rank)` per month", x = "Month", y = "", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

ggplot(adwNetworks, aes(x=factor(adwNetworks$`month(Month)`), y=(100*SearchImpr.share), fill=Network)) +
  geom_bar(stat="identity") +
  labs(title = "SearchImpr.share per month", x = "Month", y = "", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")


ggplot(adwNetworks, aes(x = factor(adwNetworks$`month(Month)`) , y = CostImpr.share, group=fill=Network, colour = factor(Network))) + 
  geom_point(stat='summary', fun.y=sum) +
  stat_summary(fun.y=sum, geom="line") +
  labs(title = "CPM over adSpend/year, per month", x = "Month", y = "CPM", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Analytics", tag="") +
  theme_wijs


# CAMPAIGNS
adwCampaignData[is.na(adwCampaignData)] <- 0

# totalData$lostImpressionByBudgetSearch  <- round(totalData$Impressions / (1-totalData$`SearchLostIS(budget)`) - totalData$Impressions,0)
# totalData$lostImpressionByRankSearch    <- round(totalData$Impressions / (1-totalData$`SearchLostIS(rank)`) - totalData$Impressions,0)
# totalData$lostImpressionByBudgetDisplay <- round(totalData$Impressions / (1-totalData$`ContentLostIS(budget)`) - totalData$Impressions,0)
# totalData$lostImpressionByRankDisplay   <- round(totalData$Impressions / (1-totalData$`ContentLostIS(rank)`) - totalData$Impressions,0)
# totalData$lostImpressionByBudget        <- totalData$lostImpressionByBudgetSearch + totalData$lostImpressionByBudgetDisplay
# totalData$lostImpressionByRank          <- totalData$lostImpressionByRankSearch  + totalData$lostImpressionByRankDisplay
# totalData$lostClicksByBudget            <- round(totalData$lostImpressionByBudget * (totalData$CTR),0)
# totalData$lostClicksByRank              <- round(totalData$lostImpressionByRank * (totalData$CTR),0)
# totalData$lostTransactionsByBudget      <- round(totalData$lostClicksByBudget * (totalData$transactions / totalData$Clicks),0)
# totalData$lostTransactionsByRank        <- round(totalData$lostClicksByRank * (totalData$transactions / totalData$Clicks),0)
# totalData$lostTransactions              <- totalData$lostTransactionsByBudget + totalData$lostTransactionsByRank
# totalData$lostRevenueByBudget           <- round(totalData$lostTransactionsByBudget * (totalData$transactionRevenue / totalData$transactions),0)
# totalData$lostRevenueByRank             <- round(totalData$lostTransactionsByRank * (totalData$transactionRevenue / totalData$transactions),0)
# totalData$lostRevenue                   <- totalData$lostRevenueByBudget + totalData$lostRevenueByRank

# ggplot(adwCampaignData, aes(x=factor(adwCampaignData$CampaignName), y=(100*adwCampaignData$`SearchLostIS(budget)`))) +
#   geom_bar(stat="identity") +
#   labs(title = "Lost Impression share of search by budget, per campaign", x = "Campaign", y = "SearchBudgetLostImpr.share", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

adwNLCampaignsData <- adwCampaignData %>%
  filter(grepl('-NL-', Campaign)) %>% 
  group_by(month(Month), Campaign, quarter(Month)) %>%
  arrange(-Clicks)

adwNLQCampaignsData <- adwCampaignData %>%
  filter(grepl('-NL-', Campaign)) %>% 
  group_by(quarter(Month), Campaign) %>%
  summarise(Clicks = sum(Clicks), Impressions = sum(Impressions), Cost = sum(Cost), CTR = mean(CTR), Conversions = sum(Conversions)) %>%
  arrange(-Clicks) %>%
  arrange(`quarter(Month)`)

adwFRCampaignsData <- adwCampaignData %>%
  filter(grepl('-FR-', Campaign)) %>% 
  group_by(month(Month), Campaign, quarter(Month))

adwFRQCampaignsData <- adwCampaignData %>%
  filter(grepl('-FR-', Campaign)) %>% 
  group_by(quarter(Month), Campaign) %>%
  summarise(Clicks = sum(Clicks), Impressions = sum(Impressions), Cost = sum(Cost), CTR = mean(CTR), Conversions = sum(Conversions)) %>%
  arrange(-Clicks) %>%
  arrange(`quarter(Month)`)


adwNLSearchCampaignsData <- adwCampaignData %>%
  filter(grepl('-NL-S-', Campaign)) %>% 
  group_by(month(Month), Campaign, quarter(Month))

adwFRSearchCampaignsData <- adwCampaignData %>%
  filter(grepl('-FR-S-', Campaign)) %>% 
  group_by(month(Month), Campaign, quarter(Month))

adwNLDisplayCampaignsData <- adwCampaignData %>%
  filter(grepl('-NL-D-', Campaign)) %>% 
  group_by(month(Month), Campaign, quarter(Month))

adwFRDisplayCampaignsData <- adwCampaignData %>%
  group_by(month(Month), Campaign) %>%
  filter(grepl('-FR-D-', Campaign, quarter(Month))
         
adwNLShoppingCampaignsData <- adwCampaignData %>%
  filter(grepl('-NL-Shopping', Campaign)) %>% 
  group_by(month(Month), Campaign, quarter(Month))

adwFRShoppingCampaignsData <- adwCampaignData %>%
  filter(grepl('-FR-Shopping', Campaign)) %>% 
  group_by(month(Month), Campaign, quarter(Month))
  
listOfDatasetsTemp <- list("NL" = adwNLCampaignsData,"FR" = adwFRCampaignsData,"NL Search" = adwNLSearchCampaignsData, "FR Search" = adwFRSearchCampaignsData ,"NL Display" = adwNLDisplayCampaignsData,"FR Display" = adwFRDisplayCampaignsData,"NL Shopping" = adwNLShoppingCampaignsData,"FR Shopping" = adwFRShoppingCampaignsData)
write.xlsx(listOfDatasetsTemp, paste(gaClient,"-Adwords-Campaigns-",adwDateRange[1],"-",adwDateRange[2],".xls", sep=""))


clickOrder <- adwNLQCampaignsData$Campaign[order(adwNLQCampaignsData$`quarter(Month)`, adwNLQCampaignsData$Clicks)]
adwNLQCampaignsData$Campaign <- factor(adwNLQCampaignsData$Campaign, levels=unique(clickOrder),ordered=TRUE)
ggplot(adwNLQCampaignsData, aes(x = Clicks, y = Campaign, colour=as.factor(`quarter(Month)`))) +
  geom_segment(aes(yend = Campaign), xend = 0, colour="grey") +
  geom_point(aes(size=Clicks)) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) + 
  labs(title = "NL - Clicks per Campaign", x = "Clicks", y = "Campaign", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Quarter", caption="based on unsampled data from Google Adwords", tag="")
  #facet_grid(`quarter(Month)` ~ ., scales="free_y",space="free_y")


convOrder <- adwNLQCampaignsData$Campaign[order(adwNLQCampaignsData$`quarter(Month)`, adwNLQCampaignsData$Conversions)]
adwNLQCampaignsData$Campaign <- factor(adwNLQCampaignsData$Campaign, levels=unique(convOrder),ordered=TRUE)
ggplot(adwNLQCampaignsData, aes(x = Conversions, y = Campaign, colour=as.factor(`quarter(Month)`))) +
  geom_segment(aes(yend = Campaign), xend = 0, colour="grey") +
  geom_point(aes(size=Conversions)) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) + 
  labs(title = "NL - Conversions per Campaign", x = "Conversions", y = "Campaign", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Quarter", caption="based on unsampled data from Google Adwords", tag="")
#facet_grid(`quarter(Month)` ~ ., scales="free_y",space="free_y")


clickOrder <- adwFRQCampaignsData$Campaign[order(adwFRQCampaignsData$`quarter(Month)`, adwFRQCampaignsData$Clicks)]
adwFRQCampaignsData$Campaign <- factor(adwFRQCampaignsData$Campaign, levels=unique(clickOrder),ordered=TRUE)
ggplot(adwFRQCampaignsData, aes(x = Clicks, y = Campaign, colour=as.factor(`quarter(Month)`))) +
  geom_segment(aes(yend = Campaign), xend = 0, colour="grey") +
  geom_point(aes(size=Clicks)) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  labs(title = "FR - Clicks per Campaign", x = "Clicks", y = "Campaign", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Quarter", caption="based on unsampled data from Google Adwords", tag="")
  #facet_grid(`quarter(Month)` ~ ., scales="free_y",space="free_y")

convOrder <- adwFRQCampaignsData$Campaign[order(adwFRQCampaignsData$`quarter(Month)`, adwFRQCampaignsData$Conversions)]
adwFRQCampaignsData$Campaign <- factor(adwFRQCampaignsData$Campaign, levels=unique(convOrder),ordered=TRUE)
ggplot(adwFRQCampaignsData, aes(x = Conversions, y = Campaign, colour=as.factor(`quarter(Month)`))) +
  geom_segment(aes(yend = Campaign), xend = 0, colour="grey") +
  geom_point(aes(size=Conversions)) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) + 
  labs(title = "FR - Conversions per Campaign", x = "Conversions", y = "Campaign", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Quarter", caption="based on unsampled data from Google Adwords", tag="")
#facet_grid(`quarter(Month)` ~ ., scales="free_y",space="free_y")

# KEYWORDS

# berekenen van overschot
# x keywords die boven x impressies halen, etc.

#low QS
adwKeywordData$Qualityscore <- as.numeric(adwKeywordData$Qualityscore)
adwKeywordData[is.na(adwKeywordData)] <- 0

adwKeywordQS <- adwKeywordData %>% 
  group_by(Qualityscore) %>% 
  filter(Qualityscore >= 1) %>%
  summarise(keywords = length(Keyword)) %>% 
  arrange(-Qualityscore)

adwKeywordQS <- as.data.frame(adwKeywordQS)

ggplot(adwKeywordQS, aes(x=factor(Qualityscore), y=keywords)) +
  geom_bar(stat="identity") +
  labs(title = "Keywords per Qualityscore", x = "Qualityscore", y = "Keywords", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")


#low impressions
adwKeywordImpr <- adwKeywordData %>% 
  group_by(Impressions) %>% 
  filter(Impressions <= 10) %>%
  summarise(keywords = length(Keyword)) %>% 
  arrange(-Impressions)

adwKeywordImpr <- as.data.frame(adwKeywordImpr)

ggplot(adwKeywordImpr, aes(x=factor(Impressions), y=keywords)) +
  geom_bar(stat="identity") +
  labs(title = "Keywords per Impressions", x = "Impressions", y = "Keywords", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")


#low clicks
adwKeywordClicks <- adwKeywordData %>% 
  group_by(Clicks) %>% 
  filter(Clicks <= 10) %>%
  summarise(keywords = length(Keyword)) %>% 
  arrange(-Clicks)

adwKeywordClicks <- as.data.frame(adwKeywordClicks)

ggplot(adwKeywordClicks, aes(x=factor(Clicks), y=keywords)) +
  geom_bar(stat="identity") +
  labs(title = "Keywords per Clicks", x = "Clicks", y = "Keywords", subtitle=paste0("As of ", adwDateRange[1]," till ",adwDateRange[2]), colour = "Label", caption="based on unsampled data from Google Adwords", tag="")

  
# ------------------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------------------

listOfDatasets <- list("Adwords Account Data" = adwAccountData, "Adwords Network Data" = adwNetworks ,"Adwords Campaign Data" = adwCampaignData,"Adwords Keywords Data" = adwKeywordData)
write.xlsx(listOfDatasets, paste(gaClient,"-Adwords-",adwDateRange[1],"-",adwDateRange[2],".xls", sep=""))