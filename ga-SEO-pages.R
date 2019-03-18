# ------------------------------------------------------------------------
# Biohorma GA export page details + page path analysis for SEO
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://www.rdocumentation.org/packages/googleAnalyticsR/versions/0.4.2/topics/filter_clause_ga4
# https://www.statmethods.net/advstats/cluster.html
# http://www.sthda.com/english/wiki/print.php?id=234
# https://www.hackerearth.com/zh/practice/machine-learning/machine-learning-algorithms/clustering-algorithms-evaluation-r/tutorial/
# https://www.r-bloggers.com/finding-optimal-number-of-clusters/
# https://www.r-bloggers.com/k-means-clustering-in-r/
# https://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
# https://uc-r.github.io/kmeans_clustering
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","tidyverse","scales","plyr","lubridate","reshape2","tidyr","dplyr","formatR","openxlsx","readxl")
install_load("TTR","forecast","CausalImpact","formatR","corrplot","Hmisc","fpc","cluster","tm","wordcloud")

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
gaDimensions <- c("year","landingPagePath") #,"deviceCategory", sourceMedium, pagePath, month, year
gaMetrics <- c("sessions","percentNewSessions","pageviews","entrances","exitRate","exits","bounces","bounceRate","pageValue") #exitPagePath, sessions, newUsers",goal2Completions,transactions
gaDelta <- order_type("date","ASCENDING", "DELTA")
gaDimFilterSourceMedium <- dim_filter("sourceMedium","REGEXP","organic|direct")
gaDimFilters <- filter_clause_ga4(list(gaDimFilterSourceMedium))


# ------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------

colnames_to_numeric <- function(df) {
  for (i in colnames(df)){
    colname <- colnames(df[i])
    #if(!is.numeric(colname))
    df[[colname]] <- as.numeric(df[[colname]])
  }
}

colnames_to_character <- function(df) {
  for (i in colnames(df)){
    colname <- colnames(df[i])
    #if(!is.numeric(colname))
    df[[colname]] <- as.character(df[[colname]])
  }
}

elbow.k <- function(df){
  dist.obj <- dist(df)
  hclust.obj <- hclust(dist.obj)
  css.obj <- css.hclust(dist.obj,hclust.obj)
  elbow.obj <- elbow.batch(css.obj)
  k <- elbow.obj$k
  return(k)
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
# Import data
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)
#meta <- google_analytics_meta()

#get data
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)

#inlezen screaming frog data
dfSFPages = read_excel("avogel-screaming-frog.xls",col_names = TRUE, col_types = NULL, na = "", skip = 1)

# ------------------------------------------------------------------------
# DATA HANDLING
# ------------------------------------------------------------------------

gaDataLP <- gaData
gaDataLP <- as.data.frame(gaDataLP)
gaDataLP$landingPagePath <- tolower(gaDataLP$landingPagePath)

dfSFPages <- as.data.frame(dfSFPages)
dfSFPages[is.na(dfSFPages)] <- 0
dfSFPages$Address <- tolower(dfSFPages$Address)

#naam kolom aanpassen
#names(dfSFPages)[1]<-"pagePath"

#factors naar char
#str(dfSFPages)
#dfSFPages <- data.frame(lapply(dfSFPages, as.character), stringsAsFactors=FALSE)

dfSFPages$Address <- unlist(sapply(strsplit(dfSFPages$Address, "\\www.avogel.be"), tail, 1),use.names=FALSE)
names(dfSFPages)[1]<-"landingPagePath"

#join alles op pagePath
dfMerged <- merge(x = gaDataLP, y = dfSFPages, by = "landingPagePath", all = TRUE)
dfMerged[is.na(dfMerged)] <- 0
dfMerged <- dfMerged[!duplicated(dfMerged$landingPagePath), ]
dfMerged$id <- 1:nrow(dfMerged)

# -----------------------------------------------------------------------------
# MACHINE LEARNING - UNSUPERVISED - CLUSTERING BASED ON VOLUME
# -----------------------------------------------------------------------------

dfClusterOnVolume <- dfMerged %>% 
  dplyr::select(id, landingPagePath, sessions)

dfScaledOnVolume <- dfClusterOnVolume
dfScaledOnVolume$sessions <- scale(dfClusterOnVolume$sessions, center = TRUE, scale = TRUE)

# scale data for elbow method
set.seed(123)
k.max <- 50
dfClusteredOnVolumeTemp <- sapply(1:k.max, function(k){kmeans(dfScaledOnVolume$sessions, k, nstart=50,iter.max=15)$tot.withinss})
plot(1:k.max, dfClusteredOnVolumeTemp,type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusterssum of squares")

set.seed(234)
dfClusteredOnVolume <- kmeans(dfScaledOnVolume$sessions,8, nstart=50)
dfClusteredOnVolume$cluster <- as.factor(dfClusteredOnVolume$cluster)
ggplot(dfClusterOnVolume, aes(id, sessions, color = dfClusteredOnVolume$cluster)) + geom_point() +
  labs(color="Cluster")

#order(dfClusteredOnVolume$centers)
# opletten met ID voor matching!

prio <- toString(order(dfClusteredOnVolume$centers))

dfClusterOnVolume$cluster <- dfClusteredOnVolume$cluster
dfMerged$cluster <- dfClusterOnVolume$cluster

names(dfMerged)[67] <- paste("Cluster - Sessions - Prio:", prio)


# -----------------------------------------------------------------------------
# MACHINE LEARNING - UNSUPERVISED - (SUB)CLUSTERING BASED ON TEXT PATTERNS & WEIGHTS
# -----------------------------------------------------------------------------

dfLPPALL <- dfMerged %>% 
  dplyr::select(id, landingPagePath, `Cluster - Sessions - Prio: 3, 8, 2, 4, 7, 1, 6, 5`) %>%
  filter(`Cluster - Sessions - Prio: 3, 8, 2, 4, 7, 1, 6, 5`==8)

dfLPPAll <- gsub("[?&/=]"," ", dfLPPALL$landingPagePath)

vLPP <- VectorSource(dfLPPAll)
cLPP <- Corpus(vLPP)

cLPP <- tm_map(cLPP, content_transformer(tolower))
cLPP <- tm_map(cLPP, removePunctuation)
cLPP <- tm_map(cLPP, stripWhitespace)
cLPP <- tm_map(cLPP, removeNumbers)
cLPP <- tm_map(cLPP, removeWords, stopwords("dutch"))
cLPP <- tm_map(cLPP, stemDocument, language = "dutch")

dtm <- DocumentTermMatrix(cLPP)
mdtm <- as.matrix(dtm)

frequency <- colSums(mdtm)
frequency <- sort(frequency, decreasing = TRUE)

words <- names(frequency)
wordcloud(words[1:50],frequency[1:50])

barplot(frequency[1:50], las = 2, 
        names.arg = words[1:50],
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

findAssocs(dtm, terms = "webwinkel", corlimit = 0.2)
findAssocs(dtm, terms = "productsearch", corlimit = 0.2)
findAssocs(dtm, terms = "rows", corlimit = 0.2)


# -----------------------------------------------------------------------------
# MACHINE LEARNING - UNSUPERVISED - LDA & TOPIC modeling
# https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation
# of via kmeans van victor?
# https://journal.r-project.org/archive/2013/RJ-2013-001/RJ-2013-001.pdf
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# MACHINE LEARNING - UNSUPERVISED - CLUSTERING BASED ON URL STRUCTURE
#cluster on structural info (url)
#blog, product, ... > labeling needed > hoeveel samples nodig om echt goed te zijn?
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# MACHINE LEARNING - UNSUPERVISED - CLUSTERING BASED ON CONTEXT (biz/strategy/entity)
# lijst nodig met labeling op entiteiten ???  bevat woord => is label x (multi-labeling)
# -----------------------------------------------------------------------------



# ------------------------------------------------------------------------
# VISUALIZATION
# ------------------------------------------------------------------------

#plot(dfMerged$exitRate,dfMerged$sessions)
#plot(dfMerged$bounceRate,dfMerged$sessions)


# ------------------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------------------

write.xlsx(dfMerged, paste(gaClient,"-",gaViewName,"-SE0-Pages-",gaDateRange[1],"-",gaDateRange[2],".xls", sep=""))

