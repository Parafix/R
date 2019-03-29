# ------------------------------------------------------------------------
# KWR TOPIC MODELING & CLUSTERING
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
# http://www.rpubs.com/MNidhi/NumberoftopicsLDA
# https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to-cluster-analysis-using-r/
# https://rpubs.com/williamsurles/316682
# http://uc-r.github.io/creating-text-features
# https://stackoverflow.com/questions/15547838/how-do-i-simulate-the-bag-of-words-model-in-r-to-fit-into-the-svm
# https://cran.r-project.org/web/packages/corpus/vignettes/stemmer.html > zeker checken
# ------------------------------------------------------------------------

# topics modeling op grote set > niet goed of duidelijk onderscheidend genoeg
# hierarchisch clusteren op grote set > niet goed
# eerst op volume clusteren en dan topics? 
# testen met sentiment om te clusteren in symptoom/probleem vs oplossing
# eerste clustering haalt de grote items eruit, daarna verfijnen in nieuwe dataframe excl. grote clusters
# spelen met stemming -> wel of niet in clustering
# brand related items + producten > closer to sales search persona? 
# alle andere keywords meer gerelateerd aan persona die we nieuw willen verkrijgen op de site
# search personas
# gewicht op swing & aantal keer woord

# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","tidyverse","scales","plyr","lubridate","reshape2","tidyr","dplyr","formatR","openxlsx","readxl")
install_load("TTR","forecast","CausalImpact","formatR","corrplot","Hmisc","fpc","cluster","tm","SnowballC","wordcloud","topicmodels","ldatuning")

# ------------------------------------------------------------------------
# VARIABLES & SETTINGS
# ------------------------------------------------------------------------
#set writing directory
setwd('/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/')
#rm(list=ls())
#dev.off()

gaClient <- "AVogel"
gaViewName <- ""
gaViewId <- ""
gaDateRange <- c("2018-01-01","2018-12-31")

#Set parameters for Gibbs sampling
burnin <- 4000 #skip possible limited distribution proportions
iter <- 2000 #iterations
thin <- 500 #per 500 iterations, take sample for dataset
seed <-list(2003,5,63,100001,765)
nstart <- 5 #different starting points in dataset
best <- TRUE

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
# IMPORT DATA
# ------------------------------------------------------------------------

#inlezen kwr data
dfKWR = read_excel("kwr-biohorma.xls",col_names = TRUE, col_types = NULL, na = "", skip = 0)


# ------------------------------------------------------------------------
# DATA HANDLING
# ------------------------------------------------------------------------

dfKWRTemp <- dfKWR
dfKWRTemp[is.na(dfKWRTemp)] <- 0
dfKWRTemp[dfKWRTemp=="Y"]<-1

# ------------------------------------------------------------------------
# TEXT MODELING
# ------------------------------------------------------------------------

vKWR <- VectorSource(dfKWRTemp$Keyword)
cKWR <- Corpus(vKWR)

# toSpace <- content_transformer(function(x, pattern){return(gsub(pattern," ", x))})
# cKWR <- tm_map(cKWR, toSpace, "-")
# cKWR <- tm_map(cKWR, toSpace, "???")
# cKWR <- tm_map(cKWR, toSpace, "???")
# cKWR <- tm_map(cKWR, toSpace, "???")
# cKWR <- tm_map(cKWR, toSpace, "???")
# cKWR <- tm_map(cKWR, toSpace, "???")

cKWR <- tm_map(cKWR, content_transformer(tolower))
cKWR <- tm_map(cKWR, removePunctuation)
cKWR <- tm_map(cKWR, stripWhitespace)
cKWR <- tm_map(cKWR, removeNumbers)
cKWR <- tm_map(cKWR, removeWords, stopwords("dutch"))
#cKWR <- tm_map(cKWR, stemDocument, language = "dutch")

dtm <- DocumentTermMatrix(cKWR)
tdm <- TermDocumentMatrix(cKWR)
mdtm <- as.matrix(dtm)
mtdm <- as.matrix(tdm)

frequency <- colSums(mdtm)
frequency <- sort(frequency, decreasing = TRUE)

words <- names(frequency)
wordcloud(words[1:50],frequency[1:50],col=palette_wijs)
barplot(frequency[1:50], las = 2, names.arg = words[1:50], col = palette_wijs, main ="Most frequent words", ylab = "Word frequencies")

# ------------------------------------------------------------------------
# SENTIMENT CLUSTERING
# ------------------------------------------------------------------------



# ------------------------------------------------------------------------
# LDA TOPIC MODELING
# ------------------------------------------------------------------------
system.time({
  tunes <- FindTopicsNumber(
    dtm = dtm,
    topics = c(2:15),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
    method = "Gibbs",
    control = list(seed = 12345),
    mc.cores = 4L,
    verbose = TRUE
  )
})

FindTopicsNumber_plot(tunes)

k <- 3 #number of topics

mLDA <- LDA(dtm,k,method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
mLDA.topics <- as.matrix(topics(mLDA))
mLDA.terms <- as.matrix(terms(mLDA,dtm$nrow))
mLDA.topicprob <- as.data.frame(mLDA@gamma)

topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(mLDA.topicprob[x,])[k]/sort(mLDA.topicprob[x,])[k-1])

topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(mLDA.topicprob[x,])[k-1]/sort(mLDA.topicprob[x,])[k-2])


# ------------------------------------------------------------------------
# HIERARCHICAL CLUSTERING (text)
# ------------------------------------------------------------------------
d <- dist(mtdm)
groups <- hclust(d,method="ward.D")
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
rect.hclust(groups,10)

groups$labels

# ------------------------------------------------------------------------
# K-MEANS CLUSTERING (text/volume)
# ------------------------------------------------------------------------
d <- dist(mtdm)
kfit <- kmeans(d,5, nstart=100)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)



# ------------------------------------------------------------------------
# WORD2VEC (context)
# ------------------------------------------------------------------------



# ------------------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------------------

wordcloud(words[1:50],frequency[1:50],col=palette_wijs)
dev.copy(png, paste(gaClient,"-KWR-Wordcloud",".png", sep=""),width=1024,height=768,res=72)
dev.off()

barplot(frequency[1:50], las = 2, names.arg = words[1:50], col = palette_wijs, main ="Most frequent words", ylab = "Word frequencies")
dev.copy(png, paste(gaClient,"-KWR-Word-Barplot",".png", sep=""),width=2048,height=1536,res=72)
dev.off()
