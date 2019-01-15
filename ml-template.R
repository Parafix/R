# ------------------------------------------------------------------------
# Machine Learning TEMPLATE
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References: 
# 
# https://machinelearningmastery.com/pre-process-your-dataset-in-r/
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Variables & settings
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
setwd(directory)

# ------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------
install.packages("install.load")
library(install.load)
install_load("devtools","dlookr","dplyr","tidyverse")

# ------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Data quality assessment
# ------------------------------------------------------------------------
#1 check equal numbers of rows per column
#2 check NA and change to 0 or delete rows

# ------------------------------------------------------------------------
# Prepare data
# ------------------------------------------------------------------------
#1 Scatterplot, Corr. heatmap & pairsplot
#2 check for outliers via Boxplot, cooksdistance or anamaly detection
#3 Dimensionality redcution doen via PCA 
#4 normalize features (feature engineering)

#takes in account outliers
scale() 

#scales all to the mean - no outliers
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dfNorm <- as.data.frame(lapply(df, normalize))

#3 create train, test and validation set

# ------------------------------------------------------------------------
# Build & evaluate model
# ------------------------------------------------------------------------
#1 Split in 70-80% train & 20-30% test data
#2 ALWAYS randomize to balance data
#3 MAE, MSE and R2 check, SSE for K-means + Elbow &/or silhouette 
#4 Visualize model
#5 high-order features (2nd order, 3th order, 4th order, ...)
#6 Regularization : underfitting <> overfitting balance

# ------------------------------------------------------------------------
# Predict
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------

#export to xls
export <- x
write.xlsx(export,"export.xls")

#open directory with export file
opendir(getwd())