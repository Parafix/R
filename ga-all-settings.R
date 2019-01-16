# ------------------------------------------------------------------------
# GET ALL SETTINGS of GA
#
# R Statistics & Data Science for OM
# Language: English
# Code by Dries Bultynck
# Questions & bugs or requests: Dries.bultynck@wijs.be
# ------------------------------------------------------------------------

#rm(list=ls())
setwd('/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/')

install.packages("install.load")
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","scales","plyr","lubridate","reshape2","TTR","forecast","tidyr","CausalImpact","dplyr","formatR","corrplot","Hmisc", "openxlsx")

ga_auth(new_user=TRUE)
#account_list <- ga_account_list()

ga_account = "109136184"
webproperty_list <- ga_webproperty_list(ga_account)$items$id
for(i in webproperty_list){
  webproperty_view_list <- ga_view_list(ga_account,i)$items$id
  ga_custom_var_list <- ga_custom_vars_list(ga_account,i, type = c("customMetrics","customDimensions"))$items
  #print(ga_custom_vars_list(ga_account,i, type = c("customMetrics","customDimensions"))$items)
  #print(ga_view_list(ga_account,i))
  for(j in webproperty_view_list){
    ga_goal_list_list <- ga_goal_list(ga_account,i,j)$items
    ga_filter_view_list_list <- ga_filter_view_list(ga_account,i,j)$items
    #print(ga_goal_list(ga_account,i,j)$items)
    #print(ga_filter_view_list(ga_account,i,j)$items)
  }
}

write.xlsx(webproperty_view_list, file="export-ga-account-details.xlsx", sheetName="Web Properties", row.names=FALSE)
write.xlsx(ga_custom_var_list, file="export-ga-account-details.xlsx", sheetName="Web Properties Custom Variables", append=TRUE, row.names=FALSE)