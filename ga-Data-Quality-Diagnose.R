# ------------------------------------------------------------------------
# DIAGNOSE DATA QUALITY
# ------------------------------------------------------------------------

#diagnose_numeric(gaDataCampaignsPerWeek)
#diagnose_category(gaDataCampaignsPerWeek)
#diagnose_outlier(gaDataCampaignsPerWeek)
#plot_outlier(gaDataCampaignsPerWeek)

gaDataCampaignsPerDay %>%
  diagnose_report(output_format = "html", output_file = paste(gaClient,"-",gaViewName,"-Diagnose-",gaDateRange[1],"-",gaDateRange[2], sep=""))