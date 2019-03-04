# ------------------------------------------------------------------------
# TIME NORMALIZED CAMPAIGN IMPACT
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References:
# http://rpubs.com/tgwilson/time-normalized-pageviews
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# VARIABLES & SETTINGS
# ------------------------------------------------------------------------
directory = '/Users/driesbultynck/Desktop/_Dries/_Analytics/_R/'
setwd(directory)

gaViewId <- "2897692" #Avogel Master
gaDateRange <- c("2018-01-01","2018-12-31")
gaDimensions <- c("date","campaign")
gaMetrics <- c("sessions","newUsers", "transactions","transactionRevenue")
gaDelta <- order_type("date","ASCENDING", "DELTA")
gaDimFilterSourceMedium <- dim_filter("sourceMedium","REGEXP","cpc")
gaDimFilters <- filter_clause_ga4(list(gaDimFilterSourceMedium))

minSessions <- 2
totalCampaignsSelected <- 25
days_live_range <- 365



# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library(install.load)
install_load("devtools","googleAuthR","googleAnalyticsR","tidyverse","knitr","kableExtra","scales","plotly")

# ------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------

normalize_date_start <- function(campaign){
  
  # Filter all the data to just be the page being processed
  gaDataSingleCampaign <- gaData %>% filter(campaign == campaign)
  
  # Find the first value in the result that is greater than first_day_pageviews_min. In many
  # cases, this will be the first row, but, if there has been testing/previews before it
  # actually goes live, some noise may sneak in where the page may have been live, technically,
  # but wasn't actually being considered live.
  first_live_row <- min(which(gaDataSingleCampaign$sessions > minSessions))
  
  # If the "first_live_row" is Inf, then none of the traffic for the page on any given day
  # exceeded the first_day_pageviews_min value, so we're going to go ahead and exit the function
  # with a NULL result
  if(first_live_row == Inf){return(NULL)}
  
  # Filter the data to start with that campaign
  gaDataSingleCampaign <- gaDataSingleCampaign[first_live_row:nrow(gaDataSingleCampaign),]
  
  # As the content ages, there may be days that have ZERO traffic. Those days won't show up as
  # rows at all in our data. So, we actually need to create a data frame that includes
  # all dates in the range from the "launch" until the last day traffic was recorded. There's
  # a little trick here where we're going to make a column with a sequence of *dates* (date) and,
  # with a slightly different "seq," a "days_live" that corresponds with each date.
  normalized_results <- data.frame(date = seq.Date(from = min(gaDataSingleCampaign$date), 
                                                   to = max(gaDataSingleCampaign$date), 
                                                   by = "day"),
                                   days_live = seq(min(gaDataSingleCampaign$date):
                                                     max(gaDataSingleCampaign$date)),
                                   campaign = campaign,
                                   stringsAsFactors = FALSE) %>% 
    
    # Join back to the original data to get the sessions
    left_join(gaDataSingleCampaign) %>%
    
    # Replace the "NAs" (days in the range with no sessions) with 0s (because 
    # that's exactly what happened on those days!)
    mutate(sessions = ifelse(is.na(sessions), 0, sessions)) %>%
    mutate(newUsers = ifelse(is.na(newUsers), 0, newUsers)) %>%
    mutate(transactions = ifelse(is.na(transactions), 0, transactions)) %>%
    
    # We're going to plot both the daily sessions AND the cumulative total sessions,
    # so let's add the cumulative total
    mutate(cumSessions = cumsum(sessions)) %>% 
    mutate(cumNewUsers = cumsum(newUsers)) %>% 
    mutate(cumTransactions = cumsum(transactions)) %>% 
    
    # Grab just the columns we need for our visualization!
    dplyr::select(campaign, days_live, sessions, cumSessions, newUsers, cumNewUsers, transactions, cumTransactions)
}


# ------------------------------------------------------------------------
# IMPORT DATA
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)
#meta <- google_analytics_meta()

#get data
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, dim_filters = gaDimFilters, anti_sample = TRUE)

# ------------------------------------------------------------------------
# DATA HANDLING
# ------------------------------------------------------------------------

#deleted (not set)
gaData <- subset(gaData, gaData$campaign!="(not set)")

campaign_list <- gaData %>% group_by(campaign) %>% summarise(total_traffic = sum(sessions)) %>% top_n(totalCampaignsSelected)

# The first little bit of magic can now occur. We'll run our normalize_date_start function on
# each value in our list of pages and get a data frame back that has our time-normalized
# traffic by page!
ga_data_normalized <- map_dfr(campaign_list$campaign, normalize_date_start)

# set number of days active since launch (for how many days should we get the data)
ga_data_normalized <- ga_data_normalized %>% filter(days_live <= days_live_range)


# ------------------------------------------------------------------------
# Visualization
# ------------------------------------------------------------------------

gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = sessions, color=campaign)) +
  geom_line() +                                          # The main "plot" operation
  scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
  labs(title = "Sessions by Day from Launch",
       x = "# of Days Since Campaign Launched",
       y = "Sessions") +
  theme_light() +                                        # Clean up the visualization a bit
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(color = "gray80"),
        axis.ticks = element_blank())

# Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
ggplotly(gg)


gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = cumSessions, color=campaign)) +
  geom_line() +                                          # The main "plot" operation
  scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
  labs(title = "Cumulative Sessions by Day from Launch",
       x = "# of Days Since Campaign Launched",
       y = "Cumulative Sessions") +
  theme_light() +                                        # Clean up the visualization a bit
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(color = "gray80"),
        axis.ticks = element_blank())

# Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
ggplotly(gg)


gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = cumNewUsers, color=campaign)) +
  geom_line() +                                          # The main "plot" operation
  scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
  labs(title = "Cumulative New Users by Day from Launch",
       x = "# of Days Since Campaign Launched",
       y = "Cumulative New Users") +
  theme_light() +                                        # Clean up the visualization a bit
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(color = "gray80"),
        axis.ticks = element_blank())

# Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
ggplotly(gg)



gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = cumTransactions, color=campaign)) +
  geom_line() +                                          # The main "plot" operation
  scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
  labs(title = "Cumulative Transactions by Day from Launch",
       x = "# of Days Since Campaign Launched",
       y = "Cumulative Transactions") +
  theme_light() +                                        # Clean up the visualization a bit
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(color = "gray80"),
        axis.ticks = element_blank())

# Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
ggplotly(gg)


# ------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------