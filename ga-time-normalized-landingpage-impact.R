# ------------------------------------------------------------------------
# TIME NORMALIZED LANDINGPAGE IMPACT
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
gaDimensions <- c("date","pagePath")
gaMetrics <- c("uniquePageviews","sessions","newUsers", "transactions","transactionRevenue")
gaDelta <- order_type("date","ASCENDING", "DELTA")
gaDimFilterSourceMedium <- dim_filter("sourceMedium","REGEXP","organic")
gaDimFilters <- filter_clause_ga4(list(gaDimFilterSourceMedium))

minPageviews <- 2
totalPagesSelected <- 50
days_live_range <- 365

# ------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------
install.packages("install.load")
library(install.load)
install_load("devtools","googleAuthR","googleAnalyticsR","tidyverse","knitr","kableExtra","scales","plotly","RColorBrewer","ggthemes")

theme_wijs <- theme(
  plot.title = element_text(colour=c("#1B31CC"),face="bold",size=rel(1.5),hjust = 0.95,vjust=-20),
  plot.subtitle = element_text(colour=c("#1C31CC"),face="plain",size=rel(1.2),hjust = 0.95,vjust=-20),
  plot.caption = element_text(colour=c("#1C31CC"),face="plain",size=rel(1),hjust = 0.95,vjust=165),
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
  legend.title = element_text(colour=c("#1B31CC"),face="plain",size=rel(1.2),hjust = -0.65),
  legend.text = element_text(colour=c("#1C31CC")),
  legend.position = "right",
  legend.box = "vertical"
  #DEECFF lichtblauw
  #1B31CC donkerblauw
  #1C31CC donkerblauw 2
  #FF4040 rood
)
options(scipen=999)  # turn-off scientific notation like 1e+48

colorSet <- brewer.pal(n = 9, name = "Blues")
colorRange <- colorRampPalette(colorSet)
#colorRange <- colorRampPalette(colorSet)(50)

# ------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------

normalize_date_start <- function(page){
  
  # Filter all the data to just be the page being processed
  gaDataSinglePage <- gaData %>% filter(pagePath == page)
  
  # Find the first value in the result that is greater than first_day_pageviews_min. In many
  # cases, this will be the first row, but, if there has been testing/previews before it
  # actually goes live, some noise may sneak in where the page may have been live, technically,
  # but wasn't actually being considered live.
  first_live_row <- min(which(gaDataSinglePage$uniquePageviews > minPageviews))
  
  # If the "first_live_row" is Inf, then none of the traffic for the page on any given day
  # exceeded the first_day_pageviews_min value, so we're going to go ahead and exit the function
  # with a NULL result
  if(first_live_row == Inf){return(NULL)}
  
  # Filter the data to start with that campaign
  gaDataSinglePage <- gaDataSinglePage[first_live_row:nrow(gaDataSinglePage),]
  
  # As the content ages, there may be days that have ZERO traffic. Those days won't show up as
  # rows at all in our data. So, we actually need to create a data frame that includes
  # all dates in the range from the "launch" until the last day traffic was recorded. There's
  # a little trick here where we're going to make a column with a sequence of *dates* (date) and,
  # with a slightly different "seq," a "days_live" that corresponds with each date.
  normalized_results <- data.frame(date = seq.Date(from = min(gaDataSinglePage$date), 
                                                   to = max(gaDataSinglePage$date), 
                                                   by = "day"),
                                   days_live = seq(min(gaDataSinglePage$date):
                                                     max(gaDataSinglePage$date)),
                                   page = page,
                                   stringsAsFactors = FALSE) %>% 
    
    # Join back to the original data to get the sessions
    left_join(gaDataSinglePage) %>%
    
    # Replace the "NAs" (days in the range with no sessions) with 0s (because 
    # that's exactly what happened on those days!)
    mutate(uniquePageviews = ifelse(is.na(uniquePageviews), 0, uniquePageviews)) %>%
    mutate(sessions = ifelse(is.na(sessions), 0, sessions)) %>%
    mutate(newUsers = ifelse(is.na(newUsers), 0, newUsers)) %>%
    mutate(transactions = ifelse(is.na(transactions), 0, transactions)) %>%
    
    # We're going to plot both the daily sessions AND the cumulative total sessions,
    # so let's add the cumulative total
    mutate(cumUniquePageviews = cumsum(uniquePageviews)) %>% 
    mutate(cumSessions = cumsum(sessions)) %>% 
    mutate(cumNewUsers = cumsum(newUsers)) %>% 
    mutate(cumTransactions = cumsum(transactions)) %>% 
    
    # Grab just the columns we need for our visualization!
    dplyr::select(page, days_live, uniquePageviews, cumUniquePageviews, sessions, cumSessions, newUsers, cumNewUsers, transactions, cumTransactions)
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

#filter met regex voor deel van paginas eventueel

page_list <- gaData %>% group_by(pagePath) %>% summarise(total_traffic = sum(uniquePageviews)) %>% top_n(totalPagesSelected)

# The first little bit of magic can now occur. We'll run our normalize_date_start function on
# each value in our list of pages and get a data frame back that has our time-normalized
# traffic by page!
ga_data_normalized <- map_dfr(page_list$pagePath, normalize_date_start)

# set number of days active since launch (for how many days should we get the data)
ga_data_normalized <- ga_data_normalized %>% filter(days_live <= days_live_range)

#ga_data_normalized$color = I(brewer.pal(nlevels(ga_data_normalized$page), name = 'Blues'))

# ------------------------------------------------------------------------
# Visualization
# ------------------------------------------------------------------------

gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = uniquePageviews, color=page)) +
  geom_line() +                                          # The main "plot" operation
  scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
  labs(title = "Unique Pageviews by Day from Launch",
       x = "# of Days Since Page Launched",
       y = "Unique Pageviews") +
  theme_wijs +                                        # Clean up the visualization a bit
  theme(legend.position = "none")

# Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
ggplotly(gg)


gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = cumUniquePageviews, color=page)) +
  geom_line() +                                          # The main "plot" operation
  scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
  labs(title = "Cumulative Unique Pageviews by Day since Launch",
       x = "# of Days Since Page Launched",
       y = "Cumulative Unique Pageviews") +
  theme_wijs +                                        # Clean up the visualization a bit
  theme(legend.position = "none")

# Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
ggplotly(gg)



gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = cumSessions, color=page)) +
  geom_line() +                                          # The main "plot" operation
  scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
  labs(title = "Cumulative Sessions by Day since Launch",
       x = "# of Days Since Campaign Launched",
       y = "Cumulative Sessions") +
  theme_wijs +                                        # Clean up the visualization a bit
  theme(legend.position = "none")

# Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
ggplotly(gg)


gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = cumNewUsers, color=page)) +
  geom_line() +                                          # The main "plot" operation
  scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
  labs(title = "Cumulative New Users by Day since Launch",
       x = "# of Days Since Page Launched",
       y = "Cumulative New Users") +
  theme_wijs +                                        # Clean up the visualization a bit
  theme(legend.position = "none")

# Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
ggplotly(gg)


gg <- ggplot(ga_data_normalized, mapping=aes(x = days_live, y = cumTransactions, color=page)) +
  geom_line() +                                          # The main "plot" operation
  scale_y_continuous(labels=comma) +                     # Include commas in the y-axis numbers
  labs(title = "Cumulative Transactions by Day since Launch",
       x = "# of Days Since Page Launched",
       y = "Cumulative Transactions") +
  theme_wijs +                                        # Clean up the visualization a bit
  theme(legend.position = "none")

# Output the plot. We're wrapping it in ggplotly so we will get some interactivity in the plot.
ggplotly(gg)


# ------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------