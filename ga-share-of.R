# ------------------------------------------------------------------------
# LINEAR REGRESSION TEMPLATE
#
# R Statistics & Data Science related scripts for Marketing Analytics purposes
# Language: English
# Code by Dries Bultynck
# Questions, bugs & requests: dries@driesbultynck.be or dries@parafix.io
# References: 
# 
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
library("install.load")
install_load("devtools","googleAuthR","googleAnalyticsR","ggplot2","ggthemes","ggrepel","scales","plyr","lubridate","reshape2","TTR","forecast","tidyr","tidyverse","dlookr","CausalImpact","dplyr","formatR","corrplot","Hmisc")

# ------------------------------------------------------------------------
# Import Google Analytics data
# ------------------------------------------------------------------------

ga_auth(new_user = TRUE)
#meta <- google_analytics_meta()

gaViewId <- "107861182"
gaDateRange <- c("2018-01-01","2018-12-22")
gaDimensions <- c("channelGrouping") #date, month, year,"deviceCategory", sourceMedium
gaMetrics <- c("sessions","users","transactions","transactionRevenue")
#gaDelta <- order_type("date","ASCENDING", "DELTA")
#gaDimFilterUsertype <- dim_filter("userType","REGEXP","returning")
#gaDimFilters <- filter_clause_ga4(list(gaDimFilterUsertype))

#get data
gaData <- google_analytics(gaViewId, date_range = gaDateRange, metrics = gaMetrics, dimensions = gaDimensions, anti_sample = TRUE)

# ------------------------------------------------------------------------
# Transform data
# ------------------------------------------------------------------------

gaDataCalc = gaData %>%
  mutate(
    session_share = sessions / sum(sessions),
    trans_share = transactions / sum(transactions),
    revenue_share = transactionRevenue / sum(transactionRevenue)
  ) %>%
  arrange(-session_share) %>%
  transmute(
    channel = channelGrouping,
    sessions,
    users,
    transactions,
    revenue = transactionRevenue,
    session_share,
    session_cum = cumsum(session_share),
    trans_share,
    trans_cum = cumsum(trans_share),
    revenue_share,
    revenue_cum = cumsum(revenue_share),
    tps = transactions / sessions,
    tpu = transactions / users,
    rps = revenue / sessions,
    rpu = revenue / users
  )

# ------------------------------------------------------------------------
# Visualize data
# ------------------------------------------------------------------------

lmGraph <- lm(session_share ~ revenue_share,gaDataCalc)
#summary(lmGraph)
#https://drsimonj.svbtle.com/visualising-residuals
#predict(lmGraph)
#residuals(lmGraph)
correlation <- cor(gaDataCalc$session_share,gaDataCalc$revenue_share)
#cor.test(gaDataCalc$session_share,gaDataCalc$revenue_share, method = "pearson")

gaDataCalc %>%
  filter(transactions >= 1) %>%
  ggplot(
    aes(
      x = session_share,
      y = revenue_share,
      color = channel
    )
  ) +
  geom_vline(xintercept = 0.2, alpha=3/10) +
  geom_hline(yintercept = 0.2, alpha=3/10) +
  geom_abline(intercept=lmGraph$coefficients[1] , slope=lmGraph$coefficients[2], alpha=5/10, col="blue") +
  annotate("text", label=paste0("R=",round(correlation,3)), x = 0.4, y = 0.4, colour = "blue") +
  geom_point(alpha = 7/7) +
  theme_minimal(base_family = "Helvetica Neue") +
  theme(legend.position = "none") +
  scale_x_continuous(name = "Share of sessions", limits = c(0, NA), labels = percent) +
  scale_y_continuous(name = "Share of revenue", limits = c(0, NA), labels = percent) +
  scale_color_few(name = "Channel") +
  scale_fill_few() +
  ggtitle(
    "Sessions and revenue distribution for top channels",
    subtitle = "Based on the Google Analytics data"
  ) +
  geom_label_repel(alpha = 7/7, aes(label=channel), show.legend = F)


lmGraph <- lm(rpu ~ session_share,gaDataCalc)
correlation <- cor(gaDataCalc$session_share,gaDataCalc$rpu)

gaDataCalc %>%
  filter(transactions >= 1) %>%
  ggplot(
    aes(
      x = session_cum,
      y = rpu,
      color = channel
    )
  ) +
  geom_vline(xintercept = 0.2, alpha=3/10) +
  geom_hline(yintercept = 0.2, alpha=3/10) +
  geom_abline(intercept=lmGraph$coefficients[1] , slope=lmGraph$coefficients[2], alpha=5/10, col="blue") +
  annotate("text", label=paste0("R=",round(correlation,3)), x = 0.4, y = 0.4, colour = "blue") +
  geom_point(alpha = 7/7) +
  theme_minimal(base_family = "Helvetica Neue") +
  theme(legend.position = "none") +
  scale_x_continuous(name = "Cumul of sessions", limits = c(0, NA), labels = percent) +
  scale_y_continuous(name = "Revenue per user", limits = c(0, NA), labels = percent) +
  scale_color_few(name = "Channel") +
  scale_fill_few() +
  ggtitle(
    "Cumul of Sessions and revenue per user distribution for top channels",
    subtitle = "Based on the Google Analytics data"
  ) +
  geom_label_repel(alpha = 7/7, aes(label=channel), show.legend = F)
