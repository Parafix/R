# R Scripts for Marketing Analytics purposes
Statistics & Data Science related scripts
<br>Code by Dries Bultynck
<br>Language: English
<br>Questions, bugs or requests: dries@driesbultynck.be or dries@parafix.io

## Google Analytics - Settings
Script: get an overview of all the accounts, profiles and views with their filters, custom dimensions & custom metrics
<br>Focus: needs some work

## Machine Learning Template
Basic template for machine learning applications and needed/possible steps. Clean sheet.

## Facebook algorithm impact prediction
Script: Predict needed reach based on desired clicks and real historic reach data from past reach campaigns as potential reach indicated by Facebook isn't trust worthy and doesn't allow brands to really invest in market share as they aim to do (TV budget -> GRP build-up vs online budget -> reach build-up).
<br>Model: link clicks models is accurate - first screening: avg. of 6x budget needed to possibly reach the effective potential reach
<br>Focus: done
<br>Technique: Linear regression

## Google Analytics - Seasonality
Script: Check correlation + cross lag impact, decompose trends & seasonality, forecast traffic & show causal impact
<br>Model: accuracy to check
<br>Focus: done

## Google Analytics - Day type corr
Script: Impact of type of day on Revenue - days like: day of week, holiday, closing days, ...
<br>Model: accuracy to check
<br>Focus: done

## Google Analytics - Next Best Content by Page
Script: Market basket analysis based on combined page visits per session (optional: per user-id or client-id)
<br>Model: accuracy to check
<br>Focus: done
<br>Technique: Market basket analysis

## Weather & Google Analytics - Impact of weather
Script: Correlation & prediction of outcome based on weather data & Google Analytics goals
<br>Model: accuracy to check - outliers detection needs to be better -> capping datapoints
<br>Focus: on-going

## Share of ... & Google Analytics - Channel focus
Script: Vizualisation of channels & share of total based on sessions, transactions, revenue and more
<br>Model: no model
<br>Focus: forecasting scenarios with conversion rates etc. or attribution depending on focus

## Channel Attribution & Google Analytics
Script: Basic channel attribution 
<br>Model: no model
<br>Focus: order & export needed to tie to budgets
<br>Technique: Markov

## GIS & Data
Script: Basic density & facet mapping of data
<br>Model: no model
<br>Focus: clean up, more examples, better viz
<br>Technique: none

## Media & Google Analytics
Script: Export & graph of campaign stats per day, week, month, quarter, year ... based on labeling with custom reporting by metrics liek conversion rates, spend/return, weighted values, etc.
<br>Model: no model
<br>Focus: graphs
<br>Technique: none

## Data Quality Diagnoes & Google Analytics
Script: Explore data quality & distribution
<br>Model: no model
<br>Focus: test
<br>Technique: none

## Distribution of Page Depth & Google Analytics
Script: Graph of distribution by pageDepth (converters vs non-converters) to initiate behavioural segmentation
<br>Model: no model
<br>Focus: turn into segments
<br>Technique: k-means? or weighted return on pageDepth?

## Time normalized impact of Campaigns & Google Analytics
Script: Graph cumul impact by campaign over time
<br>Model: no model
<br>Focus: /
<br>Technique: time normalization

## Time normalized impact of landing pages & Google Analytics
Script: Graph cumul impact by landing page over time
<br>Model: no model
<br>Focus: /
<br>Technique: time normalization


# Ideas
- Colour codes for graphs based upon book of leila
- Types of graphs based upon book of leila
- Make markdown of graphs or specific scripts for reporting in pdf 
- Anomaly detection on sessions or days of year over one year to make campaigns accountable or detected spam issues
- Make several machine learning template according to supervised or unsupervised techniques
- K-means clustering on user-id, session-id, urls, ...
- Neural network training with tensorflow + output in xls or api? 
- Pipeline management from input, model, output into working environment/database with R or Python
- Make functions for packages to load, handling data, etc.


# Services 
- Make the general analytical marketeer and/or its reporting obsolete for GA
