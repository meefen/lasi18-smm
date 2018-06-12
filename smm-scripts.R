### R Script for the Social Media Mining in Learning Contexts Tutorial

# 1. Preparation
## 1.1. Set working directory
##      Note: Change the folder to where the file is on your computer
setwd("~/Google Drive/Research/Conferences/LASI2018/lasi18-smm")

## 1.2. Install packages if needed
# install.packages(c("tidyverse", "igraph", "GGally", 
#                    "httr", "jsonlite", "RCurl", "stringr"))
# devtools::install_github("ColumbusCollaboratory/cognizer")

## 1.2. Load all utility functions written by Bodong
source("utils/utils.R", local=TRUE)
source("utils/collect_tweets.R")
source("utils/munge_tweets.R")
source("utils/social_network_analysis.R")
source("utils/collect_annotations.R")
source("utils/watson_analysis.R")

# Load API keys
source("load_keys.R") # you will need to create a file with your own keys

## 1.3. Load R packages
EnsurePackage("tidyverse")

# 2. Collect data

## 2.1. Collect tweets from Google Drive (Sheets)

gsheet_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSLP0cZdiKFRrcZZIXJbxIs_MMpFuIarf2VZ0NT0vrl3tyLrYwdtF3XvHzLBOZhpL_r6CBkv4aXpfz-/pub?gid=400689247&single=true&output=csv"
tweets_raw <- collect_tweets_from_gdrive(gsheet_url)

# basic data cleanup (incl. removing duplicates)
tweets <- preprocess_tweets(tweets_raw)

# basic information
tweets %>% 
  glimpse()

# save cleaned data as a csv file
# write_csv(tweets, path = "data/tweets.csv")
save(tweets, file="data/tweets.Rdata")
# load(url("https://github.com/meefen/lasi18-smm/raw/master/data/tweets.Rdata"))

## 2.2. Collect Hypothesis annotations using API


# Collect annotations
annotations <- collect_annotations(tag = "lasi18smm",
                                   token = h_token)

# basic information
glimpse(annotations)

# save data as an Rdata file
save(annotations, file="data/annotations.Rdata")
# load(url("https://github.com/meefen/lasi18-smm/raw/master/data/annotations.Rdata"))

# 3. Analyze Twitter data

## 3.0. Basic descriptive analysis

# Most active participant
tweets %>% group_by(from_user) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# Most popular participant - retweeted
tweets %>% group_by(retweet_from) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# Most popular participant - replied to
tweets %>% group_by(reply_to) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

## 3.1. Social network analysis

## 3.1.1. Set up

# Create data frames
rt_df <- create_graph_df(tweets, 
                         from = "from_user", 
                         to = "retweet_from", 
                         linkNames = "rt")
rp_df <- create_graph_df(tweets, 
                         from = "from_user", 
                         to = "reply_to", 
                         linkNames = "rp")

# Load packages
EnsurePackage("igraph")
# EnsurePackage("sna")
# EnsurePackage("Matrix")
# EnsurePackage("SparseM")

# Create a network/graph using igraph
g <- graph.data.frame(rt_df, directed = TRUE)

# Plot with igraph (quick and dirty)
plot.igraph(g)

# Plot with ggnet2
EnsurePackage("GGally")
library(GGally)
ggnet2(g, node.size = 2, node.color = "black", edge.size = 1, edge.color = "grey")

## 3.1.2. Connectivity of the community

# network level measures
edge_density(g)
reciprocity(g)
centr_degree(g)$centralization

# modularity
wtc <- cluster_walktrap(g, steps = 1000)
length(wtc)
modularity(wtc)
modularity(g, membership(wtc))

## 3.1.3. Influence and leadership

centr_bt <- igraph::betweenness(g, normalized = TRUE)
centr_bt_df <- data.frame(user = names(centr_bt), 
                          centrality = centr_bt)
centr_bt_df %>% arrange(desc(centrality)) %>% head(10)


# 4. Analyze annotation data

## 4.1. Analyze cognitive and affective states
##      of web annotations
##      using IBM Watson API

# Load the `cognizer` package
library(cognizer)

# check the structure of annotations
glimpse(annotations)

# Tone analysis
# example here: https://tone-analyzer-demo.ng.bluemix.net/
(text <- TrimUrls(annotations$text[1]))
tones <- text_tone(text, userpwd = watson_userpwd)
tones[1][[1]]$document_tone$tone_categories$tones # results

# Keyword extraction
response <- analyze_text_with_watsonNLU(text, username_NLU, password_NLU)
# check results
# response$headers
signal <- content(response)
# keywords
keywords <- signal$keywords
length(keywords)
for(i in 1:length(keywords)) {
  print(paste(keywords[[i]]$text," sentiment:",keywords[[i]]$sentiment$score))
}



## Challenges

# 1. Equity conversations are an emotional journey. 
# How are annotated keywords or entities linked to different sentiments?
# How are annotated keywords or entities linked to different tones?
# To what extent are sentiments and tones transmitted among participants?

# 2. Assuming there is a way to link social media profiles (Twitter, blogs, Hypothesis),
# How are participants using different media for different modes of engagement?
