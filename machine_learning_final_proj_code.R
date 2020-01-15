# data comes as separate csv files for each month of data
# read in multiple csv files, combine into one and write to new csv
rm(list=ls())
library(dplyr)
setwd('C:\\Users\\Evan Generoli\\Downloads\\uber-pickups-in-new-york-city')

apr14 <- read.csv('uber-raw-data-apr14.csv')
may14 <- read.csv('uber-raw-data-may14.csv')
jun14 <- read.csv('uber-raw-data-jun14.csv')
jul14 <- read.csv('uber-raw-data-jul14.csv')
aug14 <- read.csv('uber-raw-data-aug14.csv')
sep14 <- read.csv('uber-raw-data-sep14.csv')

uber_data_14 <- bind_rows(apr14, may14, jun14, jul14, aug14, sep14)
setwd('C:\\Users\\Evan Generoli\\Documents\\Graduate School\\Fall Semester 2018\\Statistical and Machine Learning')
write.csv(uber_data_14, file = 'uber_data_14_combined.csv', row.names=FALSE)


# clear environment and set working directory
rm(list=ls())
setwd('C:\\Users\\Evan Generoli\\Documents\\Graduate School\\Fall Semester 2018\\Statistical and Machine Learning')

# read in & truncate dataset by taking random sample and selecting only latitude and longitude variables
# dataset must be cut down, 30k sample size is about the limit of what the clustering function could handle
library(dplyr)
set.seed(0)
df <- read.csv('uber_data_14_combined.csv') %>% sample_n(size=30000) %>% select(Lat, Lon)

summary(df)
head(df)

# implement clustering algorithm with 5 clusters, don't standardize variables
# metric must be specified as squared euclidean for this be soft k-means/fuzzy C-means
library(cluster)
cluster <- fanny(x=df, k=5, metric='SqEuclidean', stand=F)

membership_probs <- 100 * as.data.frame(cluster$membership)
colnames(membership_probs) <- c('Cluster_1','Cluster_2','Cluster_3','Cluster_4','Cluster_5')
head(membership_probs)

#######################################  create maps of results
# in order to download map from google, 
# an api key must be obtained online through a billing enabled google account
# then api key must be registered

# create dataframe for maps
df_map <- bind_cols(membership_probs, df) %>% na.omit()
df_map %>% head()

# install developer version of ggmap in order to register api key with register_google() function
library(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")

library(ggmap)
register_google(key = 'AIzaSyA-GSv-qhnapRyYXkr-x64qJGg3lMRu0V8')

# get map of nyc
nyc_map <- get_map("New York", zoom = 10)
ggmap(nyc_map)


# overlay spatial heatmap of cluster probabilities on nyc map for each cluster
ggmap(nyc_map, extent = "device") +
  stat_summary_2d(data = df_map, aes(x = Lon, y = Lat, z = Cluster_1), 
                  fun = max , alpha = 0.6, bins = 250) +
  scale_fill_gradient(name = "% Probability", low = "green", high = "red") +
  ggtitle('Cluster 1')

ggmap(nyc_map, extent = "device") +
  stat_summary_2d(data = df_map, aes(x = Lon, y = Lat, z = Cluster_2), 
                  fun = max , alpha = 0.6, bins = 250) +
  scale_fill_gradient(name = "% Probability", low = "green", high = "red") +
  ggtitle('Cluster 2')

ggmap(nyc_map, extent = "device") +
  stat_summary_2d(data = df_map, aes(x = Lon, y = Lat, z = Cluster_3), 
                  fun = max , alpha = 0.6, bins = 250) +
  scale_fill_gradient(name = "% Probability", low = "green", high = "red") +
  ggtitle('Cluster 3')

ggmap(nyc_map, extent = "device") +
  stat_summary_2d(data = df_map, aes(x = Lon, y = Lat, z = Cluster_4), 
                  fun = max , alpha = 0.6, bins = 250) +
  scale_fill_gradient(name = "% Probability", low = "green", high = "red") +
  ggtitle('Cluster 4')

ggmap(nyc_map, extent = "device") +
  stat_summary_2d(data = df_map, aes(x = Lon, y = Lat, z = Cluster_5), 
                  fun = max , alpha = 0.6, bins = 250) +
  scale_fill_gradient(name = "% Probability", low = "green", high = "red") +
  ggtitle('Cluster 5')
