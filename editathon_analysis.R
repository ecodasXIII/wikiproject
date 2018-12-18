#### Script to pull in and make sense of EcoDAS Wiki edit-a-thon survey data ####

# install.packages('pacman')
pacman::p_load(maps, tidyverse)

raw <- read_csv('./data/WikiProject_hackathon_qualtricsData.csv') # Survey responses
key <- read_csv('./data/questionKey.csv') # Question key


# Participant locations ####
lat <- c(43.07299805,33.93209839,42.728302,45.66810608,-31.99519348,38.95239258,
         37.03129578,38.55380249,44.01849365,40.58529663,26.3677063,34.72529602,
         61.20599365,46.73129272,40.59860229)
long <- c(-89.45279694,-83.35250092,-84.48819733,-111.2404022,115.5404053,
          -76.49099731,-122.1197968,-121.7417984,-123.0998001,-105.0843964,
          127.8746033,-76.75309753, -149.8101044,-117.1795959,-105.0580978)

par(mar = c(0,0,0,0),mgp=c(0,0,0))
maps::map("world", col='grey90', #xlim=c(-180,-65), ylim=c(19,72), 
          fill=T, border='grey50', wrap=T,lwd=0.3) # background USA map
points(long, lat,col='black', pch=16, cex=1,lwd=1)