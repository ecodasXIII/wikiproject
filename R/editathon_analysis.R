#### Script to pull in and make sense of EcoDAS Wiki edit-a-thon survey data ####

# install.packages('pacman')
pacman::p_load(tidyverse)

raw <- read_csv('./data/WikiProject_hackathon_qualtricsData.csv') # Survey responses
key <- read_csv('./data/questionKey.csv') # Question key

