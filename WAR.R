library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)

WAR

#Create line history CSV

#metric for aging curve

#metric for season performance versus regression

#bayesian look at things (stochastic interval)

#close look at context


TotalWAR <- read.csv("Total WAR.csv")
SeasonalWAR <- read.csv("Seasonal WARs.csv")

PlayerStats <- read.csv("Player Stats.csv")


LineHistory <- read.csv("Line History.csv")

summary(TotalWAR)
summary(SeasonalWAR)

#Running a projection for a sample team
Season1415 <- filter(SeasonalWAR, season == 20142015)
Player1415 <- filter(PlayerStats, season == 20142015)

EDMPlayer1415 <- filter(Player1415, Team == "EDM") %>% select(Name, pos, Team, season, TOI, TOI.)
gsub("\\.", "", EDMPlayer1415$Name, fixed=TRUE)

 SampleTeam <- filter(Season1415, team == "EDM")

TeamProject <- merge(SampleTeam, EDMPlayer1415, by="Name")
