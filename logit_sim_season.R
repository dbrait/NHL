library(dplyr)
library(e1071)

data <- read.csv("AllEncompassing.csv")

data <- mutate(data, home=1)
data$homeWin <- ifelse(data$home_team_score > data$away_team_score, 1, 0)


#Set up datasets
train = filter(data, game_year %in% c(2008, 2009, 2010, 2011, 2012))
test = filter(data, game_year == 2013)

xtest = test[,9:17]
ytest = test[,18]
xtrain = train[,9:17]
ytrain = train[,18]

#Log reg to get probabilities
mylogit <- glm()
logit_preds <- as.data.frame(predict(mylogit, newdata=xtest, type="response"))
logit_preds$class <- ifelse(logit_preds[,1] >= .5, 1, 0)
logit_preds <- cbind(logit-preds, ytest)
logit_preds$result <- abs(logit_preds[,2] - logit_preds[,3])
logit_preds <- cbind(test, logit_preds)[, c(2:8, 17:22)]
names(logit_probs)[10] = "home_prob"

#Create dataset simulation
num_seasons <- 1000
season_df <- data.frame(matrix(0, nrow=length(unique(logit_probs$home_team)), ncol=num_seasons))
row.names(season_df) <- unique(logit_probs$home_team)
colnames(season_df) = paste("season_", 1:1000, sep="")

match_ids <- unique(logit_probs$match_id)

#Loop through each season
for(i in 1:1000){
  print(paste("season", i))
  
  #Generate random outcomes
  random_outcomes <- runif(length(logit_probs[,1]))
  logit_probs <- cbind(logit_probs, random_outcomes)
  
  #Loop through each match ID
  for (match in match_ids){
    
    #Using random number, assign winner of game to data frame
    if(logit_probs[logit_probs$match_id == match,]$random_outcomes <= logit_probs[logit_probs$match_id == match,]$home_prob){
      #Iterate season data frame for Home Team
      season_df[as.character(logit_probs[logit_probs$match_id == match,]$visit_team), i] = season_df[as.character(logit_probs[logit_probs$match_id==match,]$visit_team), i] + 1
    }
  }
  #Remove random outcomes
  logit_probs <- logit_probs[,-14]
}

#Check out results
means <- apply(season_df, 1, mean)
ses <- apply(season_df, 1, sd)

#Save outcomes
write.csv(season_df,)
write.csv(cbind(mean, ses), )