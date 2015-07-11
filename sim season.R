#NHL simulate season
library(dplyr)
library(e1071)
library(caret)
library(hydroGOF)
library(randomForest)
library(gbm)

data <- read.csv("AllEncompassing.csv")
data1 <- read.csv("GoalieBySeason.csv")
data2 <- read.csv("CareerGoalie.csv")

#Rename columns
data <- dplyr::rename(data, Hits=HIT, GDiff=G..., Corsi_For_Perc=CF., Corsi_60=CP60, Unblocked_Shots_Perc=OFOn.,
                      Shooting_Perc=OSh., Save_Perc=OSv., Faceoff_Win_Perc=FO., Zone_Starts_Per=ZSO., High_Danger_Scoring=HSCF.,
                      High_Scoring_Chance_Diff=HSC..., Scoring_Chances_Perc=SCF., Scoring_Chances_Diff=SC..., Corsi_Diff=C...,
                      Fenwick_For_Perc=FF., Fenwick_Diff=F..., Missed_Shots_For=MSF, Missed_Shots_Against=MSA,
                      Shots_For_Perc=SF., Shots_Diff=S..., Goals_For_Perc=GF., Unblocked_Shooting_Perc=OFenSh.,
                      Attempts_on_Goal=OCOn., Hits_Taken=HIT., Penalties=PN, Penalties_Drawn=PN., Pen_Diff=PenD)

data <- select(data, -OFOn..1, -FO..1, -OSv..1, -OSh..1, -CP60.1, -CF..1)


data1 <- dplyr::rename(data1, SV_Perc=Sv., Adjusted_SV_Perc=AdSv., SV_Perc_Low=Sv.L, SV_Perc_Mid=Sv.M, SV_Perc_High=Sv.H,
                       Goals_Low=G.L, Saves_Low=S.L, Goals_Med=G.M, Saves_Med=S.M, Goals_High=G.H, Saves_High=S.H)

data2 <- dplyr::rename(data2, SV_Perc=Sv., Adjusted_SV_Perc=AdSv., SV_Perc_Low=Sv.L, SV_Perc_Mid=Sv.M, SV_Perc_High=Sv.H,
                       Goals_Low=G.L, Saves_Low=S.L, Goals_Med=G.M, Saves_Med=S.M, Goals_High=G.H, Saves_High=S.H)

#Sorting best goalies
data1 %>% filter(Adjusted_SV_Perc > 92) %>% filter(Sh > 1000) %>% select(Name, Team, season, Adjusted_SV_Perc, Sh)
data2 %>% filter(Adjusted_SV_Perc > 92) %>% filter(Sh > 1000) %>% select(Name, Adjusted_SV_Perc, Sh)


#Data Exp
data %>% filter(High_Scoring_Chance_Diff > 71) %>% select(Team, season , GDiff, Scoring_Chances_Diff, Corsi_Diff,High_Scoring_Chance_Diff)

#merge goalies with career numbers
totalGoalie <- merge(data1, data2, by="Name")
#add differential
totalGoalie <- mutate(totalGoalie, Delta_SV_Perc = Adjusted_SV_Perc.x - Adjusted_SV_Perc.y,
                      Delta_High_SV_Perc = SV_Perc_High.x - SV_Perc_High.y,
                      Delta_Mid_SV_Perc = SV_Perc_Mid.x - SV_Perc_Mid.y,
                      Delta_Low_SV_Perc = SV_Perc_Low.x - SV_Perc_Low.y)

#high level top level predictors, divide the teams up (75% and 25% of dataset) and find best predictors of
#goal diff
set.seed(123)
train = filter(data, season %in% c(20082009, 20092010, 20102011, 20112012, 20122013))
test = filter(data, season %in% c(20132014, 20142015)



#GDiff predictors (just used to determine how predictable it is and what matters)

Logit <- glm(GDiff ~ Corsi_60 + Zone_Starts_Per + High_Danger_Scoring + Scoring_Chances_Perc + Shots_For_Perc+
               Pen_Diff + PDO + Faceoff_Win_Perc + Save_Perc + Shooting_Perc,  data=train)
LogitPred <- predict(Logit, test)

RForest <- randomForest(GDiff ~ Corsi_60 + Zone_Starts_Per + High_Danger_Scoring + Scoring_Chances_Perc + Shots_For_Perc+
                          Pen_Diff+PDO+Faceoff_Win_Perc+Save_Perc+Shooting_Perc, data=train, n.trees=5000)
RForestPred <- predict(RForest, test, n.trees=5000)

Boost <- gbm(GDiff ~ Corsi_60 + Zone_Starts_Per + High_Danger_Scoring + Scoring_Chances_Perc + Shots_For_Perc +
               Pen_Diff+PDO+Faceoff_Win_Perc+Save_Perc+Shooting_Perc, data=train, n.tree=2000, interaction.depth=5)
BoostPred <- predict(Boost, test, n.tree=2000)

#Prediction Comparison
PredCheck <- data.frame(LogitPred, test) %>% select(LogitPred, GDiff)
GDiff <- test$GDiff

#RMSE
rmse(LogitPred, GDiff)
rmse(RForestPred, GDiff)
rmse(BoostPred, GDiff)


"""Need to determine what is projectible year to year:
  Early hypothesis: Corsi, interval for players shooting percentage, interval for goalie save percentage,
high scoring chances, shots blocked, penalties taken/drawn, faceoff win percentage

#Set up datasets
years <- c(2008, 2009, 2010, 2011, 2012, 2013)
train <- filter(data, game_year %in% c(2008, 2009, 2010, 2011, 2012))
test <- filter(data, game_year == 2012)


#create dataset for simulation
num_seasons <- 1000
season_df <- data.frame(matrix(0, nrow=length(unique(probs$home_team)), ncol=num_seasons))
row.names(season_df) <- unique(probs$home_team)
colnames(season_df) = paste("season_", 1:1000, sep="")

#unique match IDs
match_ids <- unique(probs$match_id)

#get random number for each row
random_outcomes <- runif(length(probs[,1]))
probs <- cbind(probs, random_outcomes)

#Loop through each season
for(i in 1:1000){
  print(paste("season", i))
  
  #Loops through each match and probability of season
  for(match in match_ids){
    
    #Generate uniform random number
    res <- runif(1)
    
    #Pull relevant game
    game <- probs[probs$match_id ==  match,]
    
    #Using random number, assign winner of the game to data frame
    if(res <= game$away_prob){
      #Iterate season data frame from away team
      season_df[as.character(game$visit_team), i] = season_df[as.character(game$visit_team), i] + 1
    } else{
      #Iterate season data frame for home team
      season_df[as.character(game$home_team), i] = season_df[as.character(game$home_team), i] + 1
    }
  }
}

#Check out results
means <- apply(season_df, 1, mean)
ses <- apply(season_df, 1, sd)

#Write file
write.csv(cbind(means, ses), "sim.csv")