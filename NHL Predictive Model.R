#to do, pull vegas closing lines and game results


library(dplyr)
library(plyr)
library(ggplot2)
library(caret)
library(e1071)

#Line History
LineHistory <- read.csv("Line History.csv")

AllEncompass = read.csv("AllEncompassing.csv")
summary(AllEncompass)

#Goalie Data, have to switch directories to the player stats dir
CareerGoalie = read.csv("CareerGoalie.csv")
summary(CareerGoalie)
head(CareerGoalie)

GoaliebySeason = read.csv("GoalieBySeason.csv")
summary(GoaliebySeason)

#merge goalie and team
GoalieJoin <- merge(x=AllEncompass, y=GoaliebySeason, by=c("Team", "season"))
summary(GoalieJoin)
edit(GoalieJoin)

#merge goalie with career stats
GCJoin <- merge(x=GoalieJoin, y=CareerGoalie, by="Name")
summary(GCJoin)
edit(GCJoin)
#positive diff means above career averages
GCJoin$LowSaveDiff <- GCJoin$Sv.L.x - GCJoin$Sv.L.y
GCJoin$MidSaveDiff <- GCJoin$Sv.M.x - GCJoin$Sv.M.y
GCJoin$HighSaveDiff <- GCJoin$Sv.H.x - GCJoin$Sv.H.y
GCJoin$SvPctDiff <- GCJoin$Sv..x - GCJoin$Sv..y
GCJoin$AdSvDiff <- GCJoin$AdSv..x - GCJoin$AdSv..y

ggplot(data=GCJoin, aes(x=HSCDiff, y=AdSvDiff)) + geom_point() + geom_smooth(method="lm")



#Create a goalie independent metric for a team, (the number of high/medium/low scoring changes they give up),
#then add on their goalies average save percentages

head(AllEncompass)
edit(AllEncompass)


AllEncompass <- rename(AllEncompass, c("G..." = "GoalDiff", "CF."="CorsPerc", "CP60"="Cors60", "OFOn."="UnblockSAPerc","OSv."="OnIceSavePerc", "FO."="FOPerc","ZSO."="ZSOPerc", "HSCF."="HiScoChanPerc", "HSC..."="HSCDiff"))
AllEncompass <- rename(AllEncompass, c("SCF."="SCPerc", "SC..."="SCDiff", "FF."="FenwPerc", "F..."="FenwDiff"))
AllEncompass <- rename(AllEncompass, c("PN." = "PenDrawn", "PenD"="PenDiff", "HIT."="HitTaken", "FO..1"="FOPerc"))
AllEncompass <- rename(AllEncompass, c("C..."="CorsDiff", "SF."="SGoalPerc", "S..."="ShotsOnGoalDiff", "GF."="GoalsFOrPerc", "OSh."="ShootPerc", "OCOn."="AttemptsOnGoalPerc", "OFenSh."="UnBlockShotPerc"))

AllEncompass[,c("CF..1", "CP60.1", "OSh..1", "FOPerc.1", "OSv..1", "OFOn..1")] <- list(NULL)


#Visualizations
ggplot(data=AllEncompass, aes(x=FenwPerc, y=GoalDiff)) + geom_point() +geom_smooth(method="lm")
ggplot(data=AllEncompass, aes(x=CorsPerc, y=GoalDiff, color=ShotsOnGoalDiff)) + geom_point()
ggplot(data=AllEncompass, aes(x=HSCDiff, y=GoalDiff)) + geom_point() + geom_smooth(method="lm")
ggplot(data=AllEncompass, aes(x=SCP60, y=GoalDiff)) + geom_point() + geom_smooth(method="lm")
ggplot(data=AllEncompass, aes(x=PenDiff, y=GoalDiff)) + geom_point() + geom_smooth(method="lm")
ggplot(data=AllEncompass, aes(x=PDO, y=GoalDiff)) + geom_point() + geom_smooth(method="lm")

ggplot(data=AllEncompass, aes(x=season, y=GF)) + geom_bar()

#Scoring by year
AllEncompass %>% group_by(season)

#Sorting Season
AllEncompass %>% filter(season == 20142015) %>% sort(by="GoalDiff")

#PDO, stands for Players on ice shooting percentage and on ice save percentage
summary(AllEncompass$PDO)
AllEncompass %>% filter(PDO >= 101.5)

#splitting the data
set.seed(123)
train <- createDataPartition(AllEncompass, p = .8, list=FALSE, time=1)
newtrain <- AllEncompass[train,]
test <- AllEncompass[-train,]

#Log Reg
library(glmnet)
library(glm2)
lreg <- glm(GF ~HSCDiff+Cors60+FP60, data=AllEncompass, family=poisson(link=log))
summary(lreg)

#Random Forest
library(randomForest)
RandomForest = randomForest(GoalDiff ~ HSCDiff+Cors60+ZSO+ZSN+ZSD+SCP60+PenDiff+FP60, data=AllEncompass,distribution="gaussian", n.trees=1000, shrinkage=0.05, interaction.depth=10, cv.folds=10)
summary(RandomForest)

#Boosting
library(gbm)


#Save percentage versus their average goalies save percentage

