#NHL Team Analysis

allstats = read.csv("allstats.csv")

allstats$TeamGoalDiff60 = allstats$TeamGoalFor60 - allstats$TeamGoalAgn60
allstats$FenwickDiff60 = allstats$FenwickFor60 - allstats$FenwickAgn60
allstats$CorsiDiff60 = allstats$CorsiFor60 - allstats$CorsiAgn60
allstats$TeamShotDiff60 = allstats$TeamShotFor60 - allstats$TeamShotAgn60

#Splitting data set into training and test sets
library(caret)
set.seed(123)
train = createDataPartition(allstats$TeamGoalDiff60, p = .8, list = FALSE, times = 1)
allstatstrain = allstats[train,]
allstatstest = allstats[-train,]

#Exploratory Data Analysis
library(ggplot2)
ggplot(data=allstats, aes(x=FenwickDiff60, y=TeamGoalDiff60)) + geom_point() +geom_smooth(method="lm")
ggplot(data=allstats, aes(x=CorsiDiff60, y=TeamGoalDiff60, color=TeamShotDiff60)) + geom_point()

#Linreg
LinReg = lm(TeamGoalDiff60 ~ PDO + Corsi. + Off.ZS.. + Fenwick. + TeamShotDiff60 + Faceoff.. + OnIceSh., data=allstats)
summary(LinReg)
plot(LinReg)

#Random Forests
library(randomForest)
RandomForest = randomForest(TeamGoalDiff60 ~ PDO + Corsi. + Off.ZS.. + Fenwick. + TeamShotDiff60 + Faceoff.. + OnIceSh., data=allstats)
summary(RandomForest)
RandomForest
plot(RandomForest)

#Boost
library(gbm)
Boosting = gbm(TeamGoalDiff60 ~ PDO + Corsi. + Off.ZS.. + Fenwick. + TeamShotDiff60 + Faceoff.. + OnIceSh., distribution = "gaussian", n.trees=1000, shrinkage= 0.001, data=allstats, cv.folds=10)
BoostingMinusPDO = gbm(TeamGoalDiff60 ~ Corsi. + Off.ZS.. + Fenwick. + TeamShotDiff60 + Faceoff.. + OnIceSh., distribution = "gaussian", n.trees=10000, shrinkage = 0.001, cv.folds=10, interaction.depth=5, data=allstats)
summary(Boosting)
Boosting
BoostingMinusPDO
summary(BoostingMinusPDO)
plot(BoostingMinusPDO, i="Fenwick.")
plot(BoostingMinusPDO, i="Corsi.")

#Predict function
pred.boost = predict(BoostingMinusPDO, newdata=allstatstest, n.trees=1000)

#Write to CSv file
write.csv(pred.boost, "Pred.Boost.csv")
  