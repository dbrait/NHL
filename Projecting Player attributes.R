library(plyr)
library(dplyr)
library(ggplot2)
library(gbm)
library(randomForest)


Player = read.csv("Player Stats.csv")
summary(Player)


Player <- rename(Player, Corsi_For_Perc=CF., Personal_Shoot_Perc=PSh., ZS_Perc_Relative=ZSO.Rel, HSCDiff=HSC...,
                 SCDiff=SC..., CorsiDiff=C..., FenwickDiff=F..., ShotDiff=S..., GoalDiff=G..., Corsi_For_Teammates_Perc=tCF60,
                  Corsi_Against_Per60_Teammates=tCA60, Corsi_For_Per60_ofComp=cCF60, Corsi_Against_Per60_ofComp=cCA60 )

Player <- Player %>% select(-AAV, -Salary, -CF, -CA, -BK, -FF, -FA, -SF, -SA, -GF, -GA, -A1, -A2, -GV, -SH, -TK, -PN, -PN., -HIT, 
                            -HIT., -ZSO, -ZSN, -ZSD, -MS, -iFF, -iSF, -PenD, -P, -G, -A, -AB, iCF, -FO_W, -FO_L, -iHSC, -HSCF, -HSCA, -SCF, -SCA, -iSC)

Player <- Player %>% mutate(GD60 = GF60 - GA60)

#Exploration

Player %>% filter(HSCF60 > 30) %>% select(Name, HSCF60)

EliteOffense <- Player %>% filter(HSCF.Rel > 7.1)

Player %>% filter(HSCA60 > 17)

#Penalty differential
Player %>% filter(PenD60 > 0.3) %>% filter(TOI > 780) %>% select(Name, PenD60)

Player <- Player %>% filter(Gm > 30)


#Targeting GD60
Player %>% filter(GD60 > 1.5) %>% select(Name, season, GD60)


#Mapping relationships

ggplot(aes(x=Corsi_For_Perc, y=HSCF.Rel))+geom_point()+geom_smooth(method="lm")
