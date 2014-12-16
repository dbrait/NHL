#NHL Scraper

install.packages("nhlscrapr")
library(nhlscrapr)

#PLace the full database in a variable
all.games <- full.game.database()

#To create and select a subset of games
these.games = subset(all.games, season == 20132014 & gcode > 21200 & gcode <= 21230)
compile.all.games(new.game.table=these.games)

#Write files to csv
write.cvs(games, "nhlgames.csv")