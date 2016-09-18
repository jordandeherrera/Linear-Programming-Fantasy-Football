library(htmltab)
library(tidyr)
library(reshape)

############################
##
## SAMPLE 
##
############################

url <- "http://en.wikipedia.org/wiki/Demography_of_the_United_Kingdom"
ukRel <- htmltab(doc = url, which = "//th[text() = 'Religion']/ancestor::table")

# Clean Table
ukRel <- gather(ukRel, key, value, -Religion)
ukRel <- separate(ukRel, key, into = c("year", "statistic"), sep = " >> ")

url <- "http://projects.fivethirtyeight.com/2016-nfl-predictions/"
xp2 <- "//td[text() = '1643']/ancestor::table"
fbELO <- htmltab(doc = url, which = xp2, header = 0, encoding = "UTF-8", colNames = c("elo", "change","team","div","wins","losses",
                                                                                      "pointdiff","playoffs","windiv","bye","sb"))
fbELO <- fbELO[,c(1,3)]
colnames(fbELO) <- c("elo","team")
#fbELO$team <- gsub("0-1","",fbELO$team)
#fbELO$team <- gsub("1-0","",fbELO$team)
#fbELO$team <- gsub("1-1","",fbELO$team)
#fbELO$team <- gsub("0-2","",fbELO$team)
#fbELO$team <- gsub("2-0","",fbELO$team)

teamAbb <- data.frame(Long = fbELO$team, Abb = c("DEN","CAR","SEA","PIT","NE",
                                                 "KC","ARI","GB","MIN","CIN",
                                                 "HOU","NYJ","DET","BAL","PHI","NYG",
                                                 "BUF","IND","OAK","DAL","SF","ATL",
                                                 "LA","WSH","NO","TB","SD","CHI","MIA",
                                                 "JAX","CLE","TEN"))

fbELO$team <- teamAbb$Abb

url <- "http://www.espn.com/nfl/schedulegrid"
xp2 <- "//td[text() = 'TEAM']/ancestor::table"
fbSchedule <- htmltab(doc = url, which = xp2, header = 0, encoding = "UTF-8", 
                      colNames = c("team", 1:17))
fbSchedule <- t(gsub("@","",as.matrix(fbSchedule[3:34,])))
colnames(fbSchedule) <- fbSchedule[1,]
fbSchedule <- fbSchedule[2:18,]
newcol <- data.frame(Week = 1:17)
fbSchedule <- cbind(newcol, fbSchedule)

fbSchedule <- melt(fbSchedule, id=c("Week"))
colnames(fbSchedule) <- c("Week","Team","Opponent")

# Prepare data for matching
fbSchedule$Team <- as.character(fbSchedule$Team)
fbSchedule$Opponent <- as.character(fbSchedule$Opponent)
fbELO$Team <- as.character(fbELO$team)
fbELO <- fbELO[,-2]
fbELO <- rbind(fbELO, data.frame(elo=0,Team="BYE"))

fbSchedule$TeamELO <- as.numeric(fbELO[match(fbSchedule$Team,
                                             fbELO$Team,nomatch=0),c("elo")])

fbSchedule$OppELO <- as.numeric(fbELO[match(fbSchedule$Opponent,
                                             fbELO$Team,nomatch=0),c("elo")])

#Calculate ELO difference
fbSchedule$ELODiff <- ifelse(fbSchedule$Opponent == "BYE",0,
                             fbSchedule$TeamELO - fbSchedule$OppELO)

#Write results to CSV
write.csv(fbSchedule,file="schedule.csv")