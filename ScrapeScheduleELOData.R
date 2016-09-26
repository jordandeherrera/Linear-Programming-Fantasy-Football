## Regular Expressions Examples

## Uses default regex function as well as some stringr functions
## browseVignettes(package = "stringr")

## Tools for understanding and getting started with Regular Expressions
# Reference sheet:  http://www.regular-expressions.info/reference.html
# Interactively test regular expressions:  http://gskinner.com/RegExr/
# Tool to build regular expressions for you:  http://www.txt2re.com/

##############################
##
## LOAD PACKAGES
##
##############################

## Packages

packages <- c(
  "stringr"
  ,"dplyr"
  ,"htmltab"
  ,"tidyr"
  ,"reshape"
)

.loadPackages <- function(p) {require(p, character.only=TRUE, lib.loc = .libPaths())}

lapply(packages, .loadPackages)


############################
##
## GET AND CLEAN DATA
##
############################

url <- "http://projects.fivethirtyeight.com/2016-nfl-predictions/"
xp2 <- "//td[text() = 'AFC West']/ancestor::table"
fbELO <- htmltab(doc = url, which = xp2, header = 0, encoding = "UTF-8", colNames = c("elo", "change","team","div","wins","losses",
                                                                                      "pointdiff","playoffs","windiv","bye","sb"))
fbELO <- fbELO[,c(1,3)]
colnames(fbELO) <- c("elo","team")

## Regular Expression to Clean Team Names and Separate Name from Record

# Regular Expression for Use in Text Extraction
# Locate team record and take all characters other than
# Record
regex = "[0-9]+[-.][0-9]+"

# Replace existing teams with records with just the team name
fbELO$team <- unlist(strsplit(fbELO$team, regex))

## Match and replace names with abbreviations
# Read in city data
cities <- read.csv("TeamAbbreviations.csv",stringsAsFactors = F)

# Match and replace
fbELO <- inner_join(fbELO,cities,by=c("team" = "City"))
fbELO$team <- fbELO$Abbreviation

# Eliminate extraneous columns
fbELO <- select(fbELO, elo,team)

############################
##
## SCHEDULE DATA
##
############################

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

fbELO$Team <- fbELO$team
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
