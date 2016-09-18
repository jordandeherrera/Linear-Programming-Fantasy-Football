library(Rglpk)

#################################
##
## LOAD DATA
##
#################################

final <- read.csv(file="new.csv",row.names=NULL)

final$lower <- as.numeric(as.character(final$lower))
final$vor <- as.numeric(as.character(final$vor))

final <- final[!is.na(final$lower),]
final <- final[!is.na(final$vor),]

# number of variables
num.players <- length(final$player)
# objective:
obj <- final$vor
# the vars are represented as booleans
var.types <- rep("B", num.players)
# the constraints
matrix <- rbind(as.numeric(final$team == "QB"), # num QB
                as.numeric(final$team == "RB"), # num RB
                as.numeric(final$team == "RB"), # num RB
                as.numeric(final$team == "WR"), # num WR
                as.numeric(final$team == "WR"), # num WR
                as.numeric(final$team == "TE"), # num TE
                as.numeric(final$team == "TE"), # num TE
                as.numeric(final$team %in% c("RB", "WR", "TE")),  # Num RB/WR/TE
                diag(final$lower)
                )         # player's risk
direction <- c("==",
               ">=",
               "<=",
               ">=",
               "<=",
               ">=",
               "<=",
               "==",
               rep("<=", num.players)
              )
rhs <- c(1, # Quartbacks
         2, # RB Min
         3, # RB Max
         3, # WR Min
         4, # WR Max
         1, # TE Min
         2, # TE Max
         7, # RB/WR/TE
         rep(5, num.players)) #HERE, you need to enter a number that indicates how
         #risk you are willing to be, 1 being low risk,
         # 10 being high risk.  10 is max.

sol <- Rglpk_solve_LP(obj = obj, mat = matrix, dir = direction, rhs = rhs,
                      types = var.types, max = TRUE)

finalSelection <- final[sol$solution == 1 & !final$playername %in% c("DST","K"),]