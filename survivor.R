library(Rglpk)

#################################
##
## LOAD DATA
##
#################################

final <- read.csv(file="schedule.csv",row.names=NULL, stringsAsFactors=F)

teams <- sort(unique(final$Team))

# number of variables
num.picks <- nrow(final)
# objective:
obj <- final$ELODiff
# the vars are represented as booleans
var.types <- rep("B", num.picks)
# the constraints
matrix <- rbind(diag(final$TeamELO),
                diag(final$ELODiff)
                )

for (i in 1:length(teams)){
  newrow <- as.numeric(final$Team == teams[i])
  matrix <- rbind(matrix,newrow)
}

for (i in 1:17){
  newrow <- as.numeric(final$Week == i)
  matrix <- rbind(matrix,newrow)
}

matrix <- as.matrix(matrix)

direction <- c(rep(">=", num.picks),
               rep(">=", num.picks),
               rep("<=", length(teams)),
               rep("==", 17)
              )
rhs <- c(rep(0, num.picks),
         rep(0, num.picks),
         rep(1, length(teams)),
         rep(1, 17))

sol <- Rglpk_solve_LP(obj = obj, mat = matrix, dir = direction, rhs = rhs,
                      types = var.types, max = TRUE)

finalSelection <- final[sol$solution == 1,]

finalSelection <- finalSelection[order(finalSelection$Week),]

write.csv(finalSelection,file="SurvivorPicks.csv")