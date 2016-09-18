library(Rglpk)

obj = c(2, 10)
mat = matrix(c(0.5, 1.75, 1, 0, 0, 1), nrow = 3, byrow = T)
dir = c("<=", "<=", "<=")
types = c("I", "I")
rhs = c(75, 100, 25)

results <- Rglpk_solve_LP(obj, mat, dir, rhs, types, max = TRUE)