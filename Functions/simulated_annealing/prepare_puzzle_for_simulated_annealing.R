source("../Functions/sudoku.R")

prepare_puzzle_for_simulated_annealing = function(X, seed=1) {
  C = build_constraints_matrix()
  presolved = presolve(C,X)
  X <- presolved$X
  D <- presolved$D
  # List of cells that are missing in each row.
  Y = matrix(NA, 81-nrow(X), 2)
  ix = 1
  for (i in 1:9) {
    missing_columns = setdiff(1:9,X[which(X[,1]==i),2])
    nMissing = length(missing_columns)
    if (nMissing > 0) {
      for (j in 1:nMissing) {
        Y[ix,1] = i
        Y[ix,2] = missing_columns[j]
        ix = ix + 1
      }
    }
  }
  counts = matrix(9,9,1)
  for (i in 1:nrow(X)) {
    value = X[i,3]
    counts[value] = counts[value] - 1
  }
  missing_values = c()
  for (i in 1:length(counts)) {
    missing_values <- c(missing_values, rep(i,counts[i]))
  }
  set.seed(seed)
  x = sample(missing_values, length(missing_values))
  Z = cbind(Y,x)
  return(list(X=X,D=D,Z=Z))
}
