generate_probabilities = function(X,Y,x) {

  n_free = nrow(Y)
  board = matrix_rep(rbind(X,cbind(Y,x)))

  n_violations = matrix(0, n_free, 1)
  for (i in 1:n_free) {
    r = Y[i,1]
    c = Y[i,2]
    value = board[r,c]
    if (length(which(board[r,] == value)) > 1) {
      n_violations[i] = n_violations[i] + 1
    }
    if (length(which(board[,c] == value)) > 1) {
      n_violations[i] = n_violations[i] + 1
    }
    ixrow = 1:3
    while(!(r %in% ixrow)) {
      ixrow = ixrow + 3
    }
    ixcol = 1:3
    while(!(c %in% ixcol)) {
      ixcol = ixcol + 3
    }
    if(length(which(board[ixrow,ixcol] == value)) > 1) {
      n_violations[i] = n_violations[i] + 1
    }
  }

  pr = exp(n_violations)
  pr = pr/sum(pr)
  return(pr)
}
