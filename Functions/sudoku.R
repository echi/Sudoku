get_puzzle <- function(input_file) {
  require(stringr)
  con = file(input_file, open = 'r')
  puzzle = readLines(con, n = -1, warn = FALSE)
  close(con)
  board = puzzle[1]
  for (j in 2:9) {
    board = str_c(board, puzzle[j])
  }
  puzzle = coordinate_rep(matrix(as.numeric(str_split_fixed(board,"",82)[-1]),9,9))
  return(puzzle[-which(puzzle[,3]==0),,drop=F])
}

make_coordinate_constraints = function(D) {
  L <- alply(D, 1, function(x) which(x > 0))
  n = length(L)
  CCM <- matrix(-1, n, 9)
  for (i in 1:n) {
    active_set = L[[i]]
    for (j in 1:length(active_set)) {
      CCM[i,j] <- active_set[j]
    }
  }
  storage.mode(CCM) <- "integer"
  lens = as.integer(laply(L, length))
  return(list(CCM=CCM, active_lengths=lens))
}

index_map = function(ix) {
 return (ix[1] + 9*(ix[2]-1) + 81*(ix[3]-1)) 
}

coordinate_rep = function(S) {
  X = matrix(NA, 81, 3)
  for (i in 1:9) {
    for (j in 1:9) {
      X[i + 9*(j-1),] = c(i,j,S[i,j])
    }
  }
  missing_ix = which(is.na(X[,3]))
  if (length(missing_ix) > 0) {
    X = X[-missing_ix,]  
  }
  return(X)
}

matrix_rep = function(X) {
  S = matrix(0, 9, 9)
  for (ix in 1:nrow(X)) {
      S[X[ix,1],X[ix,2]] = X[ix,3]
  }
  return(S)
}

make_tensor = function(X) {
  P = array(0, c(9,9,9))
  for (ix in 1:nrow(X)) {
    i1 = X[ix,1]
    i2 = X[ix,2]
    i3 = X[ix,3]
    P[i1,i2,i3] = 1
  }
  return(P)
}

make_tensor_rand = function(X) {
  P = array(runif(9^3), c(9,9,9))
  for (ix in 1:nrow(X)) {
    i1 = X[ix,1]
    i2 = X[ix,2]
    i3 = X[ix,3]
    P[i1,i2,] = 0    
    P[i1,i2,i3] = 1
  }
  return(P)
}

tensor_to_vector = function(P) {
  p = matrix(NA, 9^3, 1)
  for (i1 in 1:9) {
    for (i2 in 1:9) {
      for (i3 in 1:9) {
        p[index_map(c(i1,i2,i3))] = P[i1,i2,i3]
      }
    }
  }
  return(p)
}

index_map_rev = function(n) {
  i1 = n %% 9
  i1 = ifelse(i1==0, 9, i1)
  rem = n - i1
  i2 = ((rem/9) %% 9) + 1
  rem = n - i1 - 9 * (i2 - 1)
  i3 = rem/81 + 1
  return(c(i1,i2,i3))
}

vector_to_tensor = function(p) {
  P = array(NA,c(9,9,9))
  for (n in 1:length(p)) {
    i1 = n %% 9
    i1 = ifelse(i1==0, 9, i1)
    rem = n - i1
    i2 = ((rem/9) %% 9) + 1
    rem = n - i1 - 9 * (i2 - 1)
    i3 = rem/81 + 1
    P[i1,i2,i3] = p[n]
  }
  return(P)
}

tensor_to_answer = function(P) {
  S = matrix(NA, 9, 9)
  for (i in 1:9) {
    for (j in 1:9) {
        q = P[i,j,]
        max_q = max(q)
        S[i,j] = ifelse(max_q == 0, 0, which(q == max_q)[1])
    }
  }
  return(S)
}

adjust_constraints_for_clues = function(C,X) {
  D = C
  for (ix in 1:nrow(X)) { #<- For each clue
    t = index_map(X[ix,])
    S = which(D[,t] == 1) #<- Each clue is involved in four constraints
    for (s in S) {
      ix_other = setdiff(which(D[s,] == 1),t) #<- all other cells involved with t for constraint s
      D[,ix_other] = 0
    }
    D = D[-S,] #<- remove the satisfied constraint
  }
  return(D)
}

adjust_constraints = function(C,X) {
  D = C
  repeat {
    D = adjust_constraints_for_clues(C,X)
    if (nrow(X) == 81) {
      break
    }    
    dup = duplicated(D)
    if (any(dup)) {
      D = D[-which(dup),,drop=F]
    }
    ix = sort(apply(D,1,sum), index.return=T, decreasing=F)$ix
    D = D[ix,,drop=F]
    # New clues  
    clue_ix = which(apply(D,1,sum) == 1)
    nClues = length(clue_ix)
    if (nClues == 0) {
      break
    }
    for (i in 1:nClues) {
      X = rbind(X, matrix(index_map_rev(which(D[clue_ix[i],] == 1)),1,3))
    }
  }
  return(list(C = D, X = X))
}

make_active_list = function(L) {
  return(alply(L, 1, function(x) which(x > 0)))
}

presolve = function(C,X) {
  require(plyr)
  adjusted = adjust_constraints(C,X)
  X = adjusted$X
  D = adjusted$C

  ### Modify constraints if new clues arise.
  repeat {
    D = adjust_constraints_for_clues(C,X)
    dup = duplicated(D)
    if (any(dup)) {
      D = D[-which(dup),,drop=F]
    }
    nActive <- aaply(D, 1, sum)
    ix = sort(nActive, index.return=T, decreasing=F)$ix
    D = D[ix,]
    nActive <- nActive[ix]
    # New clues  
    clue_ix = which(nActive == 1)
    nClues = length(clue_ix)
    if (nClues == 0) {
      break
    }
    for (i in 1:nClues) {
      X = rbind(X, matrix(index_map_rev(which(D[clue_ix[i],] == 1)),1,3))
    }
  }
  return(list(X=X,D=D))
}

check_answer = function(p,C) {
  if (all(C %*% p == 1)) {
    print("Answer is correct.")
  } else {
    print("Answer is incorrect.")
  }
}

build_constraints_matrix = function() {
### The Lookup tables for the regions
  B = matrix(NA, 9, 9)
  set = 1:3
  for (i in 1:3) {
    for (j in 1:3) {
      B[set + 3*(i-1), set + 3*(j-1)] = j + 3*(i-1)
    }
  } 

#  B[1:3,1:3] = 1
#  B[1:3,4:6] = 2
#  B[1:3,7:9] = 3
#  B[4:6,1:3] = 4
#  B[4:6,4:6] = 5
#  B[4:6,7:9] = 6
#  B[7:9,1:3] = 7
#  B[7:9,4:6] = 8
#  B[7:9,7:9] = 9

    nPars = 9^3
### Express the row constraints
  row_constraints = matrix(0, 81, nPars)
  for (i3 in 1:9) {
    for (i1 in 1:9) {
      for (i2 in 1:9) {   
          row_constraints[9*(i3-1) + i1, index_map(c(i1,i2,i3))] = 1
      }
    }
  }

  col_constraints = matrix(0, 81, nPars)
  for (i3 in 1:9) {
    for (i2 in 1:9) {
      for (i1 in 1:9) {   
        col_constraints[9*(i3-1) + i2, index_map(c(i1,i2,i3))] = 1
      }
    }
  }

  region_constraints = matrix(0, 81, nPars)
  for (i3 in 1:9) {
    for (rix in 1:9) {
      ix = which(B==rix)
      i1 = ix %% 9
      i1 = ifelse(i1==0,9,i1)
      i2 = (ix - i1)/9 + 1
      for (j in 1:length(ix)) {
        region_constraints[9*(i3-1) + rix, index_map(c(i1[j], i2[j], i3))] = 1
      }
    }
  }

  cell_constraints = matrix(0, 81, nPars)
  for (i1 in 1:9) {
    for (i2 in 1:9) {
      for (i3 in 1:9) {
        cell_constraints[i1 + 9*(i2-1), index_map(c(i1,i2,i3))] = 1
      }
    } 
  }

  C = rbind(row_constraints, col_constraints)
  C = rbind(C, region_constraints)
  C = rbind(C, cell_constraints)

  return(C)
}
