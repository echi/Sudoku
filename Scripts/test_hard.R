  rm(list=ls())
  library(ggplot2)
  library(stringr)  
  source('../Functions/sudoku.R')
  source('../Functions/projection/projection.R')
  source('../Functions/simulated_annealing/call_sudoku_by_simulated_annealing.R')
  setwd("../Puzzles/hard")
  
  ### Load Hard Puzzles
  n_puzzles=sum(str_count(dir(),'Hard_'))
  number_iterations_hard = matrix(NA, n_puzzles,1)
  hard_puzzles = vector(mode="list", n_puzzles)
  C = build_constraints_matrix()
  filenames = dir()[grep('Hard_',dir())]
  
  for (i in 1:n_puzzles) {
    hard_puzzles[[i]] = get_puzzle(filenames[i])
  }

  ### Test Backtracking
  steps_backtrack_hard = matrix(NA, n_puzzles,1)
  cputime_backtrack_hard = matrix(NA, n_puzzles, 1)  
  sudoku_path = '../../Functions/backtracking/sudoku'
  for (i in 1:n_puzzles) {
    filename = filenames[i]
    cputime_backtrack_hard[i] = system.time({    
    u = system(paste(sudoku_path, '<',filename), intern=T)
    })[3]
	  steps_backtrack_hard[i] = as.integer(u[length(u)])
  }

  ### Test Simulated Annealing
  steps_simulated_annealing_hard = matrix(0, n_puzzles, 1)
  tally_simulated_annealing_hard = matrix(FALSE, n_puzzles, 1)
  cputime_simulated_annealing_hard = double(n_puzzles)  
  for (i in 1:n_puzzles) {
    X = hard_puzzles[[i]]
    puzzle_parameters = prepare_puzzle_for_simulated_annealing(X)
    X = puzzle_parameters$X
    D = puzzle_parameters$D
    Z = puzzle_parameters$Z

    time = system.time({
      sol = call_sudoku_by_simulated_annealing(X,D,Z,max_iter=as.integer(2e5))
    })[3]
    steps_simulated_annealing_hard[i] = sol$iter
    tally_simulated_annealing_hard[i] = all(D %*% sol$q == 1)
    if (tally_simulated_annealing_hard[i]) {
      cputime_simulated_annealing_hard[i] = time
    }
  }  

  ### Test Alternating Projection
  steps_projection_hard = matrix(0, n_puzzles, 2)
  for (i in 1:n_puzzles) {
    ### 1. Presolve the problem
    X = hard_puzzles[[i]]
    presolved = presolve(C,X)
    X <- presolved$X
    D <- presolved$D
    ### 2. Initialize p
    p0 = tensor_to_vector(make_tensor(X))

    ### 3. Apply the projections
    p = p0
    max_resolves = 50
    total_iter = 0
    q_last = matrix(0,length(p),1)
    for (j in 1:max_resolves) {
      sol = call_sudoku_by_projection(p, D)
      print(paste("Attempt number",j, ", iterations =",sol$iter) )
      total_iter = total_iter + sol$iter
      ### Check Answer
      if (all(C %*% sol$q == 1)) {
        print("Correct Solution Found.")
        steps_projection_hard[i, 1] = 1
        steps_projection_hard[i, 2] = total_iter      
        break
      } else {
        if (all(sol$q == q_last)) {
          print("Stuck in a rut.")
          break
        }
        q_last = sol$q
        p = as.double(sol$q)
      }
    }
  }

  ### Find the puzzles that alternating projection successfully solved and perform timings
  cputime_projection_hard = double(n_puzzles)
  ix_solved = which(steps_projection_hard[,1]==1)
  for (i in ix_solved) {
    X = hard_puzzles[[i]]
    puzzle_parameters = prepare_puzzle_for_simulated_annealing(X)
    X = puzzle_parameters$X
    D = puzzle_parameters$D
    Z = puzzle_parameters$Z    
    ### 2. Initialize p
    p0 = tensor_to_vector(make_tensor(X))
    ### 3. Apply the projections
    p = p0
    max_resolves = 1e3
    total_iter = 0
    cputime_projection_hard[i] = system.time({
      for (j in 1:max_resolves) {
        p_last = p
        sol = call_sudoku_by_projection(p, D)
        total_iter = total_iter + sol$iter
        ### Check Answer
        if (all(C %*% sol$q == 1)) {
          steps_projection_hard[i, 1] = 1
          steps_projection_hard[i, 2] = total_iter      
          break
        } else {
          if (all(c(sol$q) == as.integer(c(p_last)))) {
            print("Stuck in a loop.")
            break
          } else {
            p = as.double(sol$q)
          }
        }
      }
    })[1]
  }  

  ### Save results
  save(hard_puzzles, steps_projection_hard, steps_simulated_annealing_hard, steps_backtrack_hard,
       tally_simulated_annealing_hard, cputime_backtrack_hard, cputime_projection_hard,
       cputime_projection_hard, file="../../Output_Files/hard_puzzles.rda")

  ### Generate and save Table for CPU times.
  library(xtable)
  ix = which(steps_projection_hard[,1]==1)
  cpu_time = summary(cputime_projection_hard[ix])
  
  ix = which(tally_simulated_annealing_hard)
  cpu_time = cbind(cpu_time, as.numeric(summary(cputime_simulated_annealing_hard[ix])))
  
  cpu_time = cbind(cpu_time, as.numeric(summary(c(cputime_backtrack_hard))))
  cpu_time = cpu_time[-c(2,5),]
  cpu_time = as.data.frame(cpu_time)
  names(cpu_time) = c("Alternating Projection", "Simulated Annealing", "Backtracking")
  print(xtable(cpu_time, digits=3), file="../../Output_Files/cpu_time_hard.tex")