  rm(list=ls())
  library(ggplot2)
  library(stringr)
  source('../Functions/sudoku.R')
  source('../Functions/projection/projection.R')
  source('../Functions/simulated_annealing/call_sudoku_by_simulated_annealing.R')
  setwd("../Puzzles/top95")
  
  ### Load Top95 Puzzles
  n_puzzles=sum(str_count(dir(),'Top95_'))
  number_iterations_top95 = matrix(NA, n_puzzles,1)
  top95_puzzles = vector(mode="list", n_puzzles)
  C = build_constraints_matrix()
  filenames = dir()[grep('Top95_',dir())]
  
  for (i in 1:n_puzzles) {
    top95_puzzles[[i]] = get_puzzle(filenames[i])
  }

  ### Test Backtracking
  steps_backtrack_top95 = matrix(NA, n_puzzles,1)
  cputime_backtrack_top95 = double(n_puzzles)  
  sudoku_path = '~/Dropbox/Work/Research/UCLA/Sudoku/Methods/backtracking/sudoku'
  for (i in 1:n_puzzles) {
    filename = filenames[i]
    cputime_backtrack_top95[i] = system.time({    
	  u = system(paste(sudoku_path, '<',filename), intern=T)
    })[3]
	  steps_backtrack_top95[i] = as.integer(u[length(u)])
  }

  ### Test Simulated Annealing
  steps_simulated_annealing_top95 = matrix(0, n_puzzles, 2)
  for (i in 16:n_puzzles) {
    X = top95_puzzles[[i]]
    puzzle_parameters = prepare_puzzle_for_simulated_annealing(X)
    X = puzzle_parameters$X
    D = puzzle_parameters$D
    Z = puzzle_parameters$Z
    max_resolves = 1
    total_iter = 0  
    for (j in 1:max_resolves) {
      sol = call_sudoku_by_simulated_annealing(X,D,Z,max_iter=as.integer(2e5))
      print(paste("Attempt number",j, ", iterations =",sol$iter) )
      total_iter = total_iter + sol$iter
    ### Check Answer
      if (all(C %*% sol$q == 1)) {
        print("Correct Solution Found.")
        steps_simulated_annealing_top95[i, 1] = 1
        steps_simulated_annealing_top95[i, 2] = total_iter
        break
      } else {
        Z[,3] = sample(Z[,3], nrow(Z))
      }
    }
  }  

  ### Test Alternating Projection
  steps_projection_top95 = matrix(0, n_puzzles, 2)
  cputime_projection_top95 = double(n_puzzles)    
  for (i in 1:n_puzzles) {
  ### 1. Presolve the problem
    X = top95_puzzles[[i]]
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
    for (j in 1:max_resolves) {
      p_last = p
      sol = call_sudoku_by_projection(p, D)
      print(paste("Attempt number",j, ", iterations =",sol$iter) )
      total_iter = total_iter + sol$iter
    ### Check Answer
      if (all(C %*% sol$q == 1)) {
        print("Correct Solution Found.")
        steps_projection_top95[i, 1] = 1
        steps_projection_top95[i, 2] = total_iter      
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
  }

  ### Find the puzzles that alternating projection successfully solved and perform timings
  cputime_projection_top95 = double(n_puzzles)
  ix_solved = which(steps_projection_top95[,1]==1)
  for (i in ix_solved) {
    X = top95_puzzles[[i]]
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
    cputime_projection_top95[i] = system.time({
      for (j in 1:max_resolves) {
        p_last = p
        sol = call_sudoku_by_projection(p, D)
        total_iter = total_iter + sol$iter
        ### Check Answer
        if (all(C %*% sol$q == 1)) {
          steps_projection_top95[i, 1] = 1
          steps_projection_top95[i, 2] = total_iter      
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
  save(top95_puzzles, steps_projection_top95, steps_simulated_annealing_top95, steps_backtrack_top95,
       cputime_backtrack_top95, cputime_projection_top95,
       file="../../Output_Files/top95_puzzles.rda")

  ### Plots for CPU times.
  load('../../Output_Files/top95_puzzles.rda')  
  golden_ratio = 1.61803399
  ht = 4
  ix = which(steps_projection_top95[,1]==1)
  cpu_time = cbind(cputime_projection_top95[ix], cputime_backtrack_top95[ix])
  cpu_time = as.data.frame(cpu_time)
  names(cpu_time) = c("Projection","Backtracking")
  q = ggplot(data=cpu_time, aes(x=Backtracking, y=Projection))
  q = q + geom_point()
  q = q + geom_abline(intercept=0, slope=1, linetype=2)
  q = q + xlab("Backtracking (sec)") + ylab("Alternating Projection (sec)")
  q = q + theme_bw()
  q = q + opts(axis.title.x=theme_text(vjust=-0.25, size=12))
  q
  ggsave('../../Output_Files/Comparison_top95.pdf', width=ht*golden_ratio, height=ht)  
  