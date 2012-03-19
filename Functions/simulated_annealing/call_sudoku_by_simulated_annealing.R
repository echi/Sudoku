  source("../Functions/simulated_annealing/generate_probabilities.R")
  source('../Functions/simulated_annealing/prepare_puzzle_for_simulated_annealing.R')

  if (is.loaded('sudoku_by_simulated_annealing')) {
    dyn.unload('../Functions/simulated_annealing/sudoku_by_simulated_annealing.so')
  }
  dyn.load('../Functions/simulated_annealing/sudoku_by_simulated_annealing.so')
  
  call_sudoku_by_simulated_annealing = function(X, D, Z, K = as.double(2), T0 = as.double(100), max_iter=as.integer(1e6)) {
    p = as.integer(tensor_to_vector(make_tensor(X)))
    n = as.integer(length(p))
    q = integer(n)
    storage.mode(Z) = "integer"
    storage.mode(X) = "integer"  
    storage.mode(D) = "integer"
    n_constraints = as.integer(nrow(D))
    n_missing = as.integer(nrow(Z))
    n_violations = integer(1)
    loss_hx = integer(max_iter)
    f_sol = .Fortran('sudoku_by_simulated_annealing',
         p=p, q=q, n, D=D, n_constraints=n_constraints,
         X=X, as.integer(nrow(X)),
         Z=Z, n_missing=n_missing, n_violations=n_violations,
         loss_hx=loss_hx, max_iter=max_iter, K, T0, iter=integer(1))
    return(list(q=f_sol$q, loss=f_sol$loss_hx[1:f_sol$iter], iter=f_sol$iter))
  }
