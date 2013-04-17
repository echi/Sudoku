if (is.loaded('sudoku_by_projection')) {
  dyn.unload('../Functions/projection/sudoku_by_projection.so')
}
dyn.load('../Functions/projection/sudoku_by_projection.so')

call_sudoku_by_projection = function(p, D, max_iter=1e6, eta=1e-16) {
  n = as.integer(length(p))
  mcc = make_coordinate_constraints(D)
  CCM = mcc$CCM
  active_lengths = as.integer(mcc$active_lengths)
  n_constraints = as.integer(nrow(CCM))
  f_sol = .Fortran('sudoku_by_projection', p=p,
                   q=double(n), n, CCM, n_constraints,
                   active_lengths, as.integer(max_iter),
                   as.double(eta), iter=integer(1), len=double(1), qint=integer(n))
  return(list(p=f_sol$q, q=f_sol$qint, iter=f_sol$iter))
}
