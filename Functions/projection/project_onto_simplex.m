function x = project_onto_simplex(y)
% PROJECT_ONTO_SIMPLEX projects a point onto the simplex.
%
% x = project_onto_simplex(y) maps the point y to the closest point x in
%   in the simplex, i.e. x is the unique point that minimizes
%
%    ||x - y||_2 subject to sum(x) = 1 and x >= 0
%
% Author: Eric Chi (ecchi@ucla.edu)

n = length(y);
w = sort(y, 'descend');
psum_w = cumsum(w);
clambdas = (psum_w - 1)./(1:n)';
k = find(w > clambdas, 1, 'last');
lambda = clambdas(k);
x = max(y - lambda, 0);