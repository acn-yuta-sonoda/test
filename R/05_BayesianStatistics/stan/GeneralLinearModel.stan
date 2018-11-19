// General Linear Model (regression)
//   N: number of records
//   M: number of parameters (except overall means)
//   X: design matrix
//   y: dependent variables

data {
  int<lower=0> N;
  int<lower=0> M;
  matrix[N, M] X;
  vector[N] y;
}
parameters {
  real beta0;
  vector[M] beta;
  real<lower=0> sigma;
}
model {
  for (n in 1:N)
    y[n] ~ normal(beta0+dot_product(X[n],beta), sigma);
}
