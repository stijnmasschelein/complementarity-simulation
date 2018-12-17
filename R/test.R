rho_12 = 0.3; rho_13 = 0.4; rho_23 = .5;
N = 1e7; mu = c(0, 0, 0) 
sigma = matrix(c(1, rho_12, rho_13,
                 rho_12, 1, rho_23,
                 rho_13, rho_23, 1), 
               nrow = 3)

n <- 5 
A <- matrix(runif(n^2)*2-1, ncol=n) 
Sigma <- t(A) %*% A

cholesky = function(M){
  if(dim(M)[1] != dim(M)[2]){
    return(NULL)    
  } else {
    N = dim(M)[1]
    L = matrix(rep(0, N * N), nrow = N)
    for (i in 1:N){
      for (j in 1:i){
        total = 0; k = 1
        if (i == j){
          while (k < j){
            total = total + L[j,k]^2
            k = k + 1
          }
          L[j,j] = sqrt(M[j, j] - total)
        } else {
          while (k < j){
            total = total + L[i,k] * L[j,k]
            k = k + 1
          }
          L[i,j] = 1/L[j,j] * (M[i,j] - total)
        }
      }
    } 
    return(L)
  }
}

L_inv = solve(L)

invert_L = function(L){
  if(dim(L)[1] != dim(L)[2]){
    return(NULL)    
  } else {
    N = dim(L)[1]
    invL = diag(N)
    D = diag(1/(diag(L)))
    Lnorm = D %*% L 
    for(j in 1:(N-1)){
      for (i in (j + 1):N){
            invL[i, i - j] = - invL[i, (i-j):i] %*%  Lnorm[(i - j):i, (i-j)]
        }
      }
  }
  return(invL %*% D)
}

##################################################

source("R/parameters.R")
library(simcompl)
library(microbenchmark)
library(boot)

get_coefs = function(formula, data, indices){
  d = data[indices, ]
  coefficients(lm(formula, data = d))["x1:x2"]
}

test_boot = function(bootstrap = FALSE, R = 100, O = 8){
  # data = create_sample(obs = 300, rate = 1/O, sd_eps = c(1, 1, 0),
  #                      b2 = c(0.25, 0, 0), g1 = c(.3, -.3, 0))
  formula = y ~ x1*x2 + (x1 + x2) * z + I(x1^2) + I(x2^2)
  if (bootstrap){
    test = boot(data = data, statistic = get_coefs, R = R,
                formula = formula)
  } else {
    test = lm(formula, data = data)
  }
}

bmark = microbenchmark(lm = test_boot(F),
                       boot200 = test_boot(T, 200),
                       boot2000 = test_boot(T, 2000),
                       times = 50)


