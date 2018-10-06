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
