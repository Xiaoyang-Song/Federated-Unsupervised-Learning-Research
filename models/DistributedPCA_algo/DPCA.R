library(mirt)
library(MASS)
library(glmnet)
library(doSNOW)
library(foreach)

sinTheta <- function(A1, A2){
  V1 <- svd(A1)$u
  V2 <- svd(A2)$u
  
  return(norm((V1 %*% t(V1) - V2 %*% t(V2)), "F"))
}

qr_decomp <- function(target){
  QRdecomp <- qr(target)
  R <- qr.R(QRdecomp)
  Q <- qr.Q(QRdecomp)
  sgn <- sign(diag(R))
  R <- diag(sgn) %*% R
  Q <- Q %*% diag(sgn)
  return(list(R = t(R), Q = Q))
}

# Seed Configuration
set.seed(2021)
# Model configuration
p <- 2 # number of dimensions
C <- 3 # number of categories
N <- 1000 # sample sizes
M <- 2 # number of clients
J <- 10 # number of items
n <- N / M # number of samples per clients

MCrep <- 5 # number of Monte Carlo replication
sinTheta_DPCA <- rep(0, MCrep)
sinTheta_AVGM <- rep(0, MCrep)
est_DPCA_collection <- matrix(0, J, p)
#================================== GROUND TRUTH ========================================#
a <- matrix(c(2.2, 2, 2.6, 1.6, 1.70, 1.8, 1.8, 1.9, 1.6, 1.7,
              0, 0, 0, 0, 0, 1.2, 1.1, 1.2, 2.1, 1.5), 10, 2)
d <- matrix(c(0.67, 1.09, -0.18, -0.76, 0.5, -0.41, -0.07, 1.15, 0.13, -1.1, -0.72, 
              -0.14, -1.22, -1.42, -0.36, -1.26, -0.96, -0.09, -0.70, -1.56), 10, 2)
truth <- cbind(a, d) # ground truth
#====================================== START MC LOOPS ======================================#
for(iter in 1:MCrep){
  print(paste("Monte carlo iteration ", iter))
  data <- simdata(a=a, d=d, N=N, itemtype=rep('graded', 10))
  
  local_est_all <- list()
  Sigma_tilde <- matrix(0, J, J)
  lambda_tilde <- rep(0, J)
  est_AVGM <- matrix(0, J, p)
  
  for(l in 1:M){
    local_data <- data[(1 + (l - 1) * n):(n * l),]
    values <- mirt(local_data, p, method='MHRM', TOL=1e-04,
                   technical=list(NCYCLES=1e5, MHDRAWS=1, gain=c(1,1)), pars="values")
    # Make sure every parameter is freely estimated
    values[38,9] = TRUE
    model <- mirt(local_data, p, method='MHRM', itemtype='graded', TOL=1e-04,
                  technical=list(MHDRAWS=1, gain=c(1,1), NCYCLES=1e5), pars=values)
    
    local_est <- matrix(0, J, 4)
    for(i in 1:10){
      local_est[i, ] <- coef(model)[[i]][1:4]
    }
    local_est_all[[l]] <- local_est
  }
  
  #------------------DPCA--------------------#
  # Recover Eigenvectors
  for(l in 1:M){
    Sigma_local <- local_est_all[[l]][,1:2] %*% t(local_est_all[[l]][,1:2])
    eigen_vector <- eigen(Sigma_local)$vector
    Sigma_tilde <- Sigma_tilde + eigen_vector %*% t(eigen_vector)
  }
  Sigma_tilde <- Sigma_tilde / M
  V_tilde <- eigen(Sigma_tilde)$vector
  
  # Recover Eigenvalues
  for(l in 1:M){
    Sigma_local <- local_est_all[[l]][,1:2] %*% t(local_est_all[[l]][,1:2])
    lambda_tilde <- lambda_tilde + diag(t(V_tilde) %*% Sigma_local %*% V_tilde)
  }
  lambda_tilde <- lambda_tilde / M
  # Sort the eigenvalues in descending orders in lambda_tilde
  lambda_tilde <- diag(sort(diag(lambda_tilde), decreasing=TRUE),nrow=J, ncol=J)
  # Compute the DPCA estimator
  est_DPCA <- V_tilde %*% sqrt(lambda_tilde)[,1:2]
  # Apply the final QR decomposition to attain PLT form
  est_DPCA <- qr_decomp(t(est_DPCA))$R
  sinTheta_DPCA[iter] <- sinTheta(est_DPCA, a)
  est_DPCA_collection <- est_DPCA_collection + est_DPCA
  #------------------AVGM--------------------#
  for(l in 1:M){
    local_est_qr <- qr_decomp(t(local_est_all[[l]][,1:2]))
    est_AVGM <- est_AVGM + local_est_qr$R / M
  }
  sinTheta_AVGM[iter] <- sinTheta(est_AVGM, a)
}

DPCA_avg_estimator <- est_DPCA_collection / MCrep
print("Bias")
DPCA_avg_estimator - a
sinTheta_DPCA
sinTheta_AVGM
mean(sinTheta_DPCA)
mean(sinTheta_AVGM)
