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

VVtranspose <- function(L){
  eigendecomp<- eigen(L %*% t(L))
  eigenvec <- eigendecomp$vector
  eigenval <- eigendecomp$values
  return(list(vvt=eigenvec %*% t(eigenvec),vec=eigenvec, val=eigenval))
}

DPCA <- function(average_VVT, Client_Sigma){
  Sigma_Tilde = average_VVT / K
  eigen_sigma = eigen(Sigma_Tilde)
  V_Tilde = eigen_sigma$vectors
  
  # Recover the EigenValues
  Lambda_k = matrix(0, J, J)
  for(i in 1:K){
    Lambda_k = Lambda_k + diag(t(V_Tilde) %*% Client_Sigma[i, , ] %*% V_Tilde)
  }
  Lambda_Tilde = Lambda_k / K
  
  # Recover LLT
  recover_LLT = V_Tilde %*% Lambda_Tilde %*% t(V_Tilde)
  # Recover L
  Lambda_Shrink = Lambda_Tilde[,1:2]
  L_recover = V_Tilde %*% sqrt(abs(Lambda_Shrink))
  
  return(list(recover=recover_LLT, recover_L = L_recover))
}

QRDecomp <- function(target){
  QRdecomp <- qr(target)
  R <- qr.R(QRdecomp)
  Q <- qr.Q(QRdecomp)
  sgn <- sign(diag(R))
  R <- diag(sgn) %*% R
  Q <- Q %*% diag(sgn)
  return(list(R = t(R), Q = Q))
}

PrintResults <- function(){
  if(USE_DPCA){
    print("Algorithm: DistributedPCA")
  }else{
    print("Algorithm: QR Average")
  }
  print("Configuration: ")
  print(paste("----Number of Clients:",K))
  print(paste("----Global Sample Size:",N))
  print(paste("----Local Sample Size:",n))
  print("Experimental Results:")
  print(paste("----Mean SinTheta Distances:",mean(sinTheta_error)))
}

# Seed Configuration
set.seed(2021)
# Algorithm Configuration
USE_DPCA = TRUE
# Model configuration
p <- 2 # number of dimensions
C <- 3 # number of categories
MCrep <-2 # number of Monte Carlo replication
N <- 1000 # sample sizes
K <- 2 # number of clients
J <- 10 # number of items
n <- N / K # number of samples per clients
#================================== GROUND TRUTH ========================================#
a <- matrix(c(2.20,2.00, 2.60, 1.60,1.70,1.80,1.80,1.90,1.60,1.70,
              0,0,0,0,0,1.2,1.1,1.2,2.1,1.5),10,2)
d <- matrix(c(0.67, 1.09, -0.18, -0.76, 0.5, -0.41, -0.07, 1.15, 0.13, -1.10,
              -0.72, -0.14, -1.22, -1.42, -0.36, -1.26, -0.96, -0.09, -0.70, -1.56),10,2)
truth <- cbind(a,d) # ground truth

mcrep <- rep(0,MCrep)
v <- list(mcrep, mcrep,mcrep, mcrep)
L0 <- list(v,v,v,v,v,v,v,v,v,v)

sinTheta_error <- rep(0, MCrep)

#====================================== START MC LOOPS ======================================#
for(iter in 1:MCrep){
  
  print(paste("Monte carlo iteration ", iter))
  print("Step [1]: Data Generation")
  data <- simdata(a=a,d=d,N=N,itemtype = rep('graded',10))
  # divide the whole sample into K clients
  print("Step [2]: Fit MIRT Model on Each Client:")
  
  if(USE_DPCA){
    average_VVT = matrix(0, J, J)
    Client_Sigma <- array(0, dim=c(K, J, J))
  }else{
    average_L <- matrix(0, J, p)
  }
  
  for(index in 1:K){
    print(paste("---- MIRT for Client #",index))
    client.data <- data[(1+(index-1)*n):(n*index),]
    values <- mirt(client.data,p, method = 'MHRM',TOL = 1e-04,
                   technical = list(NCYCLES=1e5,MHDRAWS = 1,gain = c(1,1)),pars = "values")
    values[38,9] = TRUE
    model <- mirt(client.data, p, method = 'MHRM', itemtype = 'graded', TOL = 1e-04,
                  technical = list(MHDRAWS = 1,gain = c(1,1),NCYCLES=1e5),pars = values)
    
    for(i in 1:10){for(j in 1:4){L0[[i]][j] = coef(model)[[i]][j]}}
    
    L = matrix(unlist(L0),nrow = 10,byrow = TRUE)
    
    if(USE_DPCA){
      average_VVT = average_VVT + VVtranspose(L[,1:2])$vvt
      Client_Sigma[index, , ] = L[,1:2] %*% t(L[,1:2])
    }else{
      QR_result = QRDecomp(t(L[,1:2]))
      L_QR = QR_result$R
      average_L = average_L + L_QR
    }
  }
  if(USE_DPCA){
    L_recover = DPCA(average_VVT, Client_Sigma)$recover_L
    QR_result_DPCA = QRDecomp(t(L_recover))
    L_QR_DPCA = QR_result_DPCA$R
    sinTheta_error[iter] = sinTheta(L_QR_DPCA, a)
  }else{
    sinTheta_error[iter] = sinTheta(average_L / K, a)
  }
}
PrintResults()


