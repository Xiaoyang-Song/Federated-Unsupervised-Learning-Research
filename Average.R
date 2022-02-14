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

# Model configuration
p <- 2 # number of dimensions
C <- 3 # number of categories
MCrep <-2 # number of Monte Carlo replication
N <- 1000 # sample sizes
K <- 2 # number of clients
n <- N / K # number of samples per clients
R <- 10 # Amplification ratio

a <- matrix(c(2.20,2.00, 2.60, 1.60,1.70,1.80,1.80,1.90,1.60,1.70,
              0,0,0,0,0,1.2,1.1,1.2,2.1,1.5),10,2)
# a # 20 x 2 matrix of slopes
d <- matrix(c(0.67, 1.09, -0.18, -0.76, 0.5, -0.41, -0.07, 1.15, 0.13, -1.10,
              -0.72, -0.14, -1.22, -1.42, -0.36, -1.26, -0.96, -0.09, -0.70, -1.56),10,2)
# d # 20 x 2 matrix of intercepts
truth <- cbind(a,d) # ground truth
truth # Print ground truth

# Create a list of vectors to store the sum of coefficients 
mcrep <- rep(0,MCrep)
v <- list(mcrep, mcrep,mcrep, mcrep)
w <- rep(0,4)
coefficient <- list(v,v,v,v,v,v,v,v,v,v)
average.estimator <- list(w,w,w,w,w,w,w,w,w,w)
# DECLARE lists to store sinTheta distance and int
average.Client.sinThetaDist <- average.Client.interceptError  <- rep(0,MCrep)
Estimates_temp <- list(w,w,w,w,w,w,w,w,w,w)
sinThetaDist <- rep(0,MCrep)
interceptsError <- rep(0, MCrep)


for(iter in 1:MCrep){
  print(paste("Monte carlo iteration ", iter))
  # Generate N = 1000 samples in total
  print("Preparation: Generating Data")
  data <- simdata(a=a,d=d,N=N,itemtype = rep('graded',10))
  # data
  print("Regime 1: Centralized version of MIRT")
  values <- mirt(data,p, method = 'MHRM',TOL = 0.0001,
                 technical = list(MHDRAWS = 1,gain = c(1,1)),pars = "values")
  # Set the starting values
  for(i in 1:40){
    if(i %% 4 == 3){
      values[i,6] = 0.25
    }else if(i %% 4 == 0){
      values[i,6] = -0.25
    }else{
      values[i,6] = 1.0
    }
  }
  # The second slope of the first item is fixed to 0.0
  values[2,6] = 0.0
  values[2,9] = FALSE
  values[38,9] = TRUE
  # Fit the model again using desired starting values of parameters
  model <- mirt(data, p, method = 'MHRM', itemtype = 'graded', verbose = FALSE,
                TOL = 0.0001,technical = list(NCYCLES = 1000000, MHDRAWS = 1,gain = c(1,1)),pars = values)
  
  for(i in 1:10){
    for(j in 1:4){
      coefficient[[i]][[j]][iter] = coef(model)[[i]][j]
      Estimates_temp[[i]][j] = coef(model)[[i]][j]
    }
  }
  results <- matrix(unlist(Estimates_temp),nrow = 10,byrow = TRUE)
  sinThetaDist[iter] = sinTheta(results[,0:2], a)
  interceptsError[iter] = norm(results[,3:4]-d, "F")
  
  # Divide the whole sample into K clients
  print("Regime 2: Average Estimator")
  print("- Fit MIRT model on each client:")
  # Create a list to collect estimates of each client
  clients_Coefficients <- list()
  for(index in 1:K){
    print(paste("-- MIRT for client ",index))
    client.data <- data[(1+(index-1)*n):(n*index),]
    values <- mirt(client.data,p, method = 'MHRM',TOL = 0.0001,SE = FALSE,SE.type = 'MHRM',
                   technical = list(NCYCLES=1000000,MHDRAWS = 1,gain = c(1,1)),pars = "values")
    for(i in 1:40){
      if(i %% 4 == 3){
        values[i,6] = 0.25
      }else if(i %% 4 == 0){
        values[i,6] = -0.25
      }else{
        values[i,6] = 1.0
      }
    }
    values[2,6] = 0.0
    values[2,9] = FALSE
    values[38,9] = TRUE
    model <- mirt(client.data, p, method = 'MHRM', itemtype = 'graded', SE = FALSE,verbose = FALSE, SE.type = 'MHRM',
                  TOL = 0.0001,technical = list(MHDRAWS = 1,gain = c(1,1),NCYCLES=100000),pars = values)
    # Store the estimated parameter for all clients
    clients_Coefficients[[index]] = coef(model)
    # Compute the average estimator
    for(i in 1:10){
      for(j in 1:4){
        average.estimator[[i]][j] = average.estimator[[i]][j] + clients_Coefficients[[index]][[i]][j]
      }
    }
  }
  #========================================================================#
  client.results = matrix(unlist(average.estimator),nrow = 10,byrow = TRUE) / K
  average.Client.sinThetaDist[iter] = sinTheta(client.results[,0:2], a)
  average.Client.interceptError[iter] = norm((client.results[,3:4]-d), "F")
}
print("Step <3> Report Estimation Error: ")
print('SinTheta Distance of Slopes:')
mean(sinThetaDist)
print("Frobenius Norm of Difference of Intercepts: ")
mean(interceptsError)
print("SinTheta Distance for Average Estimator")
mean(average.Client.sinThetaDist)
print("Frobenius Norm of Difference of Intercepts of Average Estimator: ")
mean(average.Client.interceptError)
















