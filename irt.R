library(mirt)
library(MASS)
library(glmnet)
library(GPArotation)


sinTheta <- function(A1, A2){
  V1 <- svd(A1)$u
  V2 <- svd(A2)$u
  return(norm((V1 %*% t(V1) - V2 %*% t(V2)), "F"))
}
# Set-Up
p <- 2 # 2 latent variables
n <- 10 # 10 items in total
C <- 3 # number of categories
N <- 1000 # number of respondents (sample size)
MCrep <- 100 # number of Monte Carlo replications

# The ground truth parameters for all 10 items
a <- matrix(c(2.20,2.00, 2.60, 1.60,1.70,1.80,1.80,1.90,1.60,1.70,
              0,0,0,0,0,1.2,1.1,1.2,2.1,1.5),10,2)
a # 20 x 2 matrix of slopes
d <- matrix(c(0.67, 1.09, -0.18, -0.76, 0.5, -0.41, -0.07, 1.15, 0.13, -1.10,
              -0.72, -0.14, -1.22, -1.42, -0.36, -1.26, -0.96, -0.09, -0.70, -1.56),10,2)
d # 20 x 2 matrix of intercepts

ground_truth <- oblimin(a)
truth <- cbind(ground_truth$loadings,d)
truth

# Create a list of vectors to store the sum of coefficients 
mcrep <- rep(0,MCrep)
v <- list(mcrep, mcrep,mcrep, mcrep)
coefficient <- list(v,v,v,v,v,v,v,v,v,v)
w <- rep(0,4)
se <- list(w,w,w,w,w,w,w,w,w,w)
Estimates_temp <- list(w,w,w,w,w,w,w,w,w,w)
sinThetaDist <- rep(0,MCrep)

# Monte Carlo replication: MCrep = 100
for(iter in 1:MCrep){
  print(paste("Monte carlo iteration ", iter))
  # Generate data using the simdata() function
  data <- simdata(a=a,d=d,N=1000,itemtype = rep('graded',10))
  # data
  
  # Configure the MH-RM algorithm
  # p = 2 -> 2 latent variables
  # TOL -> convergence criteria
  # gain = c(1,1) -> \gamma_k = 1\k, where k is the iteration number
  # pars = "values" -> enables us to control the starting values
  # MHDRAWS = 1 -> m_k = 1
  values <- mirt(data,p, method = 'MHRM',TOL = 0.0001,verbose = FALSE,
                technical = list(MHDRAWS = 1,gain = c(1,1)),pars = "values")
  # values <- mirt(data,p, method = 'EM',TOL = 0.00001,pars = "values")
  
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
  #head(values)
  # Fit the model again using desired starting values of parameters
  model <- mirt(data, p, method = 'MHRM', itemtype = 'graded', verbose = FALSE,
                TOL = 0.0001,technical = list(MHDRAWS = 1,gain = c(1,1)),pars = values)
  #model <- mirt(data, p, method = 'EM', itemtype = 'graded', TOL = 0.00001)
  
for(i in 1:10){
    for(j in 1:4){
      coefficient[[i]][[j]][iter] = coef(model,rotate = 'oblimin')[[i]][j]
      Estimates_temp[[i]][j] = coef(model,rotate = 'oblimin')[[i]][j]
    }
}
  # Compute the sinThetaDistance
  
  results <- matrix(unlist(Estimates_temp),nrow = 10,byrow = TRUE)
  sinThetaDist[iter] = sinTheta(results, truth)
  
}

#  Take the average of estimates of all 10 items

estimates <- list(w,w,w,w,w,w,w,w,w,w)

for(i in 1:10){
  for(j in 1:4){
    estimates[[i]][j] = mean(coefficient[[i]][[j]])
  }
}
for(i in 1:10){
  for(j in 1:4){
    se[[i]][j] = sqrt(var(coefficient[[i]][[j]]))
  }
}
# coefficient
# estimates
# se
print("Centralized MIRT with oblique rotation")
print("Experimental Results:")
print("Estimator")
estimates <- matrix(unlist(estimates),nrow = 10,byrow = TRUE)
round(estimates,2)
print("Standard error of estimation")
se <- matrix(unlist(se),nrow = 10,byrow = TRUE)
round(se,2)
print('SinTheta Distance:')
mean(sinThetaDist)


