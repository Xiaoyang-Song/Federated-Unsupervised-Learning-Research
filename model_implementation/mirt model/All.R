library(mirt)
library(MASS)
library(glmnet)
library(doSNOW)
library(foreach)
cluster <- makeCluster(6, outfile="")
registerDoSNOW(cluster)

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
K <- 10 # number of clients
n <- N / K # number of samples per clients
R <- 10 # Amplification ratio

# Reboot Algorithm:
# 1. Distribute the whole sample into several clients
# 2. On each client, fit a graded IRT model and retrieve the estimated parameters
# 3. For each client, use the estimated parameter to generate a sample of size R
# 4. Aggregate all the generated samples to one single sample of size kR
# 5. Use this sample of size kR to fit a graded IRT model and report the estimated parameters
# The ground truth parameters for all 10 items
a <- matrix(c(2.20,2.00, 2.60, 1.60,1.70,1.80,1.80,1.90,1.60,1.70,
              0,0,0,0,0,1.2,1.1,1.2,2.1,1.5),10,2)
# a # 20 x 2 matrix of slopes
d <- matrix(c(0.67, 1.09, -0.18, -0.76, 0.5, -0.41, -0.07, 1.15, 0.13, -1.10,
              -0.72, -0.14, -1.22, -1.42, -0.36, -1.26, -0.96, -0.09, -0.70, -1.56),10,2)
# d # 20 x 2 matrix of intercepts
truth <- cbind(a,d) # ground truth
truth # Print ground truth

# Create a list of vectors to store the sum of coefficients 
#v <- rep(0,4)
#coef <- list(v,v,v,v,v,v,v,v,v,v)
mcrep <- rep(0,MCrep)
v <- list(mcrep, mcrep,mcrep, mcrep)
coefficient <- list(v,v,v,v,v,v,v,v,v,v)
clientCoef <- list(v,v,v,v,v,v,v,v,v,v)
w <- rep(0,4)
se <- client_se <- list(w,w,w,w,w,w,w,w,w,w)

average.estimator <- list(w,w,w,w,w,w,w,w,w,w)
# DECLARE lists to store sintheta distance and int
sinThetaDist_Reboot<- intercept_Error_Reboot  <- rep(0,MCrep)
average.Client.sinThetaDist <- average.Client.interceptError  <- rep(0,MCrep)
client1.sinThetaDist <- client1.interceptError  <- rep(0, MCrep)


for(iter in 1:MCrep){
  print(paste("Monte carlo iteration ", iter))
  # Generate N = 1000 samples in total
  print("Generate Data")
  data <- simdata(a=a,d=d,N=N,itemtype = rep('graded',10))
  data
  # divide the whole sample into K clients
  print("Fit mirt model on each client:")
  # Create a list to collect estimates of each client
  clients_Coefficients <- list()
  for(index in 1:K){
    print(paste("MIRT for client ",index))
    client.data <- data[(1+(index-1)*n):(n*index),]
    values <- mirt(client.data,p, method = 'MHRM',TOL = 0.0001,SE = FALSE,SE.type = 'MHRM',
                   technical = list(NCYCLES=100000,MHDRAWS = 1,gain = c(1,1)),pars = "values")
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
    # print(coef(model))
    if(index == 1){
      for(i in 1:10){
        for(j in 1:4){
          clientCoef[[i]][[j]][iter] = clients_Coefficients[[index]][[i]][j]
        }
      }
    }
    
    # Compute the average estimator
    for(i in 1:10){
      for(j in 1:4){
        average.estimator[[i]][j] = average.estimator[[i]][j] + clients_Coefficients[[index]][[i]][j]
      }
    }
  }
  #========================================================================#
  # END OF FITTING K CLIENTS ---> Start reboot#
  client.results = matrix(unlist(average.estimator),nrow = 10,byrow = TRUE) / K
  # print(client.results)
  average.Client.sinThetaDist[iter] = sinTheta(client.results[,0:2], a)
  average.Client.interceptError[iter] = norm((client.results[,3:4]-d), "F")
  print(client.results[,3:4])
  
  client1.results <- list(w,w,w,w,w,w,w,w,w,w)
  for(i in 1:10){
    for(j in 1:4){
      client1.results[[i]][j] = clientCoef[[i]][[j]][iter]
    }
  }
  client1.results = matrix(unlist(client1.results),nrow = 10,byrow = TRUE)
  client1.sinThetaDist[iter] = sinTheta(client1.results[,0:2],a)
  client1.interceptError[iter] = norm((client1.results[,3:4]-d), "F")
  
  # Generate data using estimated parameter for each clients
  gen.data <- list()
  for(i in 1:K){
    est <- list()
    dat <- clients_Coefficients[[i]]
    est[[1]] = dat$Item_1[1,1:4]
    est[[2]] = dat$Item_2[1,1:4]
    est[[3]] = dat$Item_3[1,1:4]
    est[[4]] = dat$Item_4[1,1:4]
    est[[5]] = dat$Item_5[1,1:4]
    est[[6]] = dat$Item_6[1,1:4]
    est[[7]] = dat$Item_7[1,1:4]
    est[[8]] = dat$Item_8[1,1:4]
    est[[9]] = dat$Item_9[1,1:4]
    est[[10]] = dat$Item_10[1,1:4]
    
    df <- matrix(unlist(est),nrow = 10,byrow = TRUE)
    gen.data[[i]] <- simdata(a=df[,1:2],d=df[,3:4],N=R*n,itemtype = rep('graded',10))
  }
  data <- as.data.frame(do.call(rbind, gen.data))
  values <- mirt(data,p, method = 'MHRM',TOL = 0.0001,SE = TRUE,SE.type = 'MHRM',
                 technical = list(NCYCLES = 100000,MHDRAWS = 1,gain = c(1,1)),pars = "values")
  # values
  for(i in 1:10){
    for(j in 1:4){
      values[4*(i-1)+j,6] = clientCoef[[i]][[j]][iter]
    }
  }
  values[38,9] = TRUE
  modelf <- mirt(data, p, method = 'MHRM', itemtype = 'graded', SE = TRUE,verbose = FALSE, SE.type = 'MHRM',
                 TOL = 0.0001,technical = list(NCYCLES = 100000,MHDRAWS = 1,gain = c(1,1)),pars = values)
  #coef(model)
  for(i in 1:10){
    for(j in 1:4){
      coefficient[[i]][[j]][iter] = coef(modelf)[[i]][1+3*(j-1)]
    }
  }
  
  reboot.result <- list(w,w,w,w,w,w,w,w,w,w)
  for(i in 1:10){
    for(j in 1:4){
      reboot.result[[i]][j] = coefficient[[i]][[j]][iter]
    }
  }
  reboot.result = matrix(unlist(reboot.result),nrow = 10,byrow = TRUE)
  sinThetaDist_Reboot[iter] = sinTheta(reboot.result[,0:2],a)
  intercept_Error_Reboot[iter] = norm((reboot.result[,3:4]-d), "F")
}


#  Take the average of estimates of all 10 items
estimates <- list(w,w,w,w,w,w,w,w,w,w)
client1est <- list(w,w,w,w,w,w,w,w,w,w)
# coefficient
for(i in 1:10){
  for(j in 1:4){
    estimates[[i]][j] = mean(coefficient[[i]][[j]])
    client1est[[i]][j] = mean(clientCoef[[i]][[j]])
    
  }
}
# clientCoef
for(i in 1:10){
  for(j in 1:4){
    se[[i]][j] = sqrt(var(coefficient[[i]][[j]]))
    client_se[[i]][j] = sqrt(var(clientCoef[[i]][[j]]))
  }
}
#se
client1est <- matrix(unlist(client1est),nrow = 10,byrow = TRUE)
se <- matrix(unlist(se),nrow = 10,byrow = TRUE)
estimates <- matrix(unlist(estimates),nrow = 10,byrow = TRUE)

print("Reboot Algorithm Without Rotation")
print("SinTheta Distance for Slopes of Client 1")
mean(client1.sinThetaDist)
print("Frobenius Norm of Difference of Intercepts of Client 1: ")
mean(client1.interceptError)
print("SinTheta Distance for Average Estimator")
mean(average.Client.sinThetaDist)
print("Frobenius Norm of Difference of Intercepts of Average Estimator: ")
mean(average.Client.interceptError)
print("SinTheta Distance for Reboot Estimator")
mean(sinThetaDist_Reboot)
print("Frobenius Norm of Difference of Intercepts of Reboot Estimator: ")
mean(intercept_Error_Reboot)
print("Reboot Estimator")
round(estimates,2)
print("Standard error")
round(se,2)