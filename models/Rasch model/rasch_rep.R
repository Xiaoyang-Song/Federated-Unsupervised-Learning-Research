library(mirt)
library(MASS)
set.seed(2022)

mse <- function(x){
  return(sum(x ^ 2))
}

J <- 3
R <- 10
N1 <- 200
N2 <- N1 * R
d_star <- runif(J, 0, 0.2)

error_surr <- rep(0, 100)
error_mirt <- rep(0, 100)
for(t in 1:100){
  X <- matrix(rep(rnorm(N1), times = J), N1, J)
  D_star <- matrix(rep(d_star, each = N1), N1, J)
  p <- 1 / (1 + exp(- (X - D_star)))
  Y <- t(sapply(1:N1, function(i) rbinom(n = J, size = 1, prob = p[i,])))
  
  #--------------------------------------------------------------
  X_tilde <- matrix(rep(rnorm(N2), times = J), N2, J)
  Y_tilde <- Y
  for(i in 1:(R-1)){
    Y_tilde <- rbind(Y_tilde, Y)
  }
  I_d <- matrix(0, N2 * J, J)
  for(j in 1:J){
    I_d[(((j - 1) * N2 + 1):(j * N2)), j] <- rep(N2)
  }
  fit1 <- glm(c(Y_tilde) ~ offset(c(X_tilde)) + I_d - 1, family = binomial, 
              control = list(maxit = 500))
  d_fit1 <- coef(fit1)
  error_surr[t] <- mse(d_star - d_fit1)
  
  #--------------------------------------------------------------
  fit2 <- mirt(data.frame(Y), 1, itemtype = 'Rasch')
  d_fit2 <- sapply(1:J, function(j) coef(fit2)[[j]][2])
  error_mirt[t] <- mse(d_star - d_fit2)
}

mean(error_surr)
mean(error_mirt)
