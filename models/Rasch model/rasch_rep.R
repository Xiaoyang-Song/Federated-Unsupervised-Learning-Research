library(mirt)
library(MASS)
set.seed(2022)

mse <- function(x) {
  return(sum(x ^ 2))
}

J <- 10
R <- 10
N1 <- 1000
N2 <- N1 * R
a <- matrix(1, 10, 1)
# d_star <- runif(J, 0, 0.2)
d_star <- matrix(c(0, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1)
# d_star
error_surr <- rep(0, 100)
error_mirt <- rep(0, 100)
for (t in 1:100) {
  X <- matrix(rep(rnorm(N1), times = J), N1, J)
  D_star <- matrix(rep(d_star, each = N1), N1, J)
  p <- 1 / (1 + exp(-(X - D_star)))
  Y <- simdata(a = a, d = d_star, N = N1, itemtype = rep("2PL", J))
  # Y <- t(sapply(1:N1, function(i) rbinom(n = J, size = 1, prob = p[i,])))

  #--------------------------------------------------------------
  I_d <- matrix(0, N1 * J, J)
  for (j in 1:J) {
    I_d[(((j - 1) * N1 + 1):(j * N1)), j] <- rep(1, N1)
  }
  # print(I_d)
  fit1 <- glm(c(Y) ~ offset(c(X)) + I_d - 1, family = binomial, control = list(maxit = 500))
  d_fit1 <- coef(fit1)
  error_surr[t] <- mse(d_star - d_fit1)

  #--------------------------------------------------------------
  fit2 <- mirt(data.frame(Y), 1, itemtype = 'Rasch')
  d_fit2 <- sapply(1:J, function(j) coef(fit2)[[j]][2])
  error_mirt[t] <- mse(d_star - d_fit2)
}

mean(error_surr)
mean(error_mirt)
# d_fit1


