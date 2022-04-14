library(mirt)
library(MASS)
set.seed(2022)

mse <- function(x) {
  return(sum(x ^ 2))
}

J <- 50
R <- 10
N1 <- 3000 # Global Sample Size
N2 <- N1 * R # Sample size to estimate expectation
a <- matrix(1, 50, 1)
# d_star <- runif(J, 0, 0.2)
# d_star <- matrix(c(0, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1)
# d_star <- matrix(c(0.016, 0.008, -0.050, 0.003, 0.082, -0.177, 0.076, 0.119, 0.135, 0.053, # nolint
#             - 0.099, -0.045, 0.140, -0.124, -0.138, -0.187, -0.143, -0.025, 0.049, 0.042), 20, 1) # nolint
d_star <- matrix(c(0.016, 0.008, -0.050, 0.003, 0.082, -0.177, 0.076, 0.119, 0.135, 0.053, # nolint
            - 0.099, -0.045, 0.140, -0.124, -0.138, -0.187, -0.143, -0.025, 0.049, 0.042, #nolint
            - 0.195, -0.127, 0.161, 0.073, 0.002, -0.181, -0.130, 0.143, -0.022, -0.033, # nolint
            - 0.115, -0.115, 0.150, -0.046, 0.129, 0.012, -0.131, -0.133, 0.196, -0.135, # nolint
            - 0.108, -0.020, 0.180, -0.073, -0.088, 0.132, -0.041, 0.118, 0.097, -0.112), 50, 1) # nolint
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
  fit1 <- glm(c(Y) ~ offset(c(X)) + I_d - 1, family = binomial, control = list(maxit = 500)) #nolint
  d_fit1 <- coef(fit1)
  error_surr[t] <- mse(d_star - d_fit1)

  #--------------------------------------------------------------
  fit2 <- mirt(data.frame(Y), 1, itemtype = 'Rasch')
  # fit2 <- mirt(data.frame(Y), 1, method = "MHRM", TOL = 0.0001, itemtype = 'Rasch')
  d_fit2 <- sapply(1:J, function(j) coef(fit2)[[j]][2])
  error_mirt[t] <- mse(d_star - d_fit2)
}
print("Surrogate Loss MLE:")
print(round(mean(error_surr), 5))
print("CMIRT:")
print(round(mean(error_mirt), 5))
# X
# head(I_d-1)
