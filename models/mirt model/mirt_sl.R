library(mirt)
library(Rlab)
library(MASS)
# set.seed(2022)
l2 <- function(x) {
  return(sum(x ^ 2) ^ 0.5)
}
Y2Ytilde <- function(Y) {
  Y_tilde <- Y
  for (i in 1:(R - 1)) {
    Y_tilde <- rbind(Y_tilde, Y)
  }
  return(Y_tilde)
}

J <- 5
K <- 2
N <- 3000
R <- 10
M <- N * R
error_surr <- rep(0, 100)
error_mirt <- rep(0, 100)
# For now, there is no intercept term involved.
a <- matrix(c(2.2, 1.2, 1.6, -1.4, -1.2, -2, -1.0, -2.4, 3.2, 0.0), J, K) # J x K #nolint
a
# Generate data
X <- matrix(rnorm(N * K), N, K) # Factor score matrix: N x K #nolint
prob <- 1 / (1 + exp(-(X %*% t(a))))
# Y <- t(sapply(1:N, function(i) rbern(n = J, prob = prob[i,]))) #nolint
Y <- t(sapply(1:N, function(i) rbinom(n = J, size = 1, prob = prob[i,]))) #nolint
dim(Y)
# Y <- simdata(a = a, d = rep(0, J), N = N, itemtype = rep("3PL", J)) #nolint
Y
X_tilde <- matrix(rnorm(M * K), M, K) # M x K
head(X_tilde)
dim(X_tilde)
Y_tilde <- Y2Ytilde(Y) # M x J
Y_tilde
c(Y_tilde)
ld_mat <- matrix(0, M * J, J * K)
for (j in 1:J) {
  ld_mat[(((j - 1) * M + 1):(j * M)), (((j - 1) * K + 1):(j * K))] <- X_tilde
  #   print(dim(ld_mat[(((j - 1) * M + 1):(j * M)), (((j - 1) * K + 1):(j * K))]))
}
head(ld_mat)
sl_fit <- glm(c(Y_tilde) ~ ld_mat - 1, family = binomial,
              control = list(maxit = 10000))
sl_fit_coef <- coef(sl_fit)
sl_fit_coef <- matrix(coef(sl_fit))
sl_fit_coef
a_trans <- matrix(t(a), byrow = TRUE)
error_surr[1] <- l2(a_trans - sl_fit_coef)
error_surr[1]
values <- mirt(data.frame(Y), 2, itemtype = "graded", method = "MHRM", technical = list(NCYCLES = 10000), pars = "values")
values
for (j in 1:J) {
  values[3 * j, 6] <- 0
  values[3 * j, 9] <- FALSE
}
values[14, 9] <- TRUE
mirt_fit <- mirt(data.frame(Y), 2, itemtype = "graded", method = "MHRM", technical = list(NCYCLES = 10000), pars = values)
coef(mirt_fit)
