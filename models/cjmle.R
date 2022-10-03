library(mirt)
library(mirtjml)

# sin theta distance between two matrix
sin_theta <- function(A1, A2) {
  V1 <- svd(A1)$u
  V2 <- svd(A2)$u
  return(norm((V1 %*% t(V1) - V2 %*% t(V2)), "F"))
}

mirt_coef <- function(data, K) {
  J <- ncol(data)
  est_slope <- matrix(nrow = J, ncol = K)
  est_intcp <- vector(length = J)
  values <- mirt(data, K, method = 'MHRM', pars = "values")
  values[which(values[, 4] == "a1"), 6] <- 1
  values[which(values[, 4] == "a2")[1:(J - 1)], 6] <- 1
  fit <- mirt(data, K, method = 'MHRM', itemtype = '2PL', pars = values)
  for (j in 1:J) {
    est_slope[j,] <- coef(fit)[[j]][1:K]
    est_intcp[j] <- coef(fit)[[j]][(K + 1)]
  }

  return(list(slope = est_slope, intcp = est_intcp))
}

standardize <- function(A, d, theta) {
  N <- dim(theta)[1]
  d_tilde <- d + rowMeans(A %*% t(theta))
  target <- theta - (1 / N) * (rep(1, N) %*% t(rep(1, N)) %*% theta)
  svd_obj <- svd(target)
  u <- svd_obj$u
  d <- diag(svd_obj$d)
  v <- svd_obj$v
  theta_tilde <- sqrt(N) * u
  A_tilde <- (A %*% v %*% d) / sqrt(N)
  return(list(d_tilde = d_tilde, theta_tilde = theta_tilde, A_tilde = A_tilde))
}

theta_norm <- function(theta) {
  return(apply(theta, 1, function(i) sqrt(sum(matrix(i) ^ 2) + 1)))
}

d_a_norm <- function(d, A) {
  # A: J x K
  # d: J x 1
  sq_fnorm_a <- apply(A, 1, function(i) sum(matrix(i) ^ 2))
  return(sqrt(sq_fnorm_a + d ^ 2))
}
a <- matrix(1:10)
a <- sapply(1:3, function(i) a)
a
n <- theta_norm(theta)
n

s <- apply(a, 2, function(i) norm(matrix(i), 'F'))
s
#==============================================================================#
#==============================================================================#
# set.seed(2022)
# Simulation study for 2PL model
N <- 1000
J <- 10
K <- 2
mc <- 5
A <- matrix(runif((J * K), 1, 2), J, K)
A
A[6:10, 2] <- 0
d <- rnorm(J, -0.5, 0.5)
d

D <- t(sapply(1:N, function(i) d)) # N x J
dim(D)
head(D)
dd
dim(dd)

theta <- matrix(rnorm(N * K), N, K)
head(theta)
P <- 1 / (1 + exp(-theta %*% t(A) - D))

A0 <- matrix(1, J, K)
A0[10, 2] <- 0
d0 <- rep(0, J)
theta0 <- matrix(rnorm(N * K), N, K)
Q <- matrix(TRUE, J, K)
Q[10, 2] <- FALSE

cjmle_err <- mhrm_err <- rep(0, mc)
for (t in 1:mc) {
  data <- t(sapply(1:N, function(i) rbinom(J, 1, P[i,])))
  res_jml <- mirtjml_conf(data, Q, theta0, A0, d0, tol = 1e-3, cc = 2)
  cjmle_err[t] <- sin_theta(res_jml$A_hat, A)
  # standard_obj <- standardize(res_jml$A_hat, res_jml$d_hat, res_jml$theta_hat)
  # standard_obj$A_tilde
  # sin_theta(standard_obj$A_tilde, A)
  # standard_obj$theta_tilde
  colnames(data) <- c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                    "Item_6", "Item_7", "Item_8", "Item_9", "Item_10")

  res_mml <- mirt_coef(data, K)
  mhrm_err[t] <- sin_theta(res_mml$slope, A)
}

print(mean(mhrm_err))
print("--------")
print(mean(cjmle_err))
