library(mirt)
library(Rlab)
library(MASS)
# set.seed(2022)
qr_decomp <- function(target) {
  QRdecomp <- qr(target)
  R <- qr.R(QRdecomp)
  Q <- qr.Q(QRdecomp)
  sgn <- sign(diag(R))
  R <- diag(sgn) %*% R
  Q <- Q %*% diag(sgn)
  return(list(R = t(R), Q = Q))
}
sinTheta <- function(A1, A2) {
  V1 <- svd(A1)$u
  V2 <- svd(A2)$u
  return(norm((V1 %*% t(V1) - V2 %*% t(V2)), "F"))
}
mse <- function(x) {
  return(sum(x ^ 2))
}
fnorm <- function(x) {
  return(sum(x ^ 2) ^ 0.5)
}
Y2Ytilde <- function(Y) {
  Y_tilde <- Y
  for (i in 1:(R - 1)) {
    Y_tilde <- rbind(Y_tilde, Y)
  }
  return(Y_tilde)
}

J <- 10
K <- 2
N <- 10000
R <- 100
M <- N * R
mc <- 1
error_surr <- rep(0, 100)
error_mirt <- rep(0, 100)
# For now, there is no intercept term involved.
# a <- matrix(c(2.2, 2.0, 2.6, -1.4, -1.2, 0, 0, 0, 3.2, 1.7), J, K) # J x K #nolint
# a
a_2d <- matrix(t(a), byrow = TRUE, nrow = J)
a_2d
# fnorm(a)
# a_est <- matrix(c(2.3, 1.97, 2.44, -1.34, -1.21, 0.0, 0.07, 0.12, 3.10, 1.74), 5, 2)
# l2(a_est - a)
a <- matrix(c(2.20, 2.00, 2.60, 1.60, 1.70, 1.80, 1.80, 1.90, 1.60, 1.70,
              0, 0, 0, 0, 0, 1.2, 1.1, 1.2, 2.1, 1.5), 10, 2)
a
fnorm(a)
# d <- matrix(c(0.67, 1.09, -0.18, -0.76, 0.5, -0.41, -0.07, 1.15, 0.13, -1.10,
#               -0.72, -0.14, -1.22, -1.42, -0.36, -1.26, -0.96, -0.09, -0.70, -1.56), 10, 2)
# d <- matrix(c(1.1, 0.2, 0.4, 0.2, -0.5), J, 1)
for (t in 1:mc) {
  # TODO: rewrite this portion
  # Generate data
  X <- matrix(rnorm(N * K), N, K) # Factor score matrix: N x K #nolint
  prob <- 1 / (1 + exp(-(X %*% t(a))))
  # Y <- t(sapply(1:N, function(i) rbern(n = J, prob = prob[i,]))) #nolint
  Y <- t(sapply(1:N, function(i) rbinom(n = J, size = 1, prob = prob[i,]))) #nolint
  # Y <- simdata(a = a, d = d, N = N, itemtype = rep("graded", J)) #nolint
  # Y
  # TODO: rewrite this portion to make it more efficient.
  X_tilde <- matrix(rnorm(M * K), M, K) # M x K
  # head(X_tilde)
  # dim(X_tilde)
  Y_tilde <- Y2Ytilde(Y) # M x J
  # Y_tilde
  # dim(Y_tilde)
  # TODO: fix bugs

  ld_mat <- matrix(0, M * J, J * K)
  # dim(ld_mat)
  for (j in 1:J) {
    ld_mat[(((j - 1) * M + 1):(j * M)), (((j - 1) * K + 1):(j * K))] <- X_tilde
    print(dim(ld_mat[(((j - 1) * M + 1):(j * M)), (((j - 1) * K + 1):(j * K))])) #nolint
  }
  ld_mat
  # dim(ld_mat)
  # dim(t(Y_tilde))
  # c(t(Y_tilde))
  # c(Y_tilde)
  sl_fit <- glm(c(t(Y_tilde)) ~ ld_mat - 1, family = binomial,
              control = list(maxit = 10000))
  coef(sl_fit)
  sl_fit_coef <- matrix(coef(sl_fit))
  sl_fit_coef
  sl_fit_coef_2d <- matrix(sl_fit_coef, nrow = 10, byrow = TRUE)
  # sl_fit_coef_2d
  sl_fit_coef_qr <- qr_decomp(t(sl_fit_coef_2d))$R
  sl_fit_coef_qr
  error_surr[t] <- fnorm(a_2d - sl_fit_coef_qr)
  # fnorm(a_2d - sl_fit_coef_qr)
  sinTheta(sl_fit_coef_qr, a)
  # error_surr[t]
  values <- mirt(data.frame(Y), 2, itemtype = "graded", method = "MHRM",
                    technical = list(NCYCLES = 10000), pars = "values")
  print(values)
  values[2, 6] <- 0
  values[2, 9] <- FALSE
  for (j in 1:J) {
    values[3 * j, 6] <- 0
    values[3 * j, 9] <- FALSE
  }
  values[29, 9] <- TRUE
  values
  # dim(Y)
  # print(values)
  mirt_fit <- mirt(data.frame(Y), 2, itemtype = "graded", method = "MHRM", TOL = 0.0001, #nolint
                 technical = list(NCYCLES = 10000), pars = values)
  mirt_fit_coef <- simplify2array(matrix(coef(mirt_fit, simplify = TRUE)$items[, 1:2], nrow = J, ncol = K)) #nolint
  # mirt_fit_coef
  mirt_fit_coef_qr <- qr_decomp(t(mirt_fit_coef))$R
  mirt_fit_coef_qr
  fnorm(mirt_fit_coef_qr - a)
  sinTheta(mirt_fit_coef_qr, a)
  error_mirt[t] <- fnorm(mirt_fit_coef_qr - a)
}
print("Surrogate Loss MLE:")
sur_ls <- round(error_surr / mc, 6)
print(sur_ls)
print("CMIRT:")
cmirt <- round(error_mirt / mc, 6)
print(cmirt)
