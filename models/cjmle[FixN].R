library(mirt)
library(mirtjml)
library(Dict)

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
  values[which(values[, 4] == "d"), 6] <- 0
  # print(values)
  fit <- mirt(data, K, method = 'MHRM', itemtype = '2PL',
            technical = list(NCYCLES = 10000), pars = values)
  for (j in 1:J) {
    est_slope[j,] <- coef(fit)[[j]][1:K]
    est_intcp[j] <- coef(fit)[[j]][(K + 1)]
  }
  fscores <- fscores(fit, full.scores = TRUE)
  return(list(slope = est_slope, intcp = est_intcp, theta = fscores))
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
#==============================================================================#
#==============================================================================#
set.seed(2022)
# Simulation study for 2PL model
N <- 1000
K <- 2
mc <- 50
# d
# Check norm
C <- sqrt(K) * 5
# Fixed N Regime

J_list <- c(5, 10, 15, 20, 25, 30, 40)

for (J in J_list) {
  # Generate ground truth
  A <- matrix(runif((J * K), 0.5, 1.5), J, K)
  # A[6:10, 2] <- 0 # Impose constraint as in Cai's paper
  A[floor((J / 2) + 1):J] <- 0
  # A # A: J x K
  d <- rnorm(J, -0.5, 0.5) # d: J x 1
  D <- t(sapply(1:N, function(i) d)) # N x J
  # Capability parameters: N x K
  theta <- matrix(rnorm(N * K), N, K)
  sprintf("Default C is %f.", C)
  max_theta_norm <- max(theta_norm(theta))
  sprintf("Max norm of Theta is %f.", max_theta_norm)
  max_d_a_norm <- max(d_a_norm(d, A))
  sprintf("Max norm of A is %f.", max_d_a_norm)
  # Save ground truth
  gt_dict <- Dict$new(
  "A" = A,
  "d" = d,
  "J" = J,
  "K" = K,
  "C" = C,
  "max_theta_norm" = max_theta_norm,
  "max_d_a_norm" = max_d_a_norm
  )
  saveRDS(gt_dict, paste("checkpoint/Fix-N[1000]/gt_dict[Fix N][cc=2][J", J, "].rds", sep = ""))
  # Ground truth Product N x J
  gt_prod <- theta %*% t(A)
  # Calculate probabilities
  prob <- 1 / (1 + exp(-theta %*% t(A) - D)) # N x J
  # A0: Initial values of loading matrix
  A0 <- matrix(1, J, K)
  A0[J, 2] <- 0
  # d0: Initial values of intercepts
  d0 <- rep(0, J)
  # Q: Design matrix
  Q <- matrix(TRUE, J, K)
  Q[J, 2] <- FALSE # Impose PLT constraint here
  # theta0: Initial values of capability parameters
  theta0 <- matrix(rnorm(N * K), N, K)

  cjmle_err_A <- cjmle_err_Theta <- cjmle_err_d <- cjmle_err_prod <- rep(0, mc)
  mhrm_err_A <- mhrm_err_Theta <- mhrm_err_d <- mhrm_err_prod <- rep(0, mc)
  mhrm_err_prod_f <- cjmle_err_prod_f <- rep(0, mc)
  for (t in 1:mc) {
    print(sprintf("J = %d | Monte Carlo Simulation #%d.", J, t))
    # flush.console()
    # Generate data
    data <- t(sapply(1:N, function(i) rbinom(J, 1, prob[i,])))
    # Start estimation
    res_jml <- mirtjml_conf(data, Q, theta0, A0, d0, tol = 1e-3, cc = 2)
    cjmle_err_A[t] <- sin_theta(res_jml$A_hat, A)
    cjmle_err_Theta[t] <- sin_theta(res_jml$theta_hat, theta)
    cjmle_err_prod[t] <- sin_theta(gt_prod, res_jml$theta_hat %*% t(res_jml$A_hat))
    cjmle_err_prod_f[t] <- norm(gt_prod - res_jml$theta_hat %*% t(res_jml$A_hat), 'F') / sqrt(N * J)
    cjmle_err_d[t] <- norm(matrix(d - res_jml$d_hat), 'F')
    # No standardization needed
    # standard_obj <- standardize(res_jml$A_hat, res_jml$d_hat, res_jml$theta_hat)
    # standard_obj$A_tilde
    # sin_theta(standard_obj$A_tilde, A)
    # standard_obj$theta_tilde
    colnames(data) <- sapply(1:J, function(i) paste("Item_", i, sep = ""))
    res_mml <- mirt_coef(data, K)
    mhrm_err_A[t] <- sin_theta(res_mml$slope, A)
    mhrm_err_Theta[t] <- sin_theta(res_mml$theta, theta)
    mhrm_err_prod[t] <- sin_theta(gt_prod, res_mml$theta %*% t(res_mml$slope))
    mhrm_err_prod_f[t] <- norm(gt_prod - res_mml$theta %*% t(res_mml$slope), 'F') / sqrt(N * J)
    mhrm_err_d[t] <- norm(matrix(d - res_mml$intcp), 'F')
  }
  result_dict <- Dict$new(
    "cjmle_err_theta" = cjmle_err_Theta,
    "cjmle_err_A" = cjmle_err_A,
    "cjmle_err_d" = cjmle_err_d,
    "cjmle_err_prod" = cjmle_err_prod,
    "cjmle_err_prod_f" = cjmle_err_prod_f,
    "mhrm_err_A" = mhrm_err_A,
    "mhrm_err_d" = mhrm_err_d,
    "mhrm_err_theta" = mhrm_err_Theta,
    "mhrm_err_prod" = mhrm_err_prod,
    "mhrm_err_prod_f" = mhrm_err_prod_f,
    "N" = N,
    "mc" = mc)
  saveRDS(result_dict, paste("checkpoint/Fix-N[1000]/Fix-N[cc=2][J=", J, "].rds", sep = ""))
}
# mhrm_err_A
# ex <- readRDS("checkpoint/Fix-N[cc=2][J=30].rds") #nolint
# mean(ex['cjmle_err_A'])
# mean(ex['mhrm_err_A'])

# ex1 <- readRDS("checkpoint//Fix-N[cc=2][J=5].rds") #nolint
# mean(ex1['cjmle_err_A'])
# mean(ex1['mhrm_err_A'])

# test <- readRDS("checkpoint/Fix-J[N=500].rds")
# mean(test['mhrm_err_A'])
# mean(test['cjmle_err_A'])

# c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
#                     "Item_6", "Item_7", "Item_8", "Item_9", "Item_10")
# col <- sapply(1:J, function(i) paste("Item_", i, sep = ""))
# col
