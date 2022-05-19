library(mirt)
library(MASS)
set.seed(2022)

mse <- function(x) {
  return(sum(x ^ 2))
}

cache <- function(root_path) {
  saveRDS(error_surr, root_path)
  saveRDS(error_mirt, root_path)
}

J <- 50
R <- 5
N1 <- 2000 # Global Sample Size
N2 <- N1 * R # Sample size to estimate expectation
a <- matrix(1, J, 1)
mu <- 0.01
# d_star <- runif(J, 0, 0.2) #nolint
# d_star <- matrix(c(0, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1)
# d_star <- matrix(c(0.016, 0.008, -0.050, 0.003, 0.082, -0.177, 0.076, 0.119, 0.135, 0.053, # nolint
#             - 0.099, -0.045, 0.140, -0.124, -0.138, -0.187, -0.143, -0.025, 0.049, 0.042), 20, 1) # nolint
d_star <- matrix(c(0.016, 0.008, -0.050, 0.003, 0.082, -0.177, 0.076, 0.119, 0.135, 0.053, # nolint
            - 0.099, -0.045, 0.140, -0.124, -0.138, -0.187, -0.143, -0.025, 0.049, 0.042, #nolint
            - 0.195, -0.127, 0.161, 0.073, 0.002, -0.181, -0.130, 0.143, -0.022, -0.033, # nolint
            - 0.115, -0.115, 0.150, -0.046, 0.129, 0.012, -0.131, -0.133, 0.196, -0.135, # nolint
            - 0.108, -0.020, 0.180, -0.073, -0.088, 0.132, -0.041, 0.118, 0.097, -0.112), 50, 1) # nolint
# d_star
mse(d_star)
error_surr <- rep(0, 100)
error_mirt <- rep(0, 100)
mu_cand <- c(-0.10, -0.05, -0.04, -0.03, -0.02, -0.01, 0.01, 0.02, 0.03, 0.04, 0.05, 0.10)

for (mu_idx in 1:12) {
  mu <- mu_cand[mu_idx]
  print(paste("mu = ", mu))
  for (t in 1:50) {
    # UNCOMMENT the following if we want to generate data ourselves
    # X <- matrix(rep(rnorm(N1), times = J), N1, J) #nolint
    # D_star <- matrix(rep(d_star, each = N1), N1, J) #nolint
    # p <- 1 / (1 + exp(-(X - D_star))) #nolint
    # Y <- t(sapply(1:N1, function(i) rbinom(n = J, size = 1, prob = p[i,]))) #nolint
    Y <- simdata(a = a, d = d_star, N = N1, mu = mu, itemtype = rep("2PL", J))

    X_tilde <- matrix(rep(rnorm(N2, mean = 0, sd = 1), times = J), N2, J) # X_tilde dimension: N2 x J #nolint
    head(X_tilde)
    Y_tilde <- Y
    for (i in 1:(R - 1)) {
      Y_tilde <- rbind(Y_tilde, Y)
    }
    # The resulting Y_tilde is of dimension (N1 x R) x J = N2 x J
    I_d <- matrix(0, N2 * J, J)
    for (j in 1:J) {
      I_d[(((j - 1) * N2 + 1):(j * N2)), j] <- rep(1, N2)
    }
    # glm fit function
    # offset(): tells the formula that these parameters are not freely-estimated. #nolint
    # I_d: tells which ds are involved in the formula.
    # -1 : tells the formula that there is no intercept term.
    # binomial: default link function is logistic link, corresponding to our Rasch model. #nolint
    fit1 <- glm(c(Y_tilde) ~ offset(c(X_tilde)) + I_d - 1, family = binomial,
              control = list(maxit = 1000))
    # fit1 <- glm(c(Y_tilde) ~ c(X_tilde) + I_d - 1, family = binomial,
    #             control = list(maxit = 1000))
    d_fit1 <- coef(fit1)
    error_surr[t] <- mse(d_star - d_fit1)

    #--------------------------------------------------------------
    values <- mirt(data.frame(Y), 1, itemtype = "Rasch", pars = "values") # DEFAULT algorithm is "S-EM" #nolint
    values[201, 6] <- 0 # Adjust the mean vector
    fit2 <- mirt(data.frame(Y), 1, itemtype = "Rasch", pars = values, verbose = FALSE) #nolint
    # TRY MIRT WITH MHRM ALGORITHM
    # fit2 <- mirt(data.frame(Y), 1, method = "MHRM", TOL = 0.0001, itemtype = 'Rasch') #nolint
    d_fit2 <- sapply(1:J, function(j) coef(fit2)[[j]][2])
    error_mirt[t] <- mse(d_star - d_fit2)
  }
  print("Surrogate Loss MLE:")
  sur_ls <- round(mean(error_surr), 6)
  print(sur_ls)
  print("CMIRT:")
  cmirt <- round(mean(error_mirt), 6)
  print(cmirt)
  print(cmirt - sur_ls)
  d_fit2
}
