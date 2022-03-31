library(mirt)
library(stats4)
library(lattice)

sin_theta <- function(a1, a2) {
  v1 <- svd(a1)$u
  v2 <- svd(a2)$u
  return(norm((v1 %*% t(v1) - v2 %*% t(v2)), "F"))
}

model_config <- function(n, j, p, model, mc) {
  return(Dict$new(
      num_test_taker = n,
      num_items = j,
      num_latent_fac = p,
      model = model,
      num_mc_iter = mc
  ))
}

fit_mirt <- function(data, num_latent_fac, d = 0) {
  num_cols <- ncol(data) #nolint
  values <- mirt(data, num_latent_fac, method = "MHRM", TOL = 0.0001,
        itemtype = "Rasch", technical = list(MHDRAWS = 5, NCYCLES = 1e5,
        gain = c(1, 1)), pars = "values")
  # TODO: automatic set the constraint based on input data.
  # add identification constraint to parameter matrix.
  values[2, 6] <- 0.016
  # values[2, 9] <- values[42, 9] <- FALSE
  values[2, 9] <- FALSE
  model <- mirt(data, num_latent_fac, method = "MHRM", itemtype = "Rasch",
        TOL = 0.0001, verbose = FALSE, technical = list(NCYCLES = 1e5, MHDRAWS = 10, #nolint
        gain = c(1, 1)), pars = values)
  return(model)
}

get_final_results <- function(cmirt_fnorm, local_fnorm,
                              avg_fnorm, reboot_fnorm) {
  print("Full-Sample Estimator:")
  print(mean(cmirt_fnorm))
  print("Local Estimator:")
  print(mean(local_fnorm))
  print("Average Estimator:")
  print(mean(avg_fnorm))
  print("Reboot Estimator:")
  print(mean(reboot_fnorm))
}

check_single_response <- function(data) {
  cols <- list()
  for (col in 1:ncol(data)) {
    if (!any(data[, col] == 1)) {
      cols[[col]] <- -1
    } else if (!any(data[, col] == 0)) {
      cols[[col]] <- 1
    } else {
      cols[[col]] <- 0
    }
  }
  return(list(cols = cols))
}

# TODO: implement a function to automatically check whether
#       we need to check single response based on probabilities.
# Simulation study setup
num_mc_iter <- 25 # Ground Truth: small-scale experiments
a <- matrix(1, 50, 1)
a
# d <- matrix(c(0, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1) #nolint
# d <- matrix(c(0, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1) #nolint
# d <- runif(50, -0.2, 0.2) #nolint
# d <- matrix(c(0, -0.2, 0.05, -0.15, 0.10, -0.10, 0.15, -0.05, 0.20, 0), 10, 1) #nolint
# d <- matrix(c(0, -0.2, 0.05, -0.15, 0.10, -0.10, 0.15, -0.05, 0.20, 0), 10, 1)-0.02 #nolint
# TODO: draw samples from uniform distribution from -2 to 2.
# d <- matrix(c(-0.1945, 0.1705, -0.0439, 0.0978, 0.1674, -0.1462,
#                 0.07, -0.1645, 0.124, -0.04), 10, 1)
# d <- round(d, 3) #nolint
d <- matrix(c(0.016, 0.008, -0.050, 0.003, 0.082, -0.177, 0.076, 0.119, 0.135, 0.053, # nolint
            - 0.099, -0.045, 0.140, -0.124, -0.138, -0.187, -0.143, -0.025, 0.049, 0.042, # nolint
            - 0.195, -0.127, 0.161, 0.073, 0.002, -0.181, -0.130, 0.143, -0.022, -0.033, # nolint
            - 0.115, -0.115, 0.150, -0.046, 0.129, 0.012, -0.131, -0.133, 0.196, -0.135, # nolint
            - 0.108, -0.020, 0.180, -0.073, -0.088, 0.132, -0.041, 0.118, 0.097, -0.112), 50, 1) # nolint
# d <- matrix(d, 10, 5)
# t(d)
#TODO: Implementing the pipeline for all models.
# Model specification
# For Rasch model, there is only one latent factor to extract.
# set.seed(2022) #nolint
num_latent_fac <- 1
print("Rasch Model Implementation using MIRT package")
# Global parameters
N <- 5000 # global sample size
J <- 50 # number of items
m <- 2 # number of local machines
R <- 10 # amplification ratio
n <- N / m # local sample size
# Global boolean flag: whether to check single response
CHECK_SINGLE_RESPONSE <- FALSE #nolint
EST_BOUND <- 2 # nolint
# REBOOT algorithm
reboot_fnorm <- avg_fnorm <- local_fnorm <- cmirt_fnorm <- rep(0, num_mc_iter) # nolint
reboot_est <- avg_est <- local_est <- cmirt_est <- list() # nolint
for (i in 1:num_mc_iter) {
  print(paste("Monte Carlo Iteration", i))
  # Generate data
  # Data is generated using 2-parameter logistic model (slope is fixed to be 1).
  data <- simdata(a = a, d = d, N = N, itemtype = rep("2PL", J))
  # Compute centralized MIRT estimator.
  print("CMIRT COMPUTATION...")
  model <- fit_mirt(data, num_latent_fac)
  # Compute full-sample mirt estimator and fnorm
  cmirt_est[[i]] <- matrix(coef(model, simplify = TRUE)[[1]][, 2]) #nolint
  cmirt_fnorm[[i]] <- norm((cmirt_est[[i]] - d), "F")
  # Start distributed setting.
  local_est_all <- list()
  for (l in 1:m) {
    print(paste("MIRT on Machine ", l))
    # Evenly split data into m local machine.
    local_data <- data[(1 + (l - 1) * n):(n * l),]
    # TODO: Add boolean logic to check whether need to check single response.
    #       This is required for fast computation when running large scale exps.
    # TODO: If CHECK_SINGLE_RESPONSE is true, process the data before fit_mirt.
    # TODO: Set all "singular" items to +/- EST_BOUND based on its response.
    # Fit the mirt model
    model <- fit_mirt(local_data, num_latent_fac)
    # TODO: After fit_mirt, re-combine all estimation together.
    # Extract model coefficients.
    local_est_all[[l]] <- simplify2array(matrix(coef(model, simplify = TRUE)[[1]][, 2])) #nolint
    # Extract local estimator (WLOG, we take the estimator of the first client)
    if (l == 1) {
      local_est[[i]] <- local_est_all[[l]]
      local_fnorm[[i]] <- norm((local_est[[i]] - d), "F")
    }
  }
  # Compute average estimators and errors.
  avg_est[[i]] <- apply(simplify2array(local_est_all), 1:2, mean)
  avg_fnorm[[i]] <- norm((avg_est[[i]] - d), "F")
  # Generate Bootstrap samples.
  reboot_data <- list()
  for (l in 1:m) {
    reboot_data[[l]] <- simdata(a = a, d = local_est_all[[l]], N = n * R, itemtype = rep("2PL", J)) #nolint
  }
  reboot_data <- as.data.frame(do.call(rbind, reboot_data))
  # Final fit using Reboot data.
  model <- fit_mirt(reboot_data, num_latent_fac)
  # Extract reboot estimator and compute F-norm.
  reboot_est[[i]] <- matrix(coef(model, simplify = TRUE)[[1]][, 2]) #nolint
  reboot_fnorm[[i]] <- norm((reboot_est[[i]] - d), "F")
}

get_final_results(cmirt_fnorm, local_fnorm, avg_fnorm, reboot_fnorm)