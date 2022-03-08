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

fit_mirt <- function(data, num_latent_fac, d = 0, cols_info, CHECK_SINGLE_RESPONSE) {
  num_cols <- ncol(data) #nolint
  values <- mirt(data, num_latent_fac, method = "MHRM", TOL = 0.0001,
        itemtype = "Rasch", technical = list(MHDRAWS = 5, NCYCLES = 1e5,
        gain = c(1, 1)), pars = "values")
  # add identification constraint to parameter matrix.
  if (CHECK_SINGLE_RESPONSE) {
    values[2, 6] <- d[which(cols_info > 0)[1]]
    values[2, 9] <- values[4 * length(which(cols_info > 0)) + 2, 9] <- FALSE
  } else {
    values[2, 6] <- d[1]
    values[2, 9] <- values[42, 9] <- FALSE
  }
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
  counter <- 1
  for (col in 1:ncol(data)) {
    if (!any(data[, col] == 1)) {
      cols[[col]] <- -1
    } else if (!any(data[, col] == 0)) {
      cols[[col]] <- 0 # 0 represents that there is all one
    } else {
      cols[[col]] <- counter
      counter <- counter + 1
    }
  }
  return(list(cols_info = cols))
}

combine_estimation <- function(BOUNDS, cols_info, est, num_latent_fac, J) {
  #TODO: change the way of passing through BOUNDS information in the future.
  #      maybe find a way to resolve that outside this function.
  bound_est <- rep(BOUNDS, num_latent_fac)
  results <- list()
  for (idx in 1:J) {
    if (cols_info[idx] == 0) {
      results[[idx]] <- bound_est * 1
    } else if (cols_info[[idx]] == -1) {
      results[[idx]] <- bound_est * (-1)
    } else {
      results[[idx]] <- est[cols_info[[idx]]]
    }
  }
  return(estimator = matrix(results))
}

get_est <- function(data, num_latent_fac, J, d, CHECK_SINGLE_RESPONSE, BOUNDS) {
  if (CHECK_SINGLE_RESPONSE) {
    cols_info <- check_single_response(data)$cols_info
    data_trim <- data[, (cols_info > 0)]
    model <- fit_mirt(data = data_trim, num_latent_fac = num_latent_fac, d = d,
          cols_info = cols_info, CHECK_SINGLE_RESPONSE = CHECK_SINGLE_RESPONSE)
    partial_estimator <- matrix(coef(model, simplify = TRUE)[[1]][, 2]) #nolint
    estimator <- combine_estimation(BOUNDS = BOUNDS, cols_info = cols_info,
          est = partial_estimator, num_latent_fac = num_latent_fac, J = J)
  } else {
    model <- fit_mirt(data = data, num_latent_fac = num_latent_fac, d = d,
          cols_info = cols_info, CHECK_SINGLE_RESPONSE = CHECK_SINGLE_RESPONSE)
    estimator <- matrix(coef(model, simplify = TRUE)[[1]][, 2])
  }
  return(estimator = estimator)
}
# fit_mirt(data_trim, 1, d, col)
# data <- simdata(a = a, d = d, N = 10, itemtype = rep("2PL", J))
# col <- check_single_response(data)$cols
# mask <- col > 0
# mask
# data_trim <- data[, mask]
# data_trim
# values <- mirt(data_trim, num_latent_fac, method = "MHRM", TOL = 0.0001,
#         itemtype = "Rasch", technical = list(MHDRAWS = 5, NCYCLES = 1e5,
#         gain = c(1, 1)), pars = "values")
# model <- mirt(data_trim, num_latent_fac, method = "MHRM", itemtype = "Rasch",
#         TOL = 0.0001, verbose = FALSE, technical = list(NCYCLES = 1e5, MHDRAWS = 10, #nolint
#         gain = c(1, 1)), pars = values)
# coef(model)
# coef <- simplify2array(matrix(coef(model, simplify = TRUE)[[1]][, 2]))
# coef
# df <- data.frame(coef)
# df
# bounds <- rep(1, 1)
# results <- list()
# bounds
# combine_estimation(2, col, coef, 1, 10)
# d[which(col > 0)[1]]
# length(which(col > 0))
# col[col > 0][2]
# for (idx in 1:10) {
#   if (col[idx] == 0) {
#     results[[idx]] <- bounds * 1
#   } else if (col[[idx]] == -1) {
#     results[[idx]] <- bounds * (-1)
#   } else {
#     results[[idx]] <- coef[col[[idx]]]
#   }
# }
# matrix(results)

# TODO: implement a function to automatically check whether
#       we need to check single response based on probabilities.
# Simulation study setup
num_mc_iter <- 2 # Ground Truth: small-scale experiments
a <- matrix(1, 10, 1)
a
# d <- matrix(c(0, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1) #nolint
d <- matrix(c(5, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1)
# d <- matrix(c(0, -0.2, 0.05, -0.15, 0.10, -0.10, 0.15, -0.05, 0.20, 0), 10, 1) #nolint
# d <- matrix(c(0, -0.2, 0.05, -0.15, 0.10, -0.10, 0.15, -0.05, 0.20, 0), 10, 1)-0.02 #nolint
# TODO: draw samples from uniform distribution from -2 to 2.
d

#TODO: Implementing the pipeline for all models.
# Model specification
# For Rasch model, there is only one latent factor to extract.
num_latent_fac <- 1
print("Rasch Model Implementation using MIRT package")
# Global parameters
N <- 100 # global sample size
J <- 10 # number of items
m <- 5 # number of local machines
R <- 10 # amplification ratio
n <- N / m # local sample size
# Global boolean flag: whether to check single response
CHECK_SINGLE_RESPONSE <- TRUE #nolint
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
  # model <- fit_mirt(data, num_latent_fac)
  # Compute full-sample mirt estimator and fnorm
  cmirt_est[[i]] <- simplify2array(get_est(data, num_latent_fac, J,
                            d, CHECK_SINGLE_RESPONSE, EST_BOUND))
  # cmirt_est[[i]] <- matrix(coef(model, simplify = TRUE)[[1]][, 2]) #nolint
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
    # model <- fit_mirt(local_data, num_latent_fac)
    # TODO: After fit_mirt, re-combine all estimation together.
    # Extract model coefficients.
    # local_est_all[[l]] <- simplify2array(matrix(coef(model, simplify = TRUE)[[1]][, 2])) #nolint
    local_est_all[[l]] <- simplify2array(get_est(local_data, num_latent_fac, J,
                            d, CHECK_SINGLE_RESPONSE, EST_BOUND))
    print(local_est_all[[l]])
    # Extract local estimator (WLOG, we take the estimator of the first client)
    if (l == 1) {
      local_est[[i]] <- local_est_all[[l]]
      local_fnorm[[i]] <- norm((local_est[[i]] - d), "F")
    }
  }
  # Compute average estimators and errors.
  # avg_est[[i]] <- apply(simplify2array(local_est_all), 1:2, mean)
  avg_est[[i]] <- rowMeans(simplify2array(local_est_all))
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
  # reboot_est[[i]] <- matrix(coef(model, simplify = TRUE)[[1]][, 2]) #nolint
  reboot_est[[i]] <- simplify2array(get_est(local_data, num_latent_fac, J,
                            d, CHECK_SINGLE_RESPONSE, EST_BOUND))
  reboot_fnorm[[i]] <- norm((reboot_est[[i]] - d), "F")
}
get_final_results(cmirt_fnorm, local_fnorm, avg_fnorm, reboot_fnorm)