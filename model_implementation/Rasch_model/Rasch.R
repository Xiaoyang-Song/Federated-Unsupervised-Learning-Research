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

fit_mirt <- function(data, num_latent_fac) {
  values <- mirt(data, num_latent_fac, method = "MHRM", TOL = 0.0001,
        itemtype = "Rasch", technical = list(MHDRAWS = 5, NCYCLES = 1e5,
        gain = c(1, 1)), pars = "values")
  # add identification constraint to parameter matrix.
  values[2, 6] <- 0
  values[2, 9] <- FALSE
  model <- mirt(data, num_latent_fac, method = "MHRM", itemtype = "Rasch",
        TOL = 0.0001, verbose = FALSE, technical = list(NCYCLES = 1e5, MHDRAWS = 5, #nolint
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
# Simulation study setup
num_mc_iter <- 2 # Ground Truth: small-scale experiments
a <- matrix(1, 10, 1)
a
d <- matrix(c(0, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1)
d
# Model specification
num_latent_fac <- 1 # For Rasch model, there is only one latent factor to extract.
print("Rasch Model Implementation using MIRT package")
# Global parameters
N <- 1000
J <- 10
m <- 10
p <- 1
R <- 10 # Amplification ratio
n <- N / m # local sample size

# REBOOT algorithm
reboot_fnorm <- avg_fnorm <- local_fnorm <- cmirt_fnorm <- rep(0, num_mc_iter) # nolint
reboot_est <- avg_est <- local_est <- cmirt_est <- list() # nolint
for (i in 1:num_mc_iter) {
  print(paste("Monte Carlo Iteration", i))
  # Generate data
  # Data is generated using 2-parameter logistic model (slope is fixed to be 1).
  data <- simdata(a = a, d = d, N = N, itemtype = rep("2PL", J))
  # Compute centralized MIRT estimator.
  model <- fit_mirt(data, num_latent_fac)
  # Compute full-sample mirt estimator and fnorm
  cmirt_est[[i]] <- matrix(coef(model, simplify = TRUE)[[1]][, 2]) #nolint
  cmirt_fnorm[[i]] <- norm((cmirt_est[[i]] - d), "F")
  # Start distributed setting.
  local_est_all <- list()
  for (l in 1:m) {
    # Split data into m local machine.
    local_data <- data[(1 + (l - 1) * n):(n * l),]
    # Fit the mirt model
    model <- fit_mirt(local_data, num_latent_fac)
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