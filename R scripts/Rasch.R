library(mirt)
library(stats4)
library(lattice)

sin_theta <- function(A1, A2) {
  v1 <- svd(A1)$u
  v2 <- svd(A2)$u
  return(norm((v1 %*% t(v1) - v2 %*% t(v2)), "F"))
}

extract_model_configuration <- function() {
  return(list(1, 2, 3))
}
# Model specification
# For Rasch model, there is only one latent factor to extract.
num_latent_fac <- 1
num_mc_iter <- 100

print("Rasch Model Implementation using MIRT package")
# Ground Truth: small-scale experiments
a <- matrix(1, 10, 1)
d <- matrix(c(0, 1, 2, 3, 4, 0, -1, -2, -3, -4), 10, 1)
# Data is generated using 2-parameter logistic model, where a is fixed to be 1.
data <- simdata(a = a, d = d, N = 100, itemtype = rep("2PL", 10))
values <- mirt(data, num_latent_fac, method = "MHRM", TOL = 0.0001,
        itemtype = "Rasch", technical = list(MHDRAWS = 1, NCYCLES = 1e5,
        gain = c(1, 1)), pars = "values")
model <- mirt(data, num_latent_fac, method = "MHRM", itemtype = "Rasch",
        TOL = 0.0001, technical = list(NCYCLES = 1e5, MHDRAWS = 1,
        gain = c(1, 1)), pars = values)

