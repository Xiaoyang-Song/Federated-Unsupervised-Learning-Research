library(stats4)
library(mirt)

N <- 200
J <- 10
a <- matrix(1, 10, 1)
est_x_ratio <- 1000
d_star <- matrix(c(0, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1)
# d_star <- runif(3, -0.1, 0.1)
d_star
# d_star <- matrix(c(0, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1)
# d_star <- matrix(c(0.016, 0.008, -0.050, 0.003, 0.082, -0.177, 0.076, 0.119, 0.135, 0.053, # nolint
#             - 0.099, -0.045, 0.140, -0.124, -0.138, -0.187, -0.143, -0.025, 0.049, 0.042), 20, 1) # nolint
# d_star <- matrix(c(0.016, 0.008, -0.050, 0.003, 0.082, -0.177, 0.076, 0.119, 0.135, 0.053, # nolint
#             - 0.099, -0.045, 0.140, -0.124, -0.138, -0.187, -0.143, -0.025, 0.049, 0.042, #nolint
#             - 0.195, -0.127, 0.161, 0.073, 0.002, -0.181, -0.130, 0.143, -0.022, -0.033, # nolint
#             - 0.115, -0.115, 0.150, -0.046, 0.129, 0.012, -0.131, -0.133, 0.196, -0.135, # nolint
#             - 0.108, -0.020, 0.180, -0.073, -0.088, 0.132, -0.041, 0.118, 0.097, -0.112), 50, 1) # nolint
# d_star
Y <- simdata(a = a, d = d_star, N = N, itemtype = rep("2PL", J))

nll <- function(d) {
  loss <- 0
  for (i in 1:N) {
    x <- rnorm(est_x_ratio, 0, 1)
    for (j in 1:J) {
      loss <- loss + mean(-Y[i, j] * (x - d[j]) + log(1 + exp(x - d[j])))
    }
  }
  return(loss)
}
# the vector below are starting values of estimation
# nlm() uses Newton-type minimization algorithm.
nlm_obj <- nlm(nll, p = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), hessian = TRUE, iterlim = 100, print.level = 2) #nolint
# print(glm_obj)
# nlm_obj <- nlm(nll, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
print(nlm_obj)
print(coef(nlm_obj))

