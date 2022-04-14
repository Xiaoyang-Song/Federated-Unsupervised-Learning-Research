library(stats4)
library(mirt)

N <- 100
J <- 10
a <- matrix(1, 10, 1)
est_x_ratio <- 1000
d_star <- matrix(c(0, -0.5, 0.1, -0.4, 0.2, -0.3, 0.3, -0.2, 0.4, -0.1), 10, 1)

Y <- simdata(a = a, d = d_star, N = N, itemtype = rep("2PL", J))

nll <- function(d) {
  loss <- 0
  for (i in 1:N) {
    for (j in 1:J) {
      x <- rnorm(est_x_ratio, 0, 1)
      loss <- loss + mean(-Y[i, j] * (x - d[j]) + log(1 + exp(x - d[j])))
    }
  }
  return(loss)
}
# the vector below are starting values of estimation
# nlm() uses Newton-type minimization algorithm.
nlm_obj <- nlm(nll, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
print(nlm_obj)