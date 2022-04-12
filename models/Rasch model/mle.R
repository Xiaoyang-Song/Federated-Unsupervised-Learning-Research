# Arthor: Xiaoyang Song
library(bspec)

x <- rnorm(10, 1, 1)
# print(x * density(x, n = 10)$y) #nolint
# print(x)
# print(sum(dnorm(x)))
# print(sum(x * dnorm(x)))
# print(mean(x < 0))
y <- 1
fn <- function(d) {
  return(integrate(ex, - Inf, Inf)$value)
}
ex <- function(x) {
  return((-y * (x - d) + log(1 + exp(x - d))) * dnorm(x))
}

print(nlm(fn, d))