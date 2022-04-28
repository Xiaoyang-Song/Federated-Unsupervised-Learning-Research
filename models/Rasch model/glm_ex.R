library(mirt)
library(MASS)

print("Hello glm")

# Explore GLM package: glm example
Y <- c(1, 1, 1, 1, 1)
X <- rnorm(5, 0, 1)
I <- matrix(c(0, 1, 0, 0, 1, 0, 0, 1, 1, 1), 5, 2)
fit <- glm(c(Y) ~ offset(c(X)) + I - 1, family = binomial,
              control = list(maxit = 500))
coef(fit)
