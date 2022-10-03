library(ggplot2)
library(ggeasy)

plot_fix_J_regime <- function(n) {
  len_n <- length(n)
  cjmle_A <- cjmle_d <- cjmle_theta <- mhrm_A <- mhrm_d <- rep(0, len_n)
  # Extract data from existing .rds data
  for (idx in 1:len_n) {
    # print(n[idx])
    rds <- readRDS(paste("checkpoint/Fix-J[cc=2][N=", n[idx], "].rds", sep = ""))
    cjmle_A[idx] <- mean(rds["cjmle_err_A"])
    cjmle_d[idx] <- mean(rds["cjmle_err_d"])
    cjmle_theta[idx] <- mean(rds["cjmle_err_theta"])
    mhrm_A[idx] <- mean(rds["mhrm_err_A"])
    mhrm_d[idx] <- mean(rds["mhrm_err_d"])
  }
  ic_green <- "#00b384"
  amber <- "#FFBF00"
  # Plot cjmle & mirt slope estimator
  ggplot(NULL, aes(x = n)) +
    geom_line(aes(y = cjmle_A, colour = "CJMLE"), size = 1) +
    geom_line(aes(y = mhrm_A, colour = "MHRM"), size = 1) +
    geom_point(aes(y = cjmle_A), shape = 0, size = 2.5, color = ic_green) +
    geom_point(aes(y = mhrm_A), shape = 0, size = 2.5, color = amber) +
    scale_color_manual(name = "Estimator", values = c("CJMLE" = ic_green, "MHRM" = amber)) +
    scale_x_continuous(breaks = n) +
    labs(x = "Sample Size", y = "SinTheta Distances") +
    ggtitle("SinTheta Distance vs. Global Sample Size") +
    easy_center_title()
  ggsave("Fix-J[A].png")

  # Plot cjmle & mirt intercept estimator
  ggplot(NULL, aes(x = n)) +
    geom_line(aes(y = cjmle_d, colour = "CJMLE"), size = 1) +
    geom_line(aes(y = mhrm_d, colour = "MHRM"), size = 1) +
    geom_point(aes(y = cjmle_d), shape = 0, size = 2.5, color = ic_green) +
    geom_point(aes(y = mhrm_d), shape = 0, size = 2.5, color = amber) +
    scale_color_manual(name = "Estimator", values = c("CJMLE" = ic_green, "MHRM" = amber)) +
    scale_x_continuous(breaks = n) +
    labs(x = "Sample Size", y = "FNorm") +
    ggtitle("F-Norm vs. Global Sample Size") +
    easy_center_title()
  ggsave("Fix-J[d].png")

  return(0)
}

plot_fix_N_regime <- function(j) {
  len_j <- length(j)
  cjmle_A <- cjmle_d <- cjmle_theta <- mhrm_A <- mhrm_d <- rep(0, len_j)
  # Extract data from existing .rds data
  for (idx in 1:len_j) {
    # print(n[idx])
    rds <- readRDS(paste("checkpoint/Fix-N[cc=2][J=", j[idx], "].rds", sep = ""))
    cjmle_A[idx] <- mean(rds["cjmle_err_A"])
    cjmle_d[idx] <- mean(rds["cjmle_err_d"])
    cjmle_theta[idx] <- mean(rds["cjmle_err_theta"])
    mhrm_A[idx] <- mean(rds["mhrm_err_A"])
    mhrm_d[idx] <- mean(rds["mhrm_err_d"])
  }
  ic_green <- "#00b384"
  amber <- "#FFBF00"
  # Plot cjmle & mirt slope estimator
  ggplot(NULL, aes(x = j)) +
    geom_line(aes(y = cjmle_A, colour = "CJMLE"), size = 1) +
    geom_line(aes(y = mhrm_A, colour = "MHRM"), size = 1) +
    geom_point(aes(y = cjmle_A), shape = 0, size = 2.5, color = ic_green) +
    geom_point(aes(y = mhrm_A), shape = 0, size = 2.5, color = amber) +
    scale_color_manual(name = "Estimator", values = c("CJMLE" = ic_green, "MHRM" = amber)) +
    scale_x_continuous(breaks = j) +
    labs(x = "Number of Items", y = "SinTheta Distances") +
    ggtitle("SinTheta Distance vs. Number of Items") +
    easy_center_title()
  ggsave("Fix-N[A].png")

  # Plot cjmle & mirt intercept estimator
  ggplot(NULL, aes(x = j)) +
    geom_line(aes(y = cjmle_d, colour = "CJMLE"), size = 1) +
    geom_line(aes(y = mhrm_d, colour = "MHRM"), size = 1) +
    geom_point(aes(y = cjmle_d), shape = 0, size = 2.5, color = ic_green) +
    geom_point(aes(y = mhrm_d), shape = 0, size = 2.5, color = amber) +
    scale_color_manual(name = "Estimator", values = c("CJMLE" = ic_green, "MHRM" = amber)) +
    scale_x_continuous(breaks = j) +
    labs(x = "Number of Items", y = "FNorm") +
    ggtitle("F-Norm vs. Number of Items") +
    easy_center_title()
  ggsave("Fix-N[d].png")

  return(0)
}

# rds <- readRDS(paste("checkpoint/Fix-J[cc=2][N=2000].rds")
# a <- mean(rds['cjmle_err_A'])
# # a
N <- c(50, 100, 200, 500, 750, 1000, 2000, 3000)
# N[1]
J <- c(5, 10, 15, 20, 25, 30) 
plot_fix_J_regime(N)
plot_fix_N_regime(J)