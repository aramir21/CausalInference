set.seed(123)

N <- 1000
Z <- runif(N, -1, 1)              # running variable
cutoff <- 0

# Treatment probability jumps at cutoff (fuzzy)
p <- 0.2 + 0.5 * (Z >= cutoff)
D <- rbinom(N, 1, p)

# Outcome (true effect = 2)
tau <- 2
Y <- 1 + 2*Z + tau*D + rnorm(N)

data <- data.frame(Y, D, Z)

library(rdrobust)

rd_out <- rdrobust(
  y = data$Y,
  x = data$Z,
  fuzzy = data$D,
  c = 0
)

summary(rd_out)

rdplot(
  y = data$Y,
  x = data$Z,
  c = 0,
  title = "Fuzzy RD: Outcome"
)

rdplot(
  y = data$D,
  x = data$Z,
  c = 0,
  title = "First Stage (Treatment probability)"
)
