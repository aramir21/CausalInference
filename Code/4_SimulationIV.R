rm(list = ls())
set.seed(10101)

library(MASS)
library(AER)

# Parameters
N <- 1000
k <- 2
S <- 100

B <- rep(1, k)          # Structural coefficients
G <- rep(1, 3)          # First-stage coefficients
s12 <- 0.8

SIGMA <- matrix(c(1, s12,
                  s12, 1), 2, 2)

# Instruments
z1 <- rnorm(N)
z2 <- rnorm(N)

# IV Overidentified
# Draw errors
U <- MASS::mvrnorm(n = N, mu = c(0, 0), Sigma = SIGMA)

# Data generating process
x <- G[1] + G[2] * z1 + G[3] * z2 + U[, 2]
y <- B[1] + B[2] * x + U[, 1]

Data <- data.frame(y = y, x = x, z1 = z1, z2 = z2)

# OLS
reg_lm <- lm(y ~ x, data = Data)
summary(reg_lm)

# IV
reg_iv <- ivreg(y ~ x | z1 + z2, data = Data)
summary(reg_iv, diagnostics = TRUE)

# Storage
beta_lm <- matrix(NA, S, 2)
beta_iv <- matrix(NA, S, 2)

colnames(beta_lm) <- c("Intercept", "x")
colnames(beta_iv) <- c("Intercept", "x")

for (s in 1:S) {
  
  # Draw errors
  U <- MASS::mvrnorm(n = N, mu = c(0, 0), Sigma = SIGMA)
  
  # Data generating process
  x <- G[1] + G[2] * z1 + G[3] * z2 + U[, 2]
  y <- B[1] + B[2] * x + U[, 1]
  
  Data <- data.frame(y = y, x = x, z1 = z1, z2 = z2)
  
  # OLS
  reg_lm <- lm(y ~ x, data = Data)
  beta_lm[s, ] <- coef(reg_lm)
  
  # IV
  reg_iv <- ivreg(y ~ x | z1 + z2, data = Data)
  beta_iv[s, ] <- coef(reg_iv)
}

# Results
head(beta_lm)
head(beta_iv)

# Average estimates
colMeans(beta_lm)
colMeans(beta_iv)

library(ggplot2)
library(latex2exp)

# Extract slope (coefficient on x)
postmeans <- c(beta_lm[, 2], beta_iv[, 2])

Model <- c(rep("OLS", S), rep("IV", S))

df <- data.frame(postmeans = postmeans, Model = Model)

histExo <- ggplot(df, aes(x = postmeans, fill = Model)) +
  geom_histogram(bins = 40, position = "identity", 
                 color = "black", alpha = 0.5) +
  
  scale_fill_manual(values = c("blue", "red")) +
  
  # Mean OLS
  geom_vline(xintercept = mean(beta_lm[, 2]),
             color = "blue", linewidth = 1, linetype = "dashed") +
  
  # Mean IV
  geom_vline(xintercept = mean(beta_iv[, 2]),
             color = "red", linewidth = 1, linetype = "dashed") +
  
  # True value
  geom_vline(xintercept = B[2],
             color = "green", linewidth = 1.2, linetype = "dashed") +
  
  labs(title = "Distribution of Estimators",
       x = TeX("$\\hat{\\beta}_2$"),
       y = "Frequency") +
  
  theme_minimal()

histExo
