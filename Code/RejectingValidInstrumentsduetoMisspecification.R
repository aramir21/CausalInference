# ============================================================
# Valid instruments + MISSPECIFIED structural model
# True DGP: y = exp(beta1*x1 + beta2*x2) + u
# Estimated model: linear IV
# Add a VALID nonlinear instrument z3 = z1^2 - 1 to create extra restrictions
# ============================================================

set.seed(123)

if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(AER)
library(ggplot2)

simulate_once <- function(n = 2000,
                          beta1 = 1.2, beta2 = -0.8,   # make nonlinearity stronger
                          pi1 = 0.8, pi2 = 0.6,
                          ax = 0.9, au = 0.9) {
  
  # VALID instruments
  z1 <- rnorm(n)
  z2 <- rnorm(n)
  
  # extra VALID nonlinear instrument (function of z1)
  z3 <- z1^2 - 1
  
  # Exogenous regressor
  x2 <- rnorm(n)
  
  # latent factor creates endogeneity in x1
  v <- rnorm(n)
  
  # error correlated with v
  u <- au * v + rnorm(n)
  
  # endogenous regressor
  x1 <- pi1 * z1 + pi2 * z2 + ax * v + rnorm(n)
  
  # true nonlinear mean
  mu <- exp(beta1 * x1 + beta2 * x2)
  
  # outcome
  y <- mu + u
  
  # MISSPECIFIED linear IV
  # endogenous: x1 ; exogenous: x2
  # instruments: z1, z2, z3, and x2
  fit <- ivreg(y ~ x1 + x2 | z1 + z2 + z3 + x2)
  
  diag <- summary(fit, diagnostics = TRUE)$diagnostics
  J  <- as.numeric(diag["Sargan", "statistic"])
  pJ <- as.numeric(diag["Sargan", "p-value"])
  
  list(J = J, pJ = pJ)
}

# ----------------------------
# Monte Carlo
# ----------------------------
R <- 500
out <- replicate(R, simulate_once(), simplify = FALSE)

J_vals <- sapply(out, `[[`, "J")
p_vals <- sapply(out, `[[`, "pJ")

# df for J: q - p
# q = 4 instruments (z1,z2,z3,x2), p = 2 parameters (x1,x2)  => df = 2
df <- 2
crit_95 <- qchisq(0.95, df = df)

rej_rate_p <- mean(p_vals < 0.05, na.rm = TRUE)
rej_rate_J <- mean(J_vals > crit_95, na.rm = TRUE)

cat("\n=== OIR (Sargan/J) rejection proportions at 5% ===\n")
cat("Using p-values:       ", round(rej_rate_p, 3), "\n")
cat("Using J > crit value: ", round(rej_rate_J, 3), "\n")
cat("Critical value chi-square(", df, ") 95%: ", round(crit_95, 3), "\n\n")

df_J <- data.frame(J = J_vals)

# ----------------------------
# Plot: histogram + critical value line
# ----------------------------
ggplot(df_J, aes(x = J)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 35,
                 fill = "grey80",
                 color = "black") +
  geom_vline(xintercept = crit_95,
             linetype = "dashed",
             linewidth = 1) +
  labs(
    title = "Distribution of the J-test (Valid Instruments, Misspecified Linear IV)",
    subtitle = paste0("True DGP: exponential. Estimated model: linear IV.  Rejection rate (5%): ",
                      round(rej_rate_J, 3),
                      ". Dashed line: chi-square(", df, ") 95% critical value."),
    x = "J statistic",
    y = "Density"
  ) +
  theme_minimal(base_size = 13)
