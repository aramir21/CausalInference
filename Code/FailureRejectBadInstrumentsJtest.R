library(AER)
library(ggplot2)

simulate_once <- function(n = 300, beta = 1,
                          pi1 = 0.30, pi2 = 0.30,
                          ax = 0.70, aZ = 0.08, au = 0.60) {
  
  v  <- rnorm(n)
  z1_clean <- rnorm(n)
  z2_clean <- rnorm(n)
  
  # Endogenous instruments
  z1 <- z1_clean + aZ * v
  z2 <- z2_clean - aZ * v
  
  u  <- au * v + rnorm(n)
  x  <- pi1 * z1 + pi2 * z2 + ax * v + rnorm(n)
  y  <- beta * x + u
  
  fit <- ivreg(y ~ x | z1 + z2)
  diag <- summary(fit, diagnostics = TRUE)$diagnostics
  
  list(
    J  = as.numeric(diag["Sargan", "statistic"]),
    pJ = as.numeric(diag["Sargan", "p-value"])
  )
}

set.seed(123)

R <- 500
out <- replicate(R, simulate_once(), simplify = FALSE)

J_vals <- sapply(out, `[[`, "J")
df_J <- data.frame(J = J_vals)

# Degrees of freedom for J-test: q - p = 2 - 1 = 1
library(ggplot2)

# df for the J-test: q - p
q <- 3
p <- 2
df_Jtest <- q - p        # e.g. 1
crit_95  <- qchisq(0.95, df = df_Jtest)

ggplot(df_J, aes(x = J)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "grey70",
                 color = "black") +
  geom_vline(xintercept = crit_95,
             linetype = "dashed",
             linewidth = 1) +
  labs(
    title = "Distribution of the J-test",
    subtitle = paste0(
      "Dashed line: chi-square(", df_Jtest, ") critical value at 5%"
    ),
    x = "J statistic",
    y = "Density"
  ) +
  theme_minimal(base_size = 13)

