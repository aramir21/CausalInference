rm(list = ls()); set.seed(10101)
library(dplyr); library(ggplot2)
# Parameters
n <- 1000; true_effect <- 2; replications <- 100
# Store results
results <- data.frame(rep = 1:replications, correct = numeric(replications), biased = numeric(replications))

for (r in 1:replications) {
  # Simulate data under DAG: S -> B -> Y, S -> Y, U -> B, U -> Y
  U <- rnorm(n, 0, 1); S <- rbinom(n, prob = 0.7, size = 1)
  B <- S + U + rnorm(n)
  Y <- S + B + 1.5*U + rnorm(n)
  # Correct model: does NOT condition on collider B
  model_correct <- lm(Y ~ S)
  # Biased model: conditions on collider B
  model_biased <- lm(Y ~ S + B)
  # Posterior means for S
  results$correct[r] <- model_correct$coefficients[2]
  results$biased[r] <- model_biased$coefficients[2]
}
# Compute bias
results <- results %>% mutate(
  bias_correct = correct - true_effect,
  bias_biased = biased - true_effect
)
# Average bias and SD
avg_bias <- results %>%
  summarise(
    mean_correct = mean(bias_correct),
    mean_biased = mean(bias_biased),
    sd_correct = sd(bias_correct),
    sd_biased = sd(bias_biased)
  )
print(avg_bias)

# Visualization: distribution of posterior means across 100 simulations
df_long <- results %>% dplyr::select(rep, correct, biased) %>% tidyr::pivot_longer(cols = c(correct, biased), names_to = "model", values_to = "estimate")

ggplot(df_long, aes(x = estimate, fill = model)) + geom_density(alpha = 0.5) + geom_vline(xintercept = true_effect, color = "black", linetype = "dashed", linewidth = 1) + labs(title = "Estimates Across 100 Simulations", x = expression(paste("Estimates of ", beta[S])), y = "Density") + theme_minimal(base_size = 14)
