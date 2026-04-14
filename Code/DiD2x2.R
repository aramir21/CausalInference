# install.packages("did")
library(did)

set.seed(123)

# ----------------------------
# 1. Simulate a 2x2 DID panel
# ----------------------------
N <- 500

id <- 1:N
D  <- rbinom(N, 1, 0.5)   # treated group indicator
X  <- rnorm(N)            # covariate

# two periods: t=1 (pre), t=2 (post)
panel <- data.frame(
  id   = rep(id, each = 2),
  t    = rep(c(1, 2), times = N),
  D    = rep(D, each = 2),
  X    = rep(X, each = 2)
)

# g = first treatment period
# treated units first treated in period 2
# never-treated units coded as 0
panel$g <- ifelse(panel$D == 1, 2, 0)

# outcome:
# untreated outcome has group effect + time effect + covariate effect
# treatment effect turns on only for treated units in post period
tau <- 2

panel$y0 <- 5 +
  1.0 * panel$D +          # permanent group difference
  1.5 * (panel$t == 2) +   # common time trend
  1.0 * panel$X +
  rnorm(nrow(panel), 0, 1)

panel$y <- panel$y0 + tau * (panel$D == 1 & panel$t == 2)

# ----------------------------
# 2. Estimate ATT(g,t)
# ----------------------------
attgt_out <- att_gt(
  yname   = "y",
  tname   = "t",
  idname  = "id",
  gname   = "g",
  xformla = ~ X,      # optional covariates
  data    = panel,
  panel   = TRUE
)

summary(attgt_out)
