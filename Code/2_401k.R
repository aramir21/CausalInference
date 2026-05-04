# 401k: Treatment effects
rm(list = ls())

mydata <- read.csv("https://raw.githubusercontent.com/BEsmarter-consultancy/BSTApp/refs/heads/master/DataApp/401k.csv", sep = ",", header = TRUE, quote = "")
attach(mydata )
y <- net_tfa/1000
# net_tfa: net financial assets
# Regressors quantity including intercept
X <- cbind(e401, age, inc, fsize, educ, marr, twoearn, db, pira, hown)
# e401: 401k eligibility 
# age, income, family size, years of education, marital status indicator, two-earner status indicator, defined benefit pension status indicator, IRA participation indicator, and home ownership indicator.
# =============== CIA =============== #
regOLS <- lm(y ~ X)
summary(regOLS)
confint(regOLS, level = 0.95)

# Robust standard errors
coeftest(regOLS, vcov = vcovHC(regOLS, type = "HC1"))
# Robust results
rob <- coeftest(regOLS, vcov = vcovHC(regOLS, type = "HC1"))

# Extract coefficient and SE for Tr
beta <- rob["Xe401", "Estimate"]
se   <- rob["Xe401", "Std. Error"]

# 95% CI (normal approximation)
ci_lowerOLS <- beta - 1.96 * se
ci_upperOLS <- beta + 1.96 * se

c(ci_lowerOLS, ci_upperOLS)

# =============== Instrumental variable =============== #
y <- net_tfa/1000  # Outcome: net financial assets
x <- as.vector(p401) # Endogenous regressor: participation
w <- as.matrix(cbind(1, age, inc, fsize, educ, marr, twoearn, db, pira, hown))  # Exogenous regressors with intercept
z <- as.matrix(e401)  # Instrument: eligibility (NO intercept here)
X <- cbind(x, w); Z <- cbind(z, w)

Data <- data.frame(
  y = net_tfa / 1000,
  x = as.vector(p401),
  e401 = as.vector(e401),
  age = age,
  inc = inc,
  fsize = fsize,
  educ = educ,
  marr = marr,
  twoearn = twoearn,
  db = db,
  pira = pira,
  hown = hown
)

library(AER)
reg_ols <- lm(
  y ~ x + age + inc + fsize + educ + marr + twoearn + db + pira + hown, 
  data = Data
)
summary(reg_ols)

reg_iv <- ivreg(
  y ~ x + age + inc + fsize + educ + marr + twoearn + db + pira + hown |
    e401 + age + inc + fsize + educ + marr + twoearn + db + pira + hown,
  data = Data
)

summary(reg_iv, diagnostics = TRUE)
