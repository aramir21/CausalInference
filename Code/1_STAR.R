rm(list = ls())
set.seed(10101)
library(dplyr)
library(AER)
data("STAR")
STARorigin <- STAR

STAR <- STAR %>%
  mutate(
    schoolidk_raw = schoolidk,
    schoolidk = if_else(
      is.na(schoolidk_raw),
      NA_integer_,
      as.integer(factor(schoolidk_raw))
    )
  )

N <- nrow(STAR)
STAR$id <- 1:N
STAR$smallk <- ifelse(STAR$stark == "small", 1, 0)
table(STAR$smallk)
yV1 <- data.frame(STAR$id, STAR$readk)
yV2 <- na.omit(yV1)

# Individual-level X
library(stringr)

STAR <- STAR %>%
  mutate(
    birth = as.character(birth),
    
    ## Year and age
    birth_year = as.integer(str_extract(birth, "^\\d{4}")),
    age = 1985 - birth_year,
    
    ## Quarter dummies
    Q1 = as.integer(str_detect(birth, "Q1")),
    Q2 = as.integer(str_detect(birth, "Q2")),
    Q3 = as.integer(str_detect(birth, "Q3")),
    Q4 = as.integer(str_detect(birth, "Q4")),
    
    eth_cauc = as.integer(ethnicity == "cauc"),
    eth_afam = as.integer(ethnicity == "afam"),
    male     = as.integer(gender == "male"),
    female   = as.integer(gender == "female")
  )
attach(STAR)

XV1  <- data.frame(id, 1, female, eth_cauc, eth_afam, age, Q1, Q2, Q3)
XV2 <- na.omit(XV1)

# Treatment assignment
TrV1 <- data.frame(STAR$id, STAR$smallk)
TrV2 <- na.omit(TrV1)

# common ids
ids_common <- Reduce(intersect, list(
  # W_fullV2$id,
  XV2$id,
  yV2$STAR.id,
  TrV2$STAR.id
))

X <- XV2 %>%
  filter(id %in% ids_common) %>%
  arrange(id) %>%
  select(female, eth_cauc, eth_afam, age, Q1, Q2, Q3) 

# Ethnicity has as baseline other race (asian, hispanic, amindian,  other)
# Q4 is the baseline for quarters of birth
# Male is the baseline for female
X <- cbind(1, as.matrix(X))

y  <- yV2  %>% filter(STAR.id %in% ids_common) %>% arrange(STAR.id) %>% pull(STAR.readk)
Tr <- TrV2 %>% filter(STAR.id %in% ids_common) %>% arrange(STAR.id) %>% pull(STAR.smallk)

# =============== RCT =============== #
diff(tapply(y, Tr, mean))
t.test(y ~ Tr)

# Regression
regRCT <- lm(y ~ Tr)
summary(regRCT)
confint(regRCT, level = 0.95)

library(sandwich)
library(lmtest)
# Robust standard errors
coeftest(regRCT, vcov = vcovHC(regRCT, type = "HC1"))
# Robust results
rob <- coeftest(regRCT, vcov = vcovHC(regRCT, type = "HC1"))

# Extract coefficient and SE for Tr
beta <- rob["Tr", "Estimate"]
se   <- rob["Tr", "Std. Error"]

# 95% CI (normal approximation)
ci_lower <- beta - 1.96 * se
ci_upper <- beta + 1.96 * se

c(ci_lower, ci_upper)

# =============== RCT + Regressors =============== #
regOLS <- lm(y ~ Tr + X - 1)
summary(regOLS)
confint(regOLS, level = 0.95)

# Robust standard errors
coeftest(regOLS, vcov = vcovHC(regOLS, type = "HC1"))
# Robust results
rob <- coeftest(regOLS, vcov = vcovHC(regOLS, type = "HC1"))

# Extract coefficient and SE for Tr
beta <- rob["Tr", "Estimate"]
se   <- rob["Tr", "Std. Error"]

# 95% CI (normal approximation)
ci_lowerOLS <- beta - 1.96 * se
ci_upperOLS <- beta + 1.96 * se

c(ci_lowerOLS, ci_upperOLS)
