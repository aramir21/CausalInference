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

J <- nlevels(factor(STAR$schoolidk_raw))

N <- nrow(STAR)
STAR$id <- 1:N
STAR$smallk <- ifelse(STAR$stark == "small", 1, 0)
table(STAR$smallk)
yV1 <- data.frame(STAR$id, STAR$readk)
yV2 <- na.omit(yV1)

# Individual-level X
library(dplyr)
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

# Z for heterogeneous treatment effect: intercept + x1 only
ZV1  <- cbind(id, 1, female, eth_cauc, eth_afam)   
ZV2 <- na.omit(ZV1) 

# Study-level covariates W_j   (Kw = 3 here)
library(tidyr)
library(forcats)
library(rlang)

STAR2 <- STAR %>%
  mutate(
    tethnicityk = fct_na_value_to_level(tethnicityk, level = "NA"),
    ladderk     = fct_na_value_to_level(ladderk,     level = "NA"),
    degreek     = fct_na_value_to_level(degreek,     level = "NA"),
    schoolk     = fct_na_value_to_level(schoolk,     level = "NA")
  )

prop_wide <- function(data, var, prefix){
  var_sym <- rlang::sym(var)
  lvls <- levels(data[[var]])
  
  data %>%
    count(schoolidk, !!var_sym, name = "n") %>%
    group_by(schoolidk) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    complete(schoolidk, !!var_sym := lvls, fill = list(n = 0, prop = 0)) %>%
    select(schoolidk, !!var_sym, prop) %>%   # <<< critical
    pivot_wider(
      id_cols = schoolidk,                   # <<< critical
      names_from  = !!var_sym,
      values_from = prop,
      names_prefix = prefix,
      values_fill  = 0
    )
}

teacher_stats_wide <- STAR2 %>%
  group_by(schoolidk) %>%
  summarise(avg_experience = mean(experiencek, na.rm = TRUE), .groups = "drop") %>%
  left_join(prop_wide(STAR2, "tethnicityk", "prop_eth_"),    by = "schoolidk") %>%
  left_join(prop_wide(STAR2, "ladderk",     "prop_ladder_"), by = "schoolidk") %>%
  left_join(prop_wide(STAR2, "degreek",     "prop_degree_"), by = "schoolidk") %>% 
  left_join(prop_wide(STAR2, "schoolk",     "prop_schoolk_"),by = "schoolidk")

summary(teacher_stats_wide)

W_study <- na.omit(teacher_stats_wide)
W_study <- W_study[,-c(3, 6, 13, 18)]


# Expand W to individual level (so we can store it in Data)
STAR <- STAR %>%
  mutate(schoolidk = as.character(schoolidk))

W_study <- W_study %>%
  mutate(schoolidk = as.character(schoolidk))

STAR_merged <- STAR %>%
  left_join(W_study, by = "schoolidk")


W_fullV1 <- STAR_merged[, c(49, 44, 61:77)]   
W_fullV2 <- na.omit(W_fullV1)
summary(W_fullV2)
W_fullV2 <- W_fullV2[, -16]

# Treatment assignment
TrV1 <- data.frame(STAR$id, STAR$smallk)
TrV2 <- na.omit(TrV1)

# common ids
ids_common <- Reduce(intersect, list(
  W_fullV2$id,
  XV2$id,
  yV2$STAR.id,
  TrV2$STAR.id
))

NJ <- length(ids_common)

# subset each dataset
W <- W_fullV2 %>%
  filter(id %in% ids_common) %>%
  arrange(id) %>%
  select(
    avg_experience,
    prop_eth_cauc, prop_eth_NA,
    prop_ladder_level1, prop_ladder_level2, prop_ladder_level3,
    prop_ladder_NA, prop_ladder_pending, prop_ladder_probation,
    prop_degree_master, `prop_degree_master+`,
    prop_degree_NA, prop_degree_specialist,
    prop_schoolk_rural, prop_schoolk_suburban, prop_schoolk_urban
  )

# Proportion of other races, mainly afroamerican, and asian, hispanic, amindian,  other,
# are the baseline
# Teachers's ladder has as baseline apprentice
# Teachers's degree has as baseline bachelor
# School's location has as baseline inner-city 

W <- cbind(1, as.matrix(W))

X <- XV2 %>%
  filter(id %in% ids_common) %>%
  arrange(id) %>%
  select(female, eth_cauc, eth_afam, age, Q1, Q2, Q3) 

# Ethnicity has as baseline other race (asian, hispanic, amindian,  other)
# Q4 is the baseline for quarters of birth
# Male is the baseline for female
X <- cbind(1, as.matrix(X))

Z <- X[,1:4]

H <- W[,c(1:3, 15:17)]

W <- W[,-1]

y  <- yV2  %>% filter(STAR.id %in% ids_common) %>% arrange(STAR.id) %>% pull(STAR.readk)
Tr <- TrV2 %>% filter(STAR.id %in% ids_common) %>% arrange(STAR.id) %>% pull(STAR.smallk)
id <- W_fullV2 %>% filter(id %in% ids_common) %>% arrange(id) %>% pull(schoolidk)

# Put everything in a data.frame including w's
Data <- data.frame(
  id = id,
  y  = y,
  Tr = Tr,
  x1 = X[,1], x2 = X[,2], x3 = X[,3], x4 = X[,4], x5 = X[,5], 
  x6 = X[,6], x7 = X[,7], x8 = X[,8],
  w1 = W[, 1], w2 = W[, 2], w3 = W[, 3], w4 = W[, 4], w5 = W[, 5],
  w6 = W[, 6], w7 = W[, 7], w8 = W[, 8], w9 = W[, 9], w10 = W[, 10],
  w11 = W[, 11], w12 = W[, 12], w13 = W[, 13], w14 = W[, 14], w15 = W[, 15],
  w16 = W[, 16],
  z1 = X[,1], z2 = X[,2], z3 = X[,3], z4 = X[,4],
  h1 = W[,1], h2 = W[,2], h3 = W[,3],
  h4 = W[,15], h5 = W[,16]
)
Kx <- ncol(X) 
Kw <- ncol(W)
Kz <- ncol(Z)
Kh <- ncol(H)

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
regOLS <- lm(y ~ Tr + x2 + x3 + x4 + x5 + x6 + x7 + x8 + w1 +
               w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10 +
               w11 + w12 + w13 + w14 + w15 + w16, data = Data)
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
