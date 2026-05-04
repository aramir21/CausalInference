rm(list=ls())

library(foreign)
library(ggplot2)
library(lpdensity)
library(rddensity)
library(rdrobust)
library(rdlocrand)
library(TeachingDemos)

options(width=280)
par(mar = rep(2, 4))

# A folder called "outputs" needs to be created in order to store 
# all of the figures, logs and tables. If the folder already exists,
# the user will get an error message but the code will not stop.
# tryCatch(dir.create("outputs"))

# Tables 1, 2, 4, 5, and 6 are only constructed in STATA
# Figures 20, 21, and 22 are only constructed in STATA

# Loading the data and defining the main variables
data = read.dta("CIT_2019_Cambridge_polecon.dta")
Y = data$Y # Percentage of young women who complete high school by 2000
X = data$X # Percentage Islamic margin victory for Mayoral elections in 1994
T = data$T # Treated = 1 (Islamic mayor)
T_X = T*X


# R Snippet 12
# Using rdrobust with uniform weights
out = rdrobust(Y, X, kernel = 'uniform',  p = 1, h = 20)
summary(out)

# R Snippet 17
# Using rdrobust with mserd bandwidth
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)

# R Snippet 22
# Using rdrobust with mserd bandwidth
# all = TRUE means rdbwselect reports all available bandwidth selection procedures 
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd', all = TRUE)
summary(out)

# R Snippet 26
# Using rdrobust with covariates
Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out = rdrobust(Y, X, covs = Z, kernel = 'triangular', p = 1, bwselect = 'mserd')
summary(out)

#-----------------------------------------------#
# Section 5                                     #
# Validation and Falsification of the RD Design #
#-----------------------------------------------#
# Figure 16
# RD plots for predetermined covariates
rdplot(data$lpop1994, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$partycount, X,
       x.label = "Score", y.label = "", title = "")


# R Snippet 29
# Using rdrobust on lpop1994
out = rdrobust(data$lpop1994, X)
summary(out)

# Formal continuity-based analysis for covariates using CER-optimal bandwidth (not reported in the text)
summary(rdrobust(data$lpop1994, X, bwselect = 'mserd'))

# R Snippet 30
# Using rdplot to show the rdrobust effect for lpop1994
bandwidth = rdrobust(data$lpop1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
rdplot(data$lpop1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

# Figure 17
# Graphical illustration of local linear RD effects for predetermined covariates
bandwidth = rdrobust(data$lpop1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
rdplot(data$lpop1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

# R Snippet 31
# Binomial test
binom.test(53, 100, 1/2)

# R Snippet 32
# Using rddensity
out = rddensity(X)
summary(out)

# Figure 19a
# Histogram
library(ggplot2)
library(rddensity)

# Bandwidths
bw <- rddensity(X)$h
bw_left  <- as.numeric(bw[1])
bw_right <- as.numeric(bw[2])

# Data
tempdata <- data.frame(v1 = X)

# Plot
plot2 <- ggplot(tempdata, aes(x = v1)) +
  theme_bw(base_size = 17) +
  
  # Left of cutoff
  geom_histogram(
    aes(y = after_stat(count)),
    breaks = seq(-bw_left, 0, by = 1),
    fill = "blue", color = "black"
  ) +
  
  # Right of cutoff
  geom_histogram(
    aes(y = after_stat(count)),
    breaks = seq(0, bw_right, by = 1),
    fill = "red", color = "black"
  ) +
  
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  
  labs(
    x = "Score",
    y = "Number of Observations",
    title = "Histogram around the cutoff"
  )

plot2

# Figure 19b
# Estimated Density
est1 = lpdensity(data = X[X < 0 & X >= -bw_left], grid = seq(-bw_left, 0, 0.1), bwselect = "IMSE",
                 scale = sum(X < 0 & X >= -bw_left) / length(X))
est2 = lpdensity(data = X[X >= 0 & X <= bw_right], grid = seq(0, bw_right, 0.1), bwselect = "IMSE",
                 scale = sum(X >= 0 & X <= bw_right) / length(X))
plot1 <- lpdensity.plot(
  est1, est2,
  CIshade = 0.2,
  lcol = c("blue", "red"),
  CIcol = c("blue", "red"),
  legendGroups = c("Control", "Treatment")
) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  labs(
    x = "Running variable (Score)",
    y = "Density",
    title = "Density around the cutoff"
  ) +
  theme_bw(base_size = 17) +
  theme(
    legend.position = c(0.8, 0.85),
    legend.title = element_blank()
  )

plot1

# R Snippet 33
# Using rdrobust with the cutoff equal to 1
out = rdrobust(Y[X >= 0], X[X >= 0], c = 1)
summary(out)

# R Snippet 34
# Using rdrobust for the donut-hole approach
out = rdrobust(Y[abs(X) >= 0.3], X[abs(X) >= 0.3])
summary(out)

