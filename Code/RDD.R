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
Y = data$Y
X = data$X
T = data$T
T_X = T*X


# R Snippet 12
# Using rdrobust with uniform weights
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_uniform_adhoc_p1_rho1_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'uniform',  p = 1, h = 20)
summary(out)
txtStop()

# R Snippet 17
# Using rdrobust with mserd bandwidth
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# R Snippet 22
# Using rdrobust with default options and showing all the output
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1_all.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd', all = TRUE)
summary(out)
txtStop()

# R Snippet 25
# Using rdbwselect with covariates
txtStart("./outputs/Vol-1-R_meyersson_rdbwselect_triangular_mserd_p1_regterm1_covariates_noi89.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out = rdbwselect(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# R Snippet 26
# Using rdrobust with covariates
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_regterm1_covariates_noi89.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out = rdrobust(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# R Snippet 27
# Using rdrobust with clusters
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_regterm1_clusters.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd', cluster = data$prov_num)
summary(out)
txtStop()

# R Snippet 28
# Using rdrobust with clusters and covariates
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_regterm1_covariates_noi89_clusters.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out = rdrobust(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd', cluster = data$prov_num)
summary(out)
txtStop()

#-----------------------------------------------#
# Section 5                                     #
# Validation and Falsification of the RD Design #
#-----------------------------------------------#
# Figure 16
# RD plots for predetermined covariates
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_lpop1994.pdf")
rdplot(data$lpop1994, X,
       x.label = "Score", y.label = "", title = "")
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_partycount.pdf")
rdplot(data$partycount, X,
       x.label = "Score", y.label = "", title = "")
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_vshr_islam1994.pdf")
rdplot(data$vshr_islam1994, X,
       x.label = "Score", y.label = "", title = "")
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_i89.pdf")
rdplot(data$i89, X,
       x.label = "Score", y.label = "", title = "", x.lim = c(-100,100))
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_merkezp.pdf")
rdplot(data$merkezp, X,
       x.label = "Score", y.label = "", title = "")
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_merkezi.pdf")
rdplot(data$merkezi, X,
       x.label = "Score", y.label = "", title = "")
dev.off()

# R Snippet 29
# Using rdrobust on lpop1994
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdrobust_lpop1994.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(data$lpop1994, X)
summary(out)
txtStop()

# Formal continuity-based analysis for covariates using CER-optimal bandwidth (not reported in the text)
summary(rdrobust(data$hischshr1520m, X, bwselect = 'cerrd'))
summary(rdrobust(data$i89, X, bwselect = 'cerrd'))
summary(rdrobust(data$vshr_islam1994, X, bwselect = 'cerrd'))
summary(rdrobust(data$partycount, X, bwselect = 'cerrd'))
summary(rdrobust(data$lpop1994, X, bwselect = 'cerrd'))
summary(rdrobust(data$merkezi, X, bwselect = 'cerrd'))
summary(rdrobust(data$merkezp, X, bwselect = 'cerrd'))
summary(rdrobust(data$subbuyuk, X, bwselect = 'cerrd'))
summary(rdrobust(data$buyuk, X, bwselect = 'cerrd'))

# R Snippet 30
# Using rdplot to show the rdrobust effect for lpop1994
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_lpop1994.txt",
         commands = TRUE, results = FALSE, append = FALSE, visible.only = TRUE)
bandwidth = rdrobust(data$lpop1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
rdplot(data$lpop1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")
txtStop()

# Figure 17
# Graphical illustration of local linear RD effects for predetermined covariates
bandwidth = rdrobust(data$lpop1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_lpop1994.pdf")
rdplot(data$lpop1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")
dev.off()

bandwidth = rdrobust(data$partycount, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_partycount.pdf")
rdplot(data$partycount[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")
dev.off()

bandwidth = rdrobust(data$vshr_islam1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_vshr_islam1994.pdf")
rdplot(data$vshr_islam1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")
dev.off()

bandwidth = rdrobust(data$i89, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_i89.pdf")
rdplot(data$i89[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")
dev.off()

# R Snippet 31
# Binomial test
txtStart("./outputs/Vol-1-R_meyersson_falsification_binomial_byhand_adhoc.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
binom.test(53, 100, 1/2)
txtStop()

# R Snippet 32
# Using rddensity
txtStart("./outputs/Vol-1-R_meyersson_falsification_rddensity.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rddensity(X)
summary(out)
txtStop()

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
ggsave("./outputs/Vol-1-R_meyersson_falsification_lpdensity2.pdf", plot = plot2, width = 6, height = 5, units = "in")

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
ggsave("./outputs/Vol-1-R_meyersson_falsification_lpdensity1.pdf", plot = plot1, width = 6, height = 5, units = "in")

# R Snippet 33
# Using rdrobust with the cutoff equal to 1
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdrobust_alternative-cutoff_c1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y[X >= 0], X[X >= 0], c = 1)
summary(out)
txtStop()

# R Snippet 34
# Using rdrobust for the donut-hole approach
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdrobust_donuthole.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y[abs(X) >= 0.3], X[abs(X) >= 0.3])
summary(out)
txtStop()

