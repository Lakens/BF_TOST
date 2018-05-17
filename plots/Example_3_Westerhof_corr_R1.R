library(TOSTER)
source("Wiens_functions/BF_t.R")

n <- 218
r <- 0.12
sesoi <- 0.14  # Since the authors treat this as an interesting correlation, and treat a correlation of .12 as small enough to consider 0, we can assume that .14 is quite close to their smalles effect size of interest (which is of course wrong, as they later judge another .12 correlation to be interesting when it is significant)

result <- TOSTr(n = 218, 
                r = 0.12, 
                low_eqbound_r = -sesoi, 
                high_eqbound_r = sesoi)

LL90 <- result$LL_CI_TOST
UL90 <- result$UL_CI_TOST
LL95 <- result$LL_CI_TTEST
UL95 <- result$UL_CI_TTEST
low_eqbound_r <- result$low_eqbound_r 
high_eqbound_r <- result$high_eqbound_r 

result

## Using the beta coefficients
## r x (SD Despair)/(SD Openness)
b.prior = 0.14 * 0.8/0.6 # 0.19
b.obtained <- 0.12 * 1.0/0.6 # 0.20 

# Option B: Specify using raw effects (will still be transformed, but won't make a difference)
# Not sure what TOSTER function to use for raw coefficients
# From Zoltan's workings: 
# Correlation of Openness with Despair = 0.12
# Fisher???s z = 0.12, SE = 1/???(218-3) = .068.  z-test = 0.12/.068 = 1.76
# Raw slope of Despair regressed on Openness = r x (SD Despair)/(SD Openness) = 0.12 x 1/0.60 = 0.20 rating units per rating unit.
# As before, .20/SE = 1.76, and SE of raw slope = 0.114
BF_t(0, .19, 10000, .20, 0.114, 10000, tail = 1) # 2.98266, Wienes
BF_t(0, .000000000000000000000000000001, 10000, .20, 0.114, 10000, tail = 1) # 1, Wienes - lower RR
BF_t(0, 3.024, 10000, .20, 0.114, 10000, tail = 1) # 0.33493, Wienes - upper RR



# Computes the BayesFactor(H1 vs H0) with the H1 defined as a t distribution and the likelihood
# defined as a t distribution.
# It also plots the Prior and Posterior (and Likelihood) and adds a pie chart.
#
#  This is a modified version of the R script presented here:
#  Dienes, Z., & Mclatchie, N. (2017). Four reasons to prefer Bayesian analyses
#  over significance testing. Psychonomic Bulletin & Review, 1-12. doi:
#  10.3758/s13423-017-1266-z
#
# 170601 -- Stefan Wiens
# people.su.se/~swiens/
# Thanks to Henrik Nordstr?m, Mats Nilsson, Marco Tullio Liuzza, Anders Sand

# #Example
meantheory = 0
sdtheory = 0.19
dftheory = 10000
meanobtained = 0.20
semobtained = 0.114
dfobtained = 10000
tail = 1
#BF_t(11, 5.4, 29, 12, 5, 81)
# should give 11.12


# Create theta (ie parameter)
# ===========================
# This array ranges from -10*SDtheory to +10*SDtheory.
# Basically, one creates parameter values (theta) around the meantheory
# This is a grid.

theta <- meantheory - 10 * sdtheory
incr <- sdtheory / 200
theta=seq(from = meantheory - 10 * sdtheory, by = incr, length = 4001)
# The original calculator is not centered on meantheory (because the loop starts with theta + incr)
# ie, value at position 2001 in loop does not give the meantheory
# theta[2001]

# Create dist_theta (ie density of prior model)
# =============================================
# The prior is a t distribution characterized by meantheory, sdtheory, and dftheory
# > mean effect (meantheory)
# > standard deviation (sdtheory): This is the SEM from the t test in the original study
# The t test = mean effect / SEM, so SEM = mean effect / t
# Example: The original study reported mean effect = 12 and t(81) = 2.4
# Thus, SEM = 12 / 2.4 = 5
# > df of t test (dftheory)
# Example: If the study reported mean effect = 12 and t(81) = 2.4,
# meantheory=12, sdtheory=5, and dftheory=84
# see Dienes & Mclatchie (2017) for examples
#
# To generate the prior, for  each level of theta:
# > compute the t score (which is the standardized difference from the mean effect)
# > find the corresponding density (height on Y axis, which depends on df)
dist_theta <- dt(x = (theta-meantheory)/sdtheory, df=dftheory)

# Is the prior one-tailed or two-tailed?
# If one-tailed, only effects larger than zero are expected.
# Accordingly, the density for negative thetas is set to zero.
if(identical(tail, 1)){
  dist_theta[theta <= 0] = 0
}

# alternative computation with normalized vectors
dist_theta_alt = dist_theta/sum(dist_theta)

# Create likelihood
# For each theta, compute how well it predicts the obtained mean,
# given the obtained SEM and the obtained dfs.
# Note that the distribution is symmetric, it does not matter if one computes
# meanobtained-theta or theta-meanobtained
likelihood <- dt((meanobtained-theta)/semobtained, df = dfobtained)
# alternative computation with normalized vectors
likelihood_alt = likelihood/sum(likelihood)

# Multiply prior with likelihood
# this gives the unstandardized posterior
height <- dist_theta * likelihood
area <- sum(height * incr)
# area <- sum(dist_height * incr * likelihood)
normarea <- sum(dist_theta * incr)
# alternative computation with normalized vectors
height_alt = dist_theta_alt * likelihood_alt
height_alt = height_alt/sum(height_alt)

LikelihoodTheory <- area/normarea
LikelihoodNull <- dt(meanobtained/semobtained, df = dfobtained)
BayesFactor <- round(LikelihoodTheory / LikelihoodNull, 6)

#plot (adapted from Wiens by DL)
myminY = 1
# rescale prior and posterior to sum = 1 (density)
dist_theta_alt = dist_theta_alt / (sum(dist_theta_alt)*incr)
height_alt = height_alt/(sum(height_alt)*incr)
# rescale likelood to maximum = 1
likelihood_alt = likelihood_alt / max(likelihood_alt)
data = cbind(dist_theta_alt, height_alt)
maxy = max(data)
max_per_x = apply(data,1,max)
max_x_keep = max_per_x/maxy*100 > myminY  # threshold (1%) here
x_keep = which(max_x_keep==1)
tiff(file=paste("Fig5.R1.tiff",sep=""),width=2300,height=2000, units = "px", res = 300)
plot(NA, ylim=c(0,maxy), xlim=c(min(LL90,low_eqbound_r)-max(UL90-LL90, high_eqbound_r-low_eqbound_r)/2, max(UL90,high_eqbound_r)+max(UL90-LL90, high_eqbound_r-low_eqbound_r)/2), bty="l", yaxt="n", ylab="",xlab="beta")
#points(x=r, y=maxy/2, pch=15, cex=2)
#abline(v=high_eqbound_r, lty=2)
#abline(v=low_eqbound_r, lty=2)
#abline(v=0, lty=2, col="grey")
#segments(LL90,maxy/2,UL90,maxy/2, lwd=5)
#segments(LL95,maxy/2,UL95,maxy/2, lwd=3)
par(new=TRUE)
plot(theta, dist_theta_alt, type = "l",
     ylim = c(0, maxy),
     xlim=c(min(LL90,low_eqbound_r)-max(UL90-LL90, high_eqbound_r-low_eqbound_r)/2, max(UL90,high_eqbound_r)+max(UL90-LL90, high_eqbound_r-low_eqbound_r)/2),
     ylab = "Density (for Prior and Posterior)", xlab = "", col = "grey46", lwd = 3, lty = 2)
lines(theta, height_alt, type = "l", col = "black", lwd = 5, lty = 1)
theta0 = which(theta == min(theta[theta>0]))
points(theta[theta0],dist_theta_alt[theta0], pch = 19, col = "grey46", cex = 1.5)
points(theta[theta0],height_alt[theta0], pch = 19, col = "black", cex = 1.5)
par(new = T)
plot(theta, likelihood_alt, type = "l",
     ylim = c(0, 1),
     xlim=c(min(LL90,low_eqbound_r)-max(UL90-LL90, high_eqbound_r-low_eqbound_r)/2, max(UL90,high_eqbound_r)+max(UL90-LL90, high_eqbound_r-low_eqbound_r)/2),     col = "grey74", lwd = 3, lty = 3, axes = F, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 3, 'Likelihood')
abline(v = theta[theta0], lwd = 2, lty = 3)
dev.off()
