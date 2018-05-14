# Example 3. Westerhof, Bohlmeijer & McAdams (2015) predicts that Big Five NEO-FFI openness should be related to Ego integrity. 
# The authors conclude "higher levels of openness to experience are significantly related to more ego integrity but not to despair." after finding a significant .14 correlation between openness and ego integrity, and a non-significant .12 correlation between openness and despair.
source('TOSTtwo.raw.bf.R') #Load function for raw mean differences
source("TOSTr.bf.R")
source("Wiens_functions/DienesMcLatchie.R")

n <- 218
r <- 0.12
sesoi <- 0.14  # Since the authors treat this as an interesting correlation, and treat a correlation of .12 as small enough to consider 0, we can assume that .14 is quite close to their smalles effect size of interest (which is of course wrong, as they later judge another .12 correlation to be interesting when it is significant)






## Using the beta coefficients
## r x (SD Despair)/(SD Openness)
b.prior = 0.14 * 0.8/0.6 # 0.19
b.obtained <- 0.12 * 1.0/0.6 # 0.20 



# Option A: Specify using standardised effect r (and the calculator while transform both using fisher's z)
TOSTr.bf(n = 218, 
         r = 0.12, 
         low_eqbound_r = -sesoi, 
         high_eqbound_r = sesoi, 
         prior_dist = "halfnormal", 
         effect_prior = 0, 
         se_prior = 0.14,  
         df_prior = 10000)
# B = 2.915786


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










# Converting standardised to raw units


# Raw SE
# 1/sqrt(N-3)
se.obtained <- 1/sqrt(218-3)  # 0.06819943

# df
df <- 218-2

# H1 prediction
h1 <- (5-1)/(6-1)  # 0.80

# BF = based on correlation heuristic
# raw slope = 0.14 * 4.2/3 = 0.20
TOSTr.bf(n = n, 
         r = r, 
         low_eqbound_r = -sesoi, 
         high_eqbound_r = sesoi, 
         prior_dist = "halfnormal", 
         effect_prior = 0, 
         se_prior = 0.40,  
         df_prior = 10000)
# B = 1.468118

