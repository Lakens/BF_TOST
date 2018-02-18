# Example 3. Westerhof, Bohlmeijer & McAdams (2015) predicts that Big Five NEO-FFI openness should be related to Ego integrity. 
# The authors conclude "higher levels of openness to experience are significantly related to more ego integrity but not to despair." after finding a significant .14 correlation between openness and ego integrity, and a non-significant .12 correlation between openness and despair.

source("TOSTr.bf.R")

n <- 218
r <- 0.12
sesoi <- 0.14  # Since the authors treat this as an interesting correlation, and treat a correlation of .12 as small enough to consider 0, we can assume that .14 is quite close to their smalles effect size of interest (which is of course wrong, as they later judge another .12 correlation to be interesting when it is significant)

# BF = no other knowledge, other than it is a correlation test.
TOSTr.bf(n = n, 
         r = r, 
         low_eqbound_r = -sesoi, 
         high_eqbound_r = sesoi, 
         prior_dist = "normal", 
         effect_prior = 0, 
         se_prior = 0.5, 
         df_prior = 10000)

# BF = no other knowledge than the average effect size in psychology
# Based on median effect size in Psychology (Aczel, Palfi & Szaszi, 2017)
# d converted to r: https://www.psychometrica.de/effect_size.html
TOSTr.bf(n = n, 
         r = r, 
         low_eqbound_r = -sesoi, 
         high_eqbound_r = sesoi, 
         prior_dist = "normal", 
         effect_prior = 0, 
         se_prior = 0.4216/2,  
         df_prior = 10000)


# Converting standardised to raw units
# r x (SD Despair)/(SD Openness)
b.obtained <- 0.12 * 2.9/3.0 # 0.116 

# Raw SE
# 1/sqrt(N-3)
se.obtained <- 1/sqrt(218-3)  # 0.06819943

# df
df <- 218-2

# H1 prediction
h1 <- (5-1)/(6-1)  # 0.80


source("TOSTtwo.raw.bf.R")
BF_t(0, h1, 100000, b.obtained, se.obtained, df, tail = 1) # 0.679599
1/sqrt(218-3)

## Question - is it appropriate to calculate a raw coefficient using the tostr.bf function?

TOSTr.bf(n = 218, 
         r = b.obtained, 
         low_eqbound_r = -sesoi, 
         high_eqbound_r = sesoi, 
         prior_dist = "halfnormal", 
         effect_prior = 0, 
         se_prior = h1,  
         df_prior = 10000)
