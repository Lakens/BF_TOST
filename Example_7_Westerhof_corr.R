# Example 7. Westerhof, Bohlmeijer & McAdams (2015) predicts that Big Five NEO-FFI openness should be related to Ego integrity. 
# The authors conclude "higher levels of openness to experience are significantly related to more ego integrity but not to despair." after finding a significant .14 correlation between openness and ego integrity, and a non-significant .12 correlation between openness and despair.

source("TOSTr.bf.R")

n <- 218
r <- 0.12
sesoi <- 0.14  # Since the authors treat this as an interesting correlation, and treat a correlation of .12 as small enough to consider 0, we can assume that .14 is quite close to their smalles effect size of interest (which is of course wrong, as they later judge another .12 correlation to be interesting when it is significant)

TOSTr.bf(n = n, 
         r = r, 
         low_eqbound_r = -sesoi, 
         high_eqbound_r = sesoi, 
         prior_dist = "normal", 
         effect_prior = 0, 
         se_prior = 2, 
         df_prior = 10000)

