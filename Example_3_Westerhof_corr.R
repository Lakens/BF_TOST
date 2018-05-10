# Example 3. Westerhof, Bohlmeijer & McAdams (2015) predicts that Big Five NEO-FFI openness should be related to Ego integrity. 
# The authors conclude "higher levels of openness to experience are significantly related to more ego integrity but not to despair." after finding a significant .14 correlation between openness and ego integrity, and a non-significant .12 correlation between openness and despair.
source("TOSTr.bf.R")
n <- 218
r <- 0.12
sesoi <- 0.14  # Since the authors treat this as an interesting correlation, and treat a correlation of .12 as small enough to consider 0, we can assume that .14 is quite close to their smalles effect size of interest (which is of course wrong, as they later judge another .12 correlation to be interesting when it is significant)

# BF = based on authors interpreting 0.14 as meaningful
# raw slope = 0.14 * 0.8/0.6 = 0.19
TOSTr.bf(n = n, 
         r = r, 
         low_eqbound_r = -sesoi, 
         high_eqbound_r = sesoi, 
         prior_dist = "halfnormal", 
         effect_prior = 0, 
         se_prior = 0.19,  
         df_prior = 10000)
# B = 2.56111

# BF = Lower region
TOSTr.bf(n = n, 
         r = r, 
         low_eqbound_r = -sesoi, 
         high_eqbound_r = sesoi, 
         prior_dist = "halfnormal", 
         effect_prior = 0, 
         se_prior = 0.125,  
         df_prior = 10000)
# B = 2.996933

# BF = Upper region
TOSTr.bf(n = n, 
         r = r, 
         low_eqbound_r = -sesoi, 
         high_eqbound_r = sesoi, 
         prior_dist = "halfnormal", 
         effect_prior = 0, 
         se_prior = 1.87,  
         df_prior = 10000)# B = 2.487562
# 0.336237




# Converting standardised to raw units
# r x (SD Despair)/(SD Openness)
b.obtained <- 0.12 * 1.0/0.6 # 0.20 

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