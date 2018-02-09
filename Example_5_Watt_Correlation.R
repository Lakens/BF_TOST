#Watt, Konnert, & Speirs (2017) discuss how research has revealed body satisfaction is quite stable across age, and based on the life-span theory of control predict that body satisfaction will not be related to age. Body satisfaction was assessed with a seven item Appearance Evaluation Scale (AES) in a sample of 362 woman. 

#There are a number of reasons to set the SESOI to 0.1. 
#It's a small effect following Cohen
#It's the boundary of what could be significant (based on 50% power)"
library("pwr")
pwr.r.test(n = 362, sig.level = 0.05, power = 0.5, 
           alternative = "two.sided")

source('TOSTr.bf.R') #Load function for correlations

n <- 362
r <- -0.03
TOSTr.bf(n = n, r = r, low_eqbound_r = -0.1, high_eqbound_r = 0.1, alpha = 0.05, prior_dist = "normal", effect_prior = 0.0, se_prior = 0.2, df_prior=10000)
