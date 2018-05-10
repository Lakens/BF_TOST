#Calculate the SD's for the difference scores or LR and HR for yound and old participants. We are calculating the sd of a difference score between two dependent variables, so need to take the correlation into account. 



## Short delay condition
sd1 <- sqrt(0.17^2+0.14^2-(2*0.85*0.14*0.17))
sd2 <- sqrt(0.12^2+0.12^2-(2*0.85*0.12*0.12))
m1 <- 0.77-0.76 #From Table 3
m2 <- 0.76-0.750001 ##From Table 3. Added a tiny difference due to bug in code the can not deal with a difference of 0.
n1 <- 32 #From Table 1
n2 <- 32 #From Table 1

sesoi <- 0.5*((sd1+sd2)/2) #Based on the statement earlier in the article that another predicted effect "failed to reach signifcance [.] even though the power to detect a medium-sized interaction was high" we set the SESOI to have a standard deviation. Because the population standard deviation is unknown we estimate it be averaging the sd's from the two groups.

#To quantify the prior, we assume the authors are interested in showing the effect size in the short delay condition is smaller than the effect size expected in the long delay condition. The effect in the long delay condition is quite large (around 0.8 standard deviations). Thus, we calculate a Bayes factor for an alternative hypothesis that reflects the data as expected for the long delay condition, which is a normal distribution centered on 0.8*((sd1+sd2)/2), or 0.0621571. Following recommendations by Dienes (2014) we set the standard deviation to twice the expected effect size (the default option in the TOSTER package). 
m1-m2
## experiment 1 results
prior_bf <- (0.61-0.54) - (0.64-0.61) #Table 2, young-old

source('TOSTtwo.raw.bf.R') #Load function for standardized mean differences

# Calculate TOST and BF
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi,
               high_eqbound = sesoi, 
               var.equal = TRUE, 
               prior_dist = "halfnormal", 
               effect_prior = prior_bf,
               df_prior = 62)
# B = 0.43819


# BF lower
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi,
               high_eqbound = sesoi, 
               var.equal = TRUE, 
               prior_dist = "halfnormal", 
               effect_prior =.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001,
               df_prior = 62)
# B = 0.323365


# BF upper
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi,
               high_eqbound = sesoi, 
               var.equal = TRUE, 
               prior_dist = "halfnormal", 
               effect_prior = 0.0540,
               df_prior = 62)
# B = 0.323365

