source("TOSTtwo.bf.prettyplot.R")

#Example 1
n.younger.scheibe <- 38
n.older.scheibe <- 39
t.crit <- qt(1-0.05/2, (n.younger.scheibe + n.older.scheibe)-2)
d.crit <- t.crit * sqrt((1/n.younger.scheibe)+(1/n.older.scheibe))

n.younger <- 32
n.older <- 32
m.younger <- 0.338 # NB! The means and standard deviations used in this example are NOT perfectly equal to those reported in Martins, Sheppes, Gross, and Mather (2016). Probably due to rounding of reported values, the numbers provided in the paper does not lead us to arrive at the same t statistic as the one reported in the paper. We therefore changed the mean and standard deviation values to be as close as possible to numbers that would replicate the t statistic reported. 
sd.younger <- 0.0344 * sqrt(n.younger) 
m.older <- 0.321 
sd.older <- 0.0344 * sqrt(n.older) 

TOSTtwo.bf.prettyplot(m1 = m.younger, 
                      m2 = m.older, 
                      sd1 = sd.younger, 
                      sd2 = sd.older, 
                      n1 = n.younger, 
                      n2 = n.older, 
                      low_eqbound_d = -d.crit, 
                      high_eqbound_d = d.crit, 
                      alpha = 0.05, 
                      var.equal = FALSE, 
                      prior_dist = "halfnormal", 
                      effect_prior = 0, 
                      se_prior = 0.189, 
                      df_prior = 10000)


#Example 2

source('TOSTtwo.raw.bf.prettyplot.R') #Load function for standardized mean differences

sesoi.vas <- 9  # On a 100mm visual analogue scale, 12mm 95%CI[9mm, 15mm] seems to be the clinical Minimally Important Difference for chronic pain ratings (see Kelly [2001]). We can pick a conservative estimate by choosing the lower end of this CI as our SESOI.
sesoi.lik <- sesoi.vas*((7-1)/(100-1))  # Need to convert the MID to Lickert scale, 

m1 <- 2.03
m2 <- 1.98
n1 <- 1020
n2 <- 1015
sd1 <- 0.084*sqrt(n1)
sd2 <- 0.057*sqrt(n2)

TOSTtwo.raw.bf.prettyplot(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi.lik,
               high_eqbound = sesoi.lik, 
               var.equal = FALSE, 
               prior_dist = "halfnormal", 
               effect_prior = 0,
               se_prior = 1.1,
               df_prior = 100000)


source("TOSTr.bf.prettyplot.R")

n <- 218
r <- 0.12
sesoi <- 0.14  # Since the authors treat this as an interesting correlation, and treat a correlation of .12 as small enough to consider 0, we can assume that .14 is quite close to their smalles effect size of interest (which is of course wrong, as they later judge another .12 correlation to be interesting when it is significant)

# BF = based on authors interpreting 0.14 as meaningful
# raw slope = 0.14 * 4.2/3 = 0.20
TOSTr.bf.prettyplot(n = n, 
         r = r, 
         low_eqbound_r = -sesoi, 
         high_eqbound_r = sesoi, 
         prior_dist = "halfnormal", 
         effect_prior = 0, 
         se_prior = 0.20,  
         df_prior = 10000)

#Example 4

source('TOSTtwo.raw.bf.prettyplot.R') #Load function for standardized mean differences

## Short delay condition
sd1 <- sqrt(0.17^2+0.14^2-(2*0.85*0.14*0.17))
sd2 <- sqrt(0.12^2+0.12^2-(2*0.85*0.12*0.12))
m1 <- 0.77-0.76 #From Table 3
m2 <- 0.76-0.750001 ##From Table 3. Added a tiny difference due to bug in code the can not deal with a difference of 0.
n1 <- 32 #From Table 1
n2 <- 32 #From Table 1

sesoi <- 0.5*((sd1+sd2)/2) #Based on the statement earlier in the article that another predicted effect "failed to reach signifcance [.] even though the power to detect a medium-sized interaction was high" we set the SESOI to have a standard deviation. Because the population standard deviation is unknown we estimate it be averaging the sd's from the two groups.
prior_bf <- (0.61-0.54) - (0.64-0.61) #Table 2, young-old

TOSTtwo.raw.bf.prettyplot(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi,
               high_eqbound = sesoi, 
               var.equal = TRUE, 
               prior_dist = "normal", 
               effect_prior = prior_bf,
               df_prior = 62)
