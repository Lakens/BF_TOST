#Shega, Tiedt, Grant & Dale (2014) examined the relationship between pain and age in  the National Social Life, Health, and Aging Project (NSHAP). In Wave 2 the presence, location, and intensity ofpain was assessed. Pain items demonstrated remarkable similarity among age cohorts.

source('TOSTtwo.raw.bf.R') #Load function for standardized mean differences

# Determine the SESOI:

sesoi.vas <- 9  # On a 100mm visual analogue scale, 12mm 95%CI[9mm, 15mm] seems to be the clinical Minimally Important Difference for chronic pain ratings (see Kelly [2001]). We can pick a conservative estimate by choosing the lower end of this CI as our SESOI.
sesoi.lik <- sesoi.vas*((7-1)/(100-1))  # Need to convert the MID to Lickert scale, 

# Descriptive statistics for Example 2a Age group 62-69 (1) and 70-79 (2)
m1 <- 2.03
m2 <- 1.98
n1 <- 1020
n2 <- 1015
sd1 <- 0.084*sqrt(n1)
sd2 <- 0.057*sqrt(n2)

# Calculate TOST and BF
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi.lik,
               high_eqbound = sesoi.lik, 
               var.equal = FALSE, 
               alpha = 0.05/3,
               prior_dist = "normal", 
               effect_prior = 0,
               se_prior = 1.21,
               df_prior = 100000)
# B=0.094322

# Rob Reg lower
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi.lik,
               high_eqbound = sesoi.lik, 
               var.equal = FALSE, 
               alpha = 0.05/3,
               prior_dist = "normal", 
               effect_prior = 0,
               se_prior = 0.323,
               df_prior = 100000)
# B = 0.334774

# Rob Reg upper
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi.lik,
               high_eqbound = sesoi.lik, 
               alpha = 0.05/3,
               var.equal = FALSE, 
               prior_dist = "normal", 
               effect_prior = 0,
               se_prior = 3000,
               df_prior = 100000)
# B = Infinity

# Descriptive statistics for Example 2b: Age group 70-79 (1) and >80 (2)
m1 <- 2.14
m2 <- 1.98
n1 <- 554
n2 <-  1015
sd1 <- 0.102*sqrt(n2)
sd2 <- 0.057*sqrt(n1) 

# Calculate TOST and BF
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi.lik,
               high_eqbound = sesoi.lik, 
               alpha = 0.05/3,
               var.equal = FALSE, 
               prior_dist = "normal", 
               effect_prior =0,
               se_prior = 1.21,
               df_prior = 100000)
# B= 0.21723

# RR lower
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi.lik,
               high_eqbound = sesoi.lik, 
               alpha = 0.05/3,
               var.equal = FALSE, 
               prior_dist = "normal", 
               effect_prior = 0,
               se_prior = 0.767,
               df_prior = 100000)
# B = 0.334986

# RR upper
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi.lik,
               high_eqbound = sesoi.lik, 
               alpha = 0.05/3,
               var.equal = FALSE, 
               prior_dist = "normal", 
               effect_prior = 0,
               se_prior = 160,
               df_prior = 100000)
# B = 0.001995

# Descriptive statistics for Example 2c: Age group 62-69 (1) and >80 (2)
m1 <- 2.14
m2 <- 2.03 
n1 <- 554
n2 <-  1020
sd1 <- 0.102*sqrt(n2)
sd2 <- 0.084*sqrt(n1) 

# Calculate TOST and BF
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi.lik,
               high_eqbound = sesoi.lik, 
               alpha = 0.05/3,
               var.equal = FALSE, 
               prior_dist = "normal", 
               effect_prior = 0,
               se_prior = 1.21,
               df_prior = 100000)
# B = 0.161194

# RR lower
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi.lik,
               high_eqbound = sesoi.lik, 
               alpha = 0.05/3,
               var.equal = FALSE, 
               prior_dist = "normal", 
               effect_prior = 0,
               se_prior = .559,
               df_prior = 100000)
# B = 0.334657

# RR upper
TOSTtwo.raw.bf(m1 = m1,
               m2 = m2,
               sd1 = sd1,
               sd2 = sd2,
               n1 = n1,
               n2 = n2,
               low_eqbound = -sesoi.lik,
               high_eqbound = sesoi.lik, 
               alpha = 0.05/3,
               var.equal = FALSE, 
               prior_dist = "normal", 
               effect_prior = 10000,
               df_prior = 100000)
# B = 0.00027