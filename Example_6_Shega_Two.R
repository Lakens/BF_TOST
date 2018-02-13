#Shega, Tiedt, Grant & Dale (2014) examined the relationship between pain and age in  the National Social Life, Health, and Aging Project (NSHAP). In Wave 2 the presence, location, and intensity ofpain was assessed. Pain items demonstrated remarkable similarity among age cohorts.

source('TOSTtwo.bf.R') #Load function for standardized mean differences

# Determine the SESOI:

sesoi.vas <- 9  # On a 100mm visual analogue scale, 12mm 95%CI[9mm, 15mm] seems to be the clinical Minimally Important Difference for chronic pain ratings (see Kelly [2001]). We can pick a conservative estimate by choosing the lower end of this CI as our SESOI.
sesoi.lic <- mid.vas*((7-1)/(100-1))  # Need to convert the MID to Lickert scale, 

# Need to convert raw SESOI to value in COhens d. First estimate total SD, the calculate Cohens d from SESOI/SD
sd62 <- 0.084*sqrt(1020)
sd70 <- 0.057*sqrt(1015)
sd80 <- 0.102*sqrt(554)
sd <- (sd62*1020 + sd70*1015 + sd80*554) / (1020+1015+554)
sesoi.d <- mid.lic/sd  


# Descriptive statistics for Age group 62-69 (1) and 70-79 (2)

m1 <- 2.03
m2 <- 1.98
n1 <- 1020
n2 <- 1015
sd1 <- 0.084*sqrt(n1)
sd2 <- 0.057*sqrt(n2)

# Calculate TOST and BF

TOSTtwo.bf(m1 = m1,
           m2 = m2,
           sd1 = sd1,
           sd2 = sd2,
           n1 = n1,
           n2 = n2,
           low_eqbound_d = -sesoi.d,
           high_eqbound_d = sesoi.d, 
           var.equal = TRUE, 
           prior_dist = "normal", 
           effect_prior = 0,
           se_prior = 3,
           df_prior = 100000)



# Descriptive statistics for Age group 70-79 (1) and >80 (2)

m1 <- 1.98
m2 <- 2.14
n1 <- 1015
n2 <- 554
sd1 <- 0.057*sqrt(n1)
sd2 <- 0.102*sqrt(n2)

# Calculate TOST and BF

TOSTtwo.bf(m1 = m1,
           m2 = m2,
           sd1 = sd1,
           sd2 = sd2,
           n1 = n1,
           n2 = n2,
           low_eqbound_d = -sesoi.d,
           high_eqbound_d = sesoi.d, 
           var.equal = TRUE, 
           prior_dist = "normal", 
           effect_prior = 0,
           se_prior = 3,
           df_prior = 100000)



# Descriptive statistics for Age group 62-69 (1) and >80 (2)

m1 <- 2.03
m2 <- 2.14
n1 <- 1020
n2 <- 554
sd1 <- 0.084*sqrt(n1)
sd2 <- 0.102*sqrt(n2)

# Calculate TOST and BF

TOSTtwo.bf(m1 = m1,
           m2 = m2,
           sd1 = sd1,
           sd2 = sd2,
           n1 = n1,
           n2 = n2,
           low_eqbound_d = -sesoi.d,
           high_eqbound_d = sesoi.d, 
           var.equal = TRUE, 
           prior_dist = "normal", 
           effect_prior = 0,
           se_prior = 3,
           df_prior = 100000)