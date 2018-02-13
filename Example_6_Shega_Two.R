#Shega, Tiedt, Grant & Dale (2014) examined the relationship between pain and age in  the National Social Life, Health, and Aging Project (NSHAP). In Wave 2 the presence, location, and intensity ofpain was assessed. Pain items demonstrated remarkable similarity among age cohorts.

source('TOSTtwo.bf.R') #Load function for standardized mean differences


# Calculate the average pain ratings for two age cohorts (older and younger than 70)

mean.old <- ((1.98*1015)+(2.14*554))/1569
se.old <- ((0.057*1015)+(0.102*554))/1569

sd.70 <- 0.057*sqrt(1015)
sd.80 <- 0.102*sqrt(554)
sd.old <- ((sd.70*1015)+(sd.80*554))/1569

m1 <- 2.03
m2 <- mean.old
n1 <- 1020
n2 <- 1569
sd1 <- 0.084*sqrt(n1)
sd2 <- sd.old


# Determine the SESOI:

mid.vas <- 9  # On a 100mm visual analogue scale, 12mm 95%CI[9mm, 15mm] seems to be the clinical Minimally Important Difference for chronic pain ratings (see Kelly [2001]). We can pick a conservative estimate by choosing the lower end of this CI as our SESOI.
mid.lic <- mid.vas*((7-1)/(100-1))  # Need to convert the MID to Lickert scale, 
mid.d <- mid.lic/sd2  # Need to convert raw SESOI to value in COhens d


# Calculate TOST and BF

TOSTtwo.bf(m1 = m1,
           m2 = m2,
           sd1 = sd1,
           sd2 = sd2,
           n1 = n1,
           n2 = n2,
           low_eqbound_d = -mid.d,
           high_eqbound_d = mid.d, 
           var.equal = TRUE, 
           prior_dist = "normal", 
           effect_prior = 0,
           se_prior = 3,
           df_prior = 100000)

