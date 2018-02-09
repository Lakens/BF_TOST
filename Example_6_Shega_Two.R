#Shega, Tiedt, Grant & Dale (2014) examined the relationship between pain and age in  the National Social Life, Health, and Aging Project (NSHAP). In Wave 2 the presence, location, and intensity ofpain was assessed. Pain items demonstrated remarkable similarity among age cohorts.

source('TOSTtwo.bf.R') #Load function for standardized mean differences

#Set SESOI to d = 0.1. Small effects can be excluded given large sample size. 

m1 <- 72.06
m2 <- 72.061
n1 <- 1300
n2 <- 1280
sd1 <- 0.308*sqrt(n1)
sd2 <- 0.387*sqrt(n2)

TOSTtwo.bf(m1 = 72.06,
           m2 = 72.060001,
           sd1 = 11.1051,
           sd2 = 13.84573,
           n1 = 1300,
           n2 = 1289,
           low_eqbound_d = -0.1,
           high_eqbound_d = 0.1, 
           var.equal = TRUE, 
           prior_dist = "normal", 
           effect_prior = 0,
           se_prior = 3,
           df_prior = 100000)

