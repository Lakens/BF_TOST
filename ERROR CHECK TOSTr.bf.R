## TOST Error Checks: tostr.bf
## Testing TOST results against other BF scripts.

## The calculators in this error check are:
# TOSTr.bf
# Baguley & Kaye:      http://danny-kaye.co.uk/Docs/Dienes_functions.txt
# Wienes:              https://figshare.com/authors/Stefan_Wiens/3862558

#Load functions

source('TOSTtwo.raw.bf.R') #Load function for raw mean differences
source('TOSTtwo.bf.R') #Load function for standardized mean differences
source('TOSTr.bf.R') #Load function for standardized mean differences
source('Leftovers/bf_Baguley.R') #Load function by Baguley Kaye: http://danny-kaye.co.uk/Docs/Dienes_functions.txt
source('Wiens_functions/BF_t.R') #Load function by Wiens
source('Wiens_functions/BF_U.R') #Load function by Wiens

## This function transforms r to Fisher's z and calculates its SE
## Required for Wienes and Baguley&Kaye
bayes.r <- function(r, N)
{
  Fisherz = 0.5*log((1 + r)/(1 - r))
  zSE = 1/sqrt(N-3)
  return(c(Fisherz, zSE))
}

#Working example 1
# Example 5 from Lakens, Scheel & Isager
# Kahane reported r(229)=-0.04,p=.525,N=231
n <- 231
r <- -0.04206585
se_r <- sqrt((1-r^2)/(n-2)) #calculate standard error for correlation (not needed)
sem_obt <- bayes.r(r, n)[2] #calculate Fisher'z Z standard error from correlation
r_obt <- bayes.r(r, n)[1] #calculate Fisher's Z from correlation

#Note TOSTr.bf needs r and n, but BF_t needs Fisher's Z and se (and dftheory = 10000 to mirror normal)
TOSTr.bf(n = n, r = r, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.0, se_prior = 0.2, df_prior=10000) # B = 0.21
BF_t(meantheory=0, sdtheory=.2, dftheory=10000, meanobtained = r_obt, semobtained = sem_obt, dfobtained = n-2, tail = 1)
Bf(sem_obt, r_obt, 0, meanoftheory=0, sdtheory=.2, tail=1) 

#Normal
TOSTr.bf(n = n, r = r, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "normal", effect_prior = 0.0, se_prior = 0.2, df_prior=10000) # B = 0.21
BF_t(meantheory=0, sdtheory=.2, dftheory=10000, meanobtained = r_obt, semobtained = sem_obt, dfobtained = n-2, tail = 2)
Bf(sem_obt, r_obt, 0, meanoftheory=0, sdtheory=.2, tail=2) 

# Same data as Example 5 in Lakens et al, but different prior: 
# Changed prior to 0.5
# HalfNormal
TOSTr.bf(n = n, r = r, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.0, se_prior = 0.5, df_prior=10000) # # B = 0.08
BF_t(meantheory=0, sdtheory=.5, dftheory=10000, meanobtained = r_obt, semobtained = sem_obt, dfobtained = n-2, tail = 1) # 0.08 - MATCH wienes
Bf( sem_obt, r_obt, 0, meanoftheory=0, sdtheory=.5, tail=1) # 0.08430528, MATCH B&K

# Normal
TOSTr.bf(n = n, r = r, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "normal", effect_prior = 0.0, se_prior = 0.5, df_prior=10000) # B = 0.03
BF_t(meantheory=.0, sdtheory=.5, dftheory=10000, meanobtained = r_obt, semobtained = sem_obt, dfobtained = n-2, tail = 2) # 03, MATCH Wiens
Bf(sem_obt, -0.04002135, 0, meanoftheory=.5, sdtheory=.5/2, tail=2) # 0.03474579, matches Baguley & Kaye

# Uniform
TOSTr.bf(n = n, r = r, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "uniform", uniform_lower_bound = 0,uniform_upper_bound = .5) # B = 0.11
BF_U(LL = 0, UL = .5, meanobtained = r_obt, semobtained = sem_obt, dfobtained = n-2) # 0.11
BF_U(LL = 0, UL = .5, meanobtained = r, semobtained = sem_obt, dfobtained = n-2) # 0.11