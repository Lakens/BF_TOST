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
TOSTr.bf(n = 231, r = -0.04, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "uniform", uniform_lower_bound = 0,uniform_upper_bound = .5) # B = 0.11
BF_U(LL = 0, UL = .5, meanobtained = r_obt, semobtained = sem_obt, dfobtained = 10000) # 0.11
BF_U(LL = 0, UL = .5, meanobtained = r, semobtained = sem_obt, dfobtained = 10000) # 0.11


Bf(sem_obt , r_obt , 1, lower=0, upper=.5) # B=0.1084745






# Example 5 from Lakens, Scheel & Isager: All calculators agree (DL: Very small difference)
# Kahane reported r(229)=-0.04,p=.525,N=231
bayes.r(-0.04, 231) #  -0.04002135  0.06622662
# HalfNormal
TOSTr.bf(n = 231, r = -0.04, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.0, se_prior = 0.2, df_prior=10000) # B = 0.21
TOSTr.bf(n = 231, r = -0.04, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.2) # B = 0.21
BF_t(meantheory=0, sdtheory=.2, dftheory=10000, meanobtained=-0.04, semobtained=0.06622662, dfobtained=229, tail = 1) # 0.21 - MATCH wienes
Bf( 0.06622662, -0.04, 0, meanoftheory=0, sdtheory=.2, tail=1) # 0.2118514, MATCH B&K
# Normal
TOSTr.bf(n = 231, r = -0.04, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "normal", effect_prior = 0.2) # B = 0.09
BF_t(meantheory=.2, sdtheory=.2/2, dftheory=10000, meanobtained=-0.04002135, semobtained=0.06622662, dfobtained=10000, tail = 2) # 0.09, MATCH Wiens
Bf( 0.06622662, -0.04002135, 0, meanoftheory=.2, sdtheory=.2/2, tail=2) # 0.08948947, matches Baguley & Kaye
# Uniform
TOSTr.bf(n = 231, r = -0.04, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "uniform", effect_prior = 0.2,uniform_lower_bound= 0,uniform_upper_bound=.5) # B = 0.11
BF_U(LL=0, UL=.5, meanobtained=-0.04002135, semobtained=0.06622662, dfobtained=100000) # 0.11
Bf(0.06622662 , -0.04002135 , 1, lower=0, upper=.5) # B=0.1084745


# Same data as Example 5 in Lakens et al, but different prior: 
# Changed prior to 0.5
bayes.r(-0.04, 229) #  -0.04002135  0.06622662
# HalfNormal
TOSTr.bf(n = 231, r = -0.04, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.5, df_prior=10000) # # B = 0.08
BF_t(meantheory=0, sdtheory=.5, dftheory=10000, meanobtained=-0.04, semobtained=0.06622662, dfobtained=10000, tail = 1) # 0.08 - MATCH wienes
Bf( 0.06622662, -0.04, 0, meanoftheory=0, sdtheory=.5, tail=1) # 0.08430528, MATCH B&K
# Normal
TOSTr.bf(n = 231, r = -0.04, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "normal", effect_prior = 0.5) # B = 0.03
BF_t(meantheory=.5, sdtheory=.5/2, dftheory=10000, meanobtained=-0.04002135, semobtained=0.06622662, dfobtained=10000, tail = 2) # 03, MATCH Wiens
Bf( 0.06622662, -0.04002135, 0, meanoftheory=.5, sdtheory=.5/2, tail=2) # 0.03474579, matches Baguley & Kaye
# Uniform
TOSTr.bf(n = 231, r = -0.04, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "uniform", effect_prior = 0.2,uniform_lower_bound= 0,uniform_upper_bound=.5) # B = 0.11
BF_U(LL=0, UL=.5, meanobtained=-0.04002135, semobtained=0.06622662, dfobtained=100000) # 0.11
Bf(0.06622662 , -0.04002135 , 1, lower=0, upper=.5) # B=0.1084745


# Same prior as Example 5 in Lakens et al, but data: 
# r changed to 0.40, n changed to 100
bayes.r(0.40, 98) #  0.4236489  0.1015346
# HalfNormal
TOSTr.bf(n = 98, r = 0.40, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.2) # # B = 0.40
BF_t(meantheory=0, sdtheory=.2, dftheory=10000, meanobtained=0.4236489, semobtained=0.1015346, dfobtained=10000, tail = 1) # 913.16
Bf( 0.1015346, 0.4236489, 0, meanoftheory=0, sdtheory=.2, tail=1) # 917.2971
# Normal
TOSTr.bf(n = 98, r = 0.40, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "normal", effect_prior = 0.2) # B = 615
BF_t(meantheory=.2, sdtheory=.2/2, dftheory=10000, meanobtained=0.4236489, semobtained=0.1015346, dfobtained=10000, tail = 2) # 1245.96, MATCH Wiens
Bf( 0.1015346, 0.4236489, 0, meanoftheory=.2, sdtheory=.2/2, tail=2) # 1254.208, matches Baguley & Kaye
# Uniform
TOSTr.bf(n = 98, r = 0.40, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "uniform", effect_prior = 0.2,uniform_lower_bound= 0,uniform_upper_bound=.5) # B = 1103.54
BF_U(LL=0, UL=.5, meanobtained=0.4236489, semobtained=0.1015346, dfobtained=100000) # 2374.34
Bf(0.1015346 , 0.4236489 , 1, lower=0, upper=.5) # B=2379.438




########### Larger raw effects generate different Bs to those obtained by Wienes and Baguley&Kaye
bayes.r(0.5, 50) #  0.5493061 0.1428571
# HalfNormal
TOSTr.bf(n = 50, r = 0.50, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.2) # B = 61.01
BF_t(meantheory=0, sdtheory=.2, dftheory=10000, meanobtained=0.5493061, semobtained=0.1428571, dfobtained=10000, tail = 1) # 154.84, 
Bf( 0.1428571, 0.5493061, 0, meanoftheory=0, sdtheory=.2, tail=1) # 155.2079, matches Baguley & Kaye
# Normal
TOSTr.bf(n = 50, r = 0.5, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "normal", effect_prior = 0.2) # # B = 71.74
BF_t(meantheory=.2, sdtheory=.2/2, dftheory=10000, meanobtained=0.5493061, semobtained=0.145865, dfobtained=10000, tail = 2) # 
Bf( 0.145865, 0.5493061, 0, meanoftheory=.2, sdtheory=.2/2, tail=2) # 140.8387, matches Baguley & Kaye
# Uniform
TOSTr.bf(n = 50, r = 0.5, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "uniform", effect_prior = 0.2 ,uniform_lower_bound= 0,uniform_upper_bound=.5) # B = 153.83
BF_U(LL=0, UL=.5, meanobtained=0.5493061, semobtained=0.145865, dfobtained=100000) # 322.91
Bf(0.145865 , 0.5493061 , 1, lower=0, upper=.5) # B=323.6547

n <- 237
r <- -0.05
se_r <- sqrt((1-r^2)/(n-2))
bayes.r(r, n)
sem_obt <- bayes.r(r, n)[2]
r_obt <- bayes.r(r, n)[1]

TOSTr.bf(n = n, r = r, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.0, se_prior = 0.2, df_prior=10000) # B = 0.21
BF_t(meantheory=0, sdtheory=.2, dftheory=10000, meanobtained = r_obt, semobtained = sem_obt, dfobtained = n-2, tail = 1) # 0.21 - MATCH wienes
Bf(sem_obt, r, 0, meanoftheory=0, sdtheory=.2, tail=1) # 0.2118514, MATCH B&K


