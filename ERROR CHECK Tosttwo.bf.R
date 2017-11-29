## TOST Error Checks: tosttwo.bf
## Testing TOST results against other BF scripts.

## The calculators in this error check are:
# TOSTtwo.bf
# Baguley & Kaye:      http://danny-kaye.co.uk/Docs/Dienes_functions.txt
# Wienes:              https://figshare.com/authors/Stefan_Wiens/3862558




## This function transforms r to Fisher's z and calculates its SE
## Required for Wienes and Baguley&Kaye



## This function calculates the SEM which is needed for Wienes and Baguley&Kaye
bayes.two<-function(m1, sd1, n1, m2, sd2, n2)
{ 
  mdif = m1-m2
  SE = (sqrt((((n1 - 1)*(sd1^2)) + (n2 - 1)*(sd2^2))/((n1+n2)-2))) * sqrt(1/n1 + 1/n2)
  return(c(mdif,SE))
}


# Example 2 from Lakens, Scheel & Isager: All calculators agree.
# Brandt results: M1=4.66, SD1=1.19, N1=50, M2=4.79, SD2=1.09, N2=50 (replication)
# Banerjee results:  M1=5.30, N1=20, M2=4.71, N2=20 (replication)
bayes.two(4.66, 1.19, 50, 4.79, 1.09, 50) #  -0.1300000  0.2282192
# HalfNormal
TOSTtwo.bf(m1=4.66,m2=4.79,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound_d=-0.4929019,high_eqbound_d=0.4929019, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 5.30-4.71) # B = 0.25
BF_t(meantheory=0, sdtheory=5.30-4.71, dftheory=10000, meanobtained=-0.1300000, semobtained=0.2282192, dfobtained=10000, tail = 1) # 0.25 - MATCH wienes
Bf( 0.2282192, -0.1300000, 0, meanoftheory=0, sdtheory=5.30-4.71, tail=1) # 0.2452931, MATCH B&K
# Normal
TOSTtwo.bf(m1=4.66,m2=4.79,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound_d=-0.4929019,high_eqbound_d=0.4929019, var.equal=FALSE, prior_dist = "normal", effect_prior = 5.30-4.71) # B = 0.11
BF_t(meantheory=5.30-4.71, sdtheory=(5.30-4.71)/2, dftheory=10000, meanobtained=-0.1300000, semobtained=0.2282192, dfobtained=10000, tail = 2) # 0.11 - MATCH wienes
Bf( 0.2282192, -0.1300000, 0, meanoftheory=0, sdtheory=5.30-4.71, tail=1) # 0.2452931, MATCH B&K
# Uniform
TOSTtwo.bf(m1=4.66,m2=4.79,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound_d=-0.4929019,high_eqbound_d=0.4929019, var.equal=FALSE, prior_dist = "uniform", effect_prior = 5.30-4.71, uniform_lower_bound= 0,uniform_upper_bound=3) #B = 0.06
BF_U(LL=0, UL=3, meanobtained=-0.1300000, semobtained=0.2282192, dfobtained=10000) # 0.06, MATCH WIENES
Bf(0.2282192,-0.1300000,1,lower=0,upper=3) # 0.06354842, MATCH B&K


# Same data as Example 2 in Lakens et al, but different prior: 
# Changed prior to 0.5
bayes.two(4.66, 1.19, 50, 4.79, 1.09, 50) #  -0.1300000  0.2282192
# HalfNormal
TOSTtwo.bf(m1=4.66,m2=4.79,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound_d=-0.4929019,high_eqbound_d=0.4929019, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0.25) # B = 0.50
BF_t(meantheory=0, sdtheory=0.25, dftheory=10000, meanobtained=-0.1300000, semobtained=0.2282192, dfobtained=10000, tail = 1) # 0.50 - MATCH wienes
Bf( 0.2282192, -0.1300000, 0, meanoftheory=0, sdtheory=0.25, tail=1) # 0.4944465, MATCH B&K
# Normal
TOSTtwo.bf(m1=4.66,m2=4.79,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound_d=-0.4929019,high_eqbound_d=0.4929019, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.25) # B = 0.36
BF_t(meantheory=0.25, sdtheory=0.25/2, dftheory=10000, meanobtained=-0.1300000, semobtained=0.2282192, dfobtained=10000, tail = 2) # 0.36 - MATCH wienes
Bf( 0.2282192, -0.1300000, 0, meanoftheory=0.25, sdtheory=0.25/2, tail=2) # 0.3551301, MATCH B&K
# Uniform
TOSTtwo.bf(m1=4.66,m2=4.79,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound_d=-0.4929019,high_eqbound_d=0.4929019, var.equal=FALSE, prior_dist = "uniform", uniform_lower_bound= 0,uniform_upper_bound=0.5) #B = 0.38
BF_U(LL=0, UL=0.5, meanobtained=-0.1300000, semobtained=0.2282192, dfobtained=10000) # 0.38, MATCH WIENES
Bf(0.2282192,-0.1300000,1,lower=0,upper=0.5) # 0.3786761, MATCH B&K



# Same prior as Example 2 in Lakens et al, but different data: 
# Changed M2=4.00, so Mdif=0.66
bayes.two(4.66, 1.19, 50, 4.00, 1.09, 50) #  0.6600000  0.2282192
# HalfNormal
TOSTtwo.bf(m1=4.66,m2=4.00,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound_d=-0.4929019,high_eqbound_d=0.4929019, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 5.30-4.71) # B = 24.1
BF_t(meantheory=0, sdtheory=5.30-4.71, dftheory=10000, meanobtained=0.6600000, semobtained=0.2282192, dfobtained=10000, tail = 1) # 27.33 - DOES NOT MATCH wienes
Bf( 0.2282192, 0.6600000, 0, meanoftheory=0, sdtheory=5.30-4.71, tail=1) # 27.3166, MATCH B&K
# Normal
TOSTtwo.bf(m1=4.66,m2=4.00,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound_d=-0.4929019,high_eqbound_d=0.4929019, var.equal=FALSE, prior_dist = "normal", effect_prior = 5.30-4.71) # B = 34.59
BF_t(meantheory=5.30-4.71, sdtheory=(5.30-4.71)/2, dftheory=10000, meanobtained=0.6600000, semobtained=0.2282192, dfobtained=10000, tail = 2) # 39.31 - DOES NOT MATCH wienes
Bf( 0.2282192, 0.6600000, 0, meanoftheory=5.30-4.71, sdtheory=(5.30-4.71)/2, tail=2) # 39.36567, DOES NOT MATCH B&K
# Uniform
TOSTtwo.bf(m1=4.66,m2=4.00,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound_d=-0.4929019,high_eqbound_d=0.4929019, var.equal=FALSE, prior_dist = "uniform", effect_prior = 5.30-4.71, uniform_lower_bound= 0,uniform_upper_bound=3) #B = 10.97
BF_U(LL=0, UL=3, meanobtained=0.6600000, semobtained=0.2282192, dfobtained=10000) # 12.43, DOES NOT MATCH WIENES
Bf(0.2282192,0.6600000,1,lower=0,upper=3) # 12.46155, DOES NOT MATCH B&K




