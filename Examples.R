#Examples

#There seems to be a difference in DF used by ZM and LSI. 100 vs 124
#It makes sense to provide more output for the BF calculation, mainly returning the ES, SE and DF for prior and results

#Load functions

source('TOSTtwo.raw.bf.R') #Load function for raw mean differences
source('TOSTtwo.bf.R') #Load function for standardized mean differences
source('TOSTr.bf.R') #Load function for standardized mean differences
source('Leftovers/bf_Baguley.R') #Load function by Baguley Kaye: http://danny-kaye.co.uk/Docs/Dienes_functions.txt
source('Wiens_functions/BF_t.R') #Load function by Wiens
source('Wiens_functions/BF_U.R') #Load function by Wiens



#

# original study: Banerjee et al., study 1
orig.m1 <- 4.71
orig.m2 <- 5.3
orig.sd1 <- 0.85
orig.sd2 <- 0.97
orig.d <- 0.65
orig.t <- 2.03
orig.p <- 0.049
orig.N <- 40 #group size unknown, therefore equal n assumed
orig.df <- orig.N-2

rep.m1 <- 4.785714
rep.m2 <- 4.656863
rep.sd1 <- 1.089725
rep.sd2 <- 1.189497
rep.n1 <- 49
rep.n2 <- 51
rep.t <- 0.56517
rep.p <- 0.5733
rep.df <- 97.783
rep.d <- (rep.m1 - rep.m2)/sqrt(((rep.n1-1)*rep.sd1^2 + (rep.n2-1)*rep.sd2^2)/(rep.n1+rep.n2-2))
d.33 <- 0.4929019

LL<-0
UL<-6
meanobtained<-0.15
semobtained<-0.2380952
dfobtained<-124
BF_U(LL, UL, meanobtained, semobtained, dfobtained)

#As reported in LSP 2017
TOSTtwo.bf(m1=rep.m1, m2=rep.m2, sd1=rep.sd1, sd2 = rep.sd2, n1 = rep.n1, n2=rep.n2, low_eqbound = -d.33, high_eqbound = d.33, var.equal = FALSE)

TOSTtwo.bf(m1=rep.m1, m2=rep.m2, sd1=rep.sd1, sd2 = rep.sd2, n1 = rep.n1, n2=rep.n2, low_eqbound = -d.33, high_eqbound = d.33, var.equal = TRUE, prior_dist = "uniform", uniform_lower_bound = 0, uniform_upper_bound = 6)

TOSTtwo.raw.bf(m1=4.66,m2=4.79,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound=-0.4929019,high_eqbound=0.4929019, var.equal=TRUE, prior_dist = "normal", effect_prior = 0.25, df_prior = 10000) # B = 0.36

TOSTtwo.raw.bf(m1=rep.m1, m2=rep.m2, sd1=rep.sd1, sd2 = rep.sd2, n1 = rep.n1, n2=rep.n2, low_eqbound = -d.33, high_eqbound = d.33, var.equal = TRUE, prior_dist = "uniform", uniform_lower_bound = 0, uniform_upper_bound = 6)

TOSTtwo.bf(m1=4.66,m2=4.79,sd1=1.19,sd2=1.09,n1=50,n2=50,low_eqbound_d=-0.4929019,high_eqbound_d=0.4929019, var.equal=TRUE, prior_dist = "normal", effect_prior = 0.25, df_prior = 10000) # B = 0.36
