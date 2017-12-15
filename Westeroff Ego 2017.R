#Some difference between our code and Zoltan's calculations:
#BF for r = 0.44 (CI the same)
#CI different for r <- 0.07
#BF and CI different for r <- 0.14
#BF and CI the same for r <- 0.12


#Load functions
source('TOSTr.bf.R') #Load function for standardized mean differences
source('Leftovers/bf_Baguley.R') #Load function by Baguley Kaye: http://danny-kaye.co.uk/Docs/Dienes_functions.txt

## This function transforms r to Fisher's z and calculates its SE
## Required for Wienes and Baguley&Kaye
bayes.r <- function(r, N)
{
  Fisherz = 0.5*log((1 + r)/(1 - r))
  zSE = 1/sqrt(N-3)
  return(c(Fisherz, zSE))
}


#"The first hypothesis stated that neuroticism is related to despair and extraversion and openness to experience to ego integrity. We first analyzed the bivariate relations of the personality traits to ego integrity and despair (Table 3). More neuroticism has a significant relation to more despair but not to ego integrity. "

n <- 218
r <- 0.44
sem_obt <- bayes.r(r, n)[2] #calculate Fisher'z Z standard error from correlation
sem_obt
r_obt <- bayes.r(r, n)[1] #calculate Fisher's Z from correlation
r_obt

#Bayes factor:
# A plausible maximum for a raw regression slope is often provided by (range of DV)/(range of IV) = 5/4 = 1.25.
# So as smaller values more likely than larger values use half-normal with SD = 0.6 (i.e. half the maximum).


#Note TOSTr.bf needs r and n, but BF_t needs Fisher's Z and se (and dftheory = 10000 to mirror normal)
TOSTr.bf(n = n, r = r, low_eqbound_r = -0.1830961, high_eqbound_r = 0.1830961, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.0, se_prior = 0.6, df_prior=10000) # B = 0.21

#Note Zoltan reports BH(0, 0.6) = 3.36 × 109
# Our formula gives BF 466086098 - Not the same!

#Zoltan: Thus 95% CI on Fisher's z transformed r= [0.34, 0.60] so 95% CI on r [0.32, 0.54]  (You may want 90% intervals of course)
#TOSTr gives 95% CI of 0.326;0.541


#Now the interesting claim - that there is no correlation of N with ego integrity.

#Correlation of Neuroticism with ego integrity = -.07
#Fishers z = 0.07, SE = 1/???(218-3) = .068 as before.  z-test = 1.03
#Raw regression slope of ego integrity as DV, N as IV = -.07 X 4.2/2.4 = 0.12 ego integrity rating units per neuroticism rating unit.
#0.07/SE = 1.02; thus SE of raw regression slope = 0.07.
#95% CI [-.07, .14]
#(note the 95% CI for the TOSTr function is -0.063;0.201)

n <- 218
r <- 0.07
sem_obt <- bayes.r(r, n)[2] #calculate Fisher'z Z standard error from correlation
sem_obt
r_obt <- bayes.r(r, n)[1] #calculate Fisher's Z from correlation
r_obt

TOSTr.bf(n = n, r = r, low_eqbound_r = -0.14, high_eqbound_r = 0.14, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.0, se_prior = 0.6, df_prior=10000) # B = 0.321296

#Zoltan: Bayes factor:
#  BH(0, 0.6) = 0.32
# 95% CI [-.07, .14]
#Moderate evidence for H0
#We get the same with our function, BF: 0.321296
# 95% CI [-.063, .201] - This is different!


#But: The correlation is then 0.07 in a one-sided predicted direction. Does that make sense? 
#Maybe a normal distribution with -0.07 is better?

r <- -0.07

TOSTr.bf(n = n, r = r, low_eqbound_r = -0.2, high_eqbound_r = 0.2, alpha = 0.05, prior_dist = "normal", effect_prior = 0.0, se_prior = 0.6, df_prior=10000)

# Zoltan: Equivalence testing:
# It all hangs on the minimally interesting effect size and I think we are in the region of reasonable dispute.  Is 0.1 rating units per rating unit too small to care about?  Maybe. The author takes a significant correlation of .14 as an effect worth talking about (openness with ego integrity, raw slope = 0.2) and a non-significant one of 0.12 (openness with despair, raw slope = 0.1) as one worth denying. So based on the internal evidence within the text (bearing in mind the author likely did not think about these issues), we could take a null region to be -0.2, 0.2].  Then equivalence testing and Bayes agree in this case in saying we have grounds for favouring H0.

# But maybe a correlation of r = 0.13 as bound makes more sense - after all, the author is happy to interpret r = 0.14 as meaningful because significant, and r = 0.12 as not meaningful.
TOSTr.bf(n = n, r = r, low_eqbound_r = -0.14, high_eqbound_r = 0.14, alpha = 0.05, prior_dist = "normal", effect_prior = 0.0, se_prior = 0.6, df_prior=10000)


r <- 0.14
sem_obt <- bayes.r(r, n)[2] #calculate Fisher'z Z standard error from correlation
sem_obt
r_obt <- bayes.r(r, n)[1] #calculate Fisher's Z from correlation
r_obt
TOSTr.bf(n = n, r = r, low_eqbound_r = -0.14, high_eqbound_r = 0.14, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.0, se_prior = 0.6, df_prior=10000)

# Zoltan: BH(0, 0.6) = 2.25
# 95% CI [0, .4]
# We find BF = 1.80 - this is different
# 95% CI [0.007, .268] - this is different

r <- 0.12
TOSTr.bf(n = n, r = r, low_eqbound_r = -0.14, high_eqbound_r = 0.14, alpha = 0.05, prior_dist = "halfnormal", effect_prior = 0.0, se_prior = 0.6, df_prior=10000) # B = 0.321296

#Zoltan: BH(0, 0.6) = 1.01
# 95% CI [-0.01, .25].
# We find 1.01141
# 95% CI [-0.013;0.249]
