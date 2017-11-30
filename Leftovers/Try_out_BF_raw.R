
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.4,high_eqbound=0.4, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.5, se_prior=0.191, df_prior=(20+20)-2) # B = 0.08




##### Correct Understanding of TOST?
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.384,high_eqbound=0.384, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0.5) # B = 0.31
# Nonsignificantly different from zero t=0.23, p=0.82, and is equivalent t=3.50, p=.0002.

TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.43) # B = 0.31
# Significantly different from zero t=3.81, p<.001, and is not equivalent t=0.53, p=.70

TOSTtwo.raw.bf(m1=5.25,m2=5.05,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.2) # B = 0.31
# Not significantly different from zero t=1.52, p<.001, and was equivalent t=-1.75, p=.04






# Calculate SE for original study and 
(sqrt((((20 - 1)*(0.59^2)) + (20 - 1)*(0.62^2))/((20+20)-2))) * sqrt(1/20 + 1/20) # 0.1913766, SE for original study
(sqrt((((95 - 1)*(0.95^2)) + (89 - 1)*(0.83^2))/((95+89)-2))) * sqrt(1/95 + 1/89) # 0.131882, SE for replication study


## Normals
# TOST raw calculator original
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0.5) # B = 0.31
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.5) # B = 0.12
# Baguely & Kaye's calculator:
Bf(0.132, 0.03, 0, meanoftheory=0, sdtheory=0.5, tail=1) # 0.30   
Bf(0.132, 0.03, 0, meanoftheory=0.5, sdtheory=0.25, tail=2) # 0.12
# Wiens calculator:
BF_t(0, 0.5, 100000, 0.03, 0.132, (89+95)-2, tail=1) # 0.31
BF_t(0.5, 0.25, 100000, 0.03, 0.132, (89+95)-2, tail=2) # 0.12


## t-distributions
# TOST raw calculator original
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.433, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0, se_prior=0.5, df_prior=(20+20)-2) # B = 0.30
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.4,high_eqbound=0.4, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.5, se_prior=0.191, df_prior=(20+20)-2) # B = 0.08
# Dienes & McLatchie calculator:
Bft(0.132, 0.03, (95+89)-2, meanoftheory=0, sdtheory=0.50, dftheory=(20+20)-2, tail = 1) # 0.3063493
Bft(0.132, 0.03, (95+89)-2, meanoftheory=0.50, sdtheory=0.191, dftheory=(20+20)-2, tail = 2) # 0.07860583
# Wiens calculator:
BF_t(0, 0.5, (20+20)-2, 0.03, 0.132, (89+95)-2, tail=1) # 0.30
BF_t(0.5, 0.191, 100000, 0.03, 0.132, (89+95)-2, tail=2) # 0.08


## Cauchy
# TOST raw calculator original
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0, se_prior=0.5, df_prior=1) # B = 0.25
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.5, se_prior=0.25, df_prior=1) # B = 0.12, using se_prior = 0.5/2
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.5, se_prior=0.191, df_prior=1) # B = 0.10, using actual se_prior
# TOST raw calculator neil
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "halfcauchy", effect_prior = 0.5) # B = 0.25
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "cauchy", effect_prior = 0.5) # B = 0.12, using se_prior = 0.5/2 (default)
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "cauchy", effect_prior = 0.5, se_prior=0.191) # B = 0.10, using actual se_prior
# Dienes & McLatchie calculator:
Bft(0.132, 0.03, (95+89)-2, meanoftheory=0, sdtheory=0.50, dftheory=1, tail = 1) # 0.2549421
Bft(0.132, 0.03, (95+89)-2, meanoftheory=0.50, sdtheory=0.25, dftheory=1, tail = 2) # 0.1029683
# Wiens calculator:
BF_t(0, 0.5, 1, 0.03, 0.132, (95+89)-2, tail=1) # 0.25
BF_t(0.5, 0.191, 1, 0.03, 0.132, (95+89)-2, tail=2) # 0.10



## Checking directionality - Moon and Roeder data
# means one way
TOSTtwo.raw.bf(m1  = 0.50,
               m2  = 0.46,
               sd1 = 0.18,
               sd2 = 0.17,
               n1  = 48,
               n2  = 53,
               low_eqbound  = -0.0625,
               high_eqbound =  0.0625,
               alpha = .05,
               prior_dist="normal",
               effect_prior=0,
               se_prior=0.05
)

TOSTtwo.raw.bf(m1  = 0.50,
               m2  = 0.46,
               sd1 = 0.18,
               sd2 = 0.17,
               n1  = 48,
               n2  = 53,
               low_eqbound  = -0.0625,
               high_eqbound =  0.0625,
               alpha = .05,
               prior_dist="halfnormal",
               effect_prior=0,
               se_prior=0.05
)

# B = 1.47




# means switched around
TOSTtwo.raw.bf(m1  = 0.46,
               m2  = 0.50,
               sd1 = 0.17,
               sd2 = 0.18,
               n1  = 53,
               n2  = 48,
               low_eqbound  = -0.0625,
               high_eqbound =  0.0625,
               alpha = .05,
               prior_dist="halfnormal",
               effect_prior=0,
               se_prior=0.05
)
# B = 0.31


TOSTtwo.raw.bf(m1  = 4.785714,  # Mean of group 1
               m2  = 4.656863,  # Mean of group 2
               sd1 = 1.089725,  # Standard deviation of group 1
               sd2 = 1.189497,  # Standard deviation of group 2
               n1  = 49,  # Number of subjects in group 1
               n2  = 51,  # Number of subjects in group 2
               low_eqbound = -0.6,  # Value for the lower equivalence bound
               high_eqbound =  0.6,  # Value for the higher equivalence bound
               alpha = .05,
               prior_dist="normal",
               effect_prior=0.5
)  # Alpha level for TOST and NHST


TOSTtwo.raw.bf(m1  = 4.785714,  # Mean of group 1
               m2  = 4.656863,  # Mean of group 2
               sd1 = 1.089725,  # Standard deviation of group 1
               sd2 = 1.189497,  # Standard deviation of group 2
               n1  = 49,  # Number of subjects in group 1
               n2  = 51,  # Number of subjects in group 2
               low_eqbound = -0.5,  # Value for the lower equivalence bound
               high_eqbound =  0.5,  # Value for the higher equivalence bound
               alpha = .05,
               prior_dist="uniform",
               uniform_lower_bound = -2,
               uniform_upper_bound = 2
)

#Differences of priors

#Normal
TOSTtwo.raw.bf(m1  = 4.785714,  # Mean of group 1
               m2  = 4.656863,  # Mean of group 2
               sd1 = 1.089725,  # Standard deviation of group 1
               sd2 = 1.189497,  # Standard deviation of group 2
               n1  = 49,  # Number of subjects in group 1
               n2  = 51,  # Number of subjects in group 2
               low_eqbound = -0.4,  # Value for the lower equivalence bound
               high_eqbound =  0.4,  # Value for the higher equivalence bound
               alpha = .05,
               prior_dist="normal",
               effect_prior=0.5,
)  # Alpha level for TOST and NHST


#Half-Normal
TOSTtwo.raw.bf(m1  = 4.785714,  # Mean of group 1
               m2  = 4.656863,  # Mean of group 2
               sd1 = 1.089725,  # Standard deviation of group 1
               sd2 = 1.189497,  # Standard deviation of group 2
               n1  = 49,  # Number of subjects in group 1
               n2  = 51,  # Number of subjects in group 2
               low_eqbound = -0.6,  # Value for the lower equivalence bound
               high_eqbound =  0.6,  # Value for the higher equivalence bound
               alpha = .05,
               prior_dist="halfnormal",
               effect_prior=0.
)  # Alpha level for TOST and NHST



#Uniform
TOSTtwo.raw.bf(m1  = 4.785714,  # Mean of group 1
               m2  = 4.656863,  # Mean of group 2
               sd1 = 1.089725,  # Standard deviation of group 1
               sd2 = 1.189497,  # Standard deviation of group 2
               n1  = 49,  # Number of subjects in group 1
               n2  = 51,  # Number of subjects in group 2
               low_eqbound = -0.4,  # Value for the lower equivalence bound
               high_eqbound =  0.4,  # Value for the higher equivalence bound
               alpha = .05,
               prior_dist="uniform",
               uniform_lower_bound = -1,
               uniform_upper_bound = 1
)

# Does new uniform calculator perform the same as other calculators?
# Previous example, with halfnormal 
# Moon & Roeder 2014 example
TOSTtwo.raw.bf(m1  = 0.46,  # Mean of group 1
               m2  = 0.50,  # Mean of group 2
               sd1 = 0.17,  # Standard deviation of group 1
               sd2 = 0.18,  # Standard deviation of group 2
               n1  = 53,  # Number of subjects in group 1
               n2  = 48,  # Number of subjects in group 2
               low_eqbound = -.0625,  # Value for the lower equivalence bound
               high_eqbound = .0625,  # Value for the higher equivalence bound
               alpha = .05,
               prior_dist="uniform",
               uniform_lower_bound=-0.0625,
               uniform_upper_bound=0.0625
)
# B = 1
## This is still all correct (equivalence tests, BFs):
BF_U(LL=-0.0625, UL=0.0625, meanobtained=-0.04, semobtained=0.03483295, dfobtained=10000) # B = 1
Bf(0.03483295, -0.04, 1, lower=-0.0625, upper=0.0625) # 0.9979883

# Change the range of the uniform distribution
TOSTtwo.raw.bf(m1  = 0.46,  # Mean of group 1
               m2  = 0.50,  # Mean of group 2
               sd1 = 0.17,  # Standard deviation of group 1
               sd2 = 0.18,  # Standard deviation of group 2
               n1  = 53,  # Number of subjects in group 1
               n2  = 48,  # Number of subjects in group 2
               low_eqbound = -.0625,  # Value for the lower equivalence bound
               high_eqbound = .0625,  # Value for the higher equivalence bound
               alpha = .05,
               prior_dist="uniform",
               uniform_lower_bound=0,
               uniform_upper_bound=1
)
# B = 0.02
BF_U(LL=0, UL=1, meanobtained=-0.04, semobtained=0.03483295, dfobtained=10000) # B = 0.02
Bf( 0.03483295,  -0.04, 1, lower=0, upper=1) # B=0.02092337




##### Correct Understanding of TOST?
TOSTtwo.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound_d=-0.384,high_eqbound_d=0.384, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0.5) # B = 0.31
# Nonsignificantly different from zero t=0.23, p=0.82, and is equivalent t=3.50, p=.0002.

TOSTtwo.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound_d=-0.43,high_eqbound_d=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.43) # B = 0.31
# Significantly different from zero t=3.81, p<.001, and is not equivalent t=0.53, p=.70

TOSTtwo.bf(m1=5.25,m2=5.05,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound_d=-0.43,high_eqbound_d=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.2) # B = 0.31
# Not significantly different from zero t=1.52, p<.001, and was equivalent t=-1.75, p=.04


#Half-Normal
TOSTtwo.raw.bf(m1  = 4.785714,  # Mean of group 1
               m2  = 4.656863,  # Mean of group 2
               sd1 = 1.089725,  # Standard deviation of group 1
               sd2 = 1.189497,  # Standard deviation of group 2
               n1  = 49,  # Number of subjects in group 1
               n2  = 51,  # Number of subjects in group 2
               low_eqbound_d = -0.6,  # Value for the lower equivalence bound
               high_eqbound_d =  0.6,  # Value for the higher equivalence bound
               alpha = .05,
               prior_dist="halfnormal",
               effect_prior=1
)  # Alpha level for TOST and NHST



#Uniform
TOSTtwo.bf(m1  = 4.785714,  # Mean of group 1
           m2  = 4.656863,  # Mean of group 2
           sd1 = 1.089725,  # Standard deviation of group 1
           sd2 = 1.189497,  # Standard deviation of group 2
           n1  = 49,  # Number of subjects in group 1
           n2  = 51,  # Number of subjects in group 2
           low_eqbound_d = -0.4,  # Value for the lower equivalence bound
           high_eqbound_d =  0.4,  # Value for the higher equivalence bound
           alpha = .05,
           prior_dist="uniform",
           uniform_lower_bound = -6,
           uniform_upper_bound = 6
)