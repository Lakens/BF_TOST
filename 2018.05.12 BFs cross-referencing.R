source("Wiens_functions/DienesMcLatchie.R")
source("BF_t.R")

## Run t function to calculate SE and mdif
t.calc<-function(m1, sd1, n1, m2, sd2, n2)
{ 
  mdif = m1-m2
  SE = (sqrt((((n1 - 1)*(sd1^2)) + (n2 - 1)*(sd2^2))/((n1+n2)-2))) * sqrt(1/n1 + 1/n2)
  return(c(mdif,SE))
}
t.calc(0.46, 0.17, 53, 0.50, 0.18, 48) # 0.03483295, SEM for replication



## Run function to transform r to fisher z
fisherz <- function(r, N)
{
  Fisherz = 0.5*log((1 + r)/(1 - r))
  zSE = 1/sqrt(N-3)
  L95CI = Fisherz - zSE*1.96
  U95CI = Fisherz + zSE*1.96
  return(c(Fisherz, zSE, L95CI, U95CI))
}
fisherz(-.07, 218)








## Example 1a: Range of Scale
t.calc(0.338, 0.1945958, 32, 0.321, 0.1945958, 32)
# mdif = 0.017
# sem = 0.0086

# TOSTER BF = 0.13
Bft( 0.04864895, 0.017 , 10000, meanoftheory=0, sdtheory=0.50, dftheory=1000, tail=1) # 0.132556, Dienes + McLatchie
BF_t(0, 0.50, 10000, 0.017, 0.04864895, 10000, tail = 1) # 0.129537, Wienes



## Example 1b
t.calc(0.338, 0.1945958, 32, 0.321, 0.1945958, 32)
# mdif = 0.017
# sem = 0.04864895

# TOSTER BF = 0.42
Bft( 0.04864895, -0.017 , 10000, meanoftheory=0, sdtheory=0.08, dftheory=1000, tail=1) # 0.4144506, Dienes + McLatchie
BF_t(0, 0.08, 10000, -0.017, 0.04864895, 10000, tail = 1) # 0.41457, Wienes







## Example 2a: Age group 62-69 (1) and 70-79 (2)
m1 <- 2.03
m2 <- 1.98
n1 <- 1020
n2 <- 1015
sd1 <- 0.084*sqrt(n1)
sd2 <- 0.057*sqrt(n2)
t.calc(m1, sd1, n1, m2, sd2, n2)
# mdif = 0.05
# sem = 0.1016063

# TOSTER BF = 0.128074
Bft( 0.1016063, 0.05 , 10000, meanoftheory=0, sdtheory=1.21, dftheory=1000, tail=1) # 0.1315551
BF_t(0, 1.21, 10000, 0.05, 0.1016063, 10000, tail = 1) # 0.128114


# Example 2b: Age group  70-79 (1) and >80 (2) - DISCREPANCY BETWEEN TOSTER AND THE REST
m1 <- 2.14
m2 <- 1.98
n1 <- 554
n2 <-  1015
sd1 <- 0.102*sqrt(n2)
sd2 <- 0.057*sqrt(n1) 
t.calc(m1, sd1, n1, m2, sd2, n2)
# mdif = 0.16
# sem = 0.1168264

# TOSTER BF = 0.37423
Bft( 0.1168264, -0.16 , 10000, meanoftheory=0, sdtheory=1.21, dftheory=1000, tail=1) # 0.4456625
BF_t(0, 1.21, 10000, 0.16, 0.1168264, 10000, tail = 1) # 0.443553



## Example 2c: Age group 62-69 (1) and >80 (2)
m1 <- 2.14
m2 <- 2.03 
n1 <- 554
n2 <-  1020
sd1 <- 0.102*sqrt(n2)
sd2 <- 0.084*sqrt(n1) 
t.calc(m1, sd1, n1, m2, sd2, n2)
# TOSTER BF = 0.244819
Bft( 0.132123, 0.11 , 10000, meanoftheory=0, sdtheory=1.21, dftheory=1000, tail=1) # 0.2448482
BF_t(0, 1.21, 10000, 0.11, 0.132123, 10000, tail = 1) # 0.241887




## Example 3: Westerhof et al
n <- 218
r <- 0.12
r2 <- 0.14
r3 <- 0.19
fisherz(r, n) # 0.12058103
fisherz(r2, n) # 0.140925576
fisherz(r3, n) # 0.19233717
# Preprint: The Bayes factor was calculated by transforming r to Fishers z, and then (bizarrely) transforming beta to fishers z
# TOSTER BF = 2.56111
Bft( 0.06819943, 0.12058103 , 10000, meanoftheory=0, sdtheory=0.19233717, dftheory=1000, tail=1) # 2.553128
BF_t(0, 0.19, 10000, 0.12058103, 0.06819943, 10000, tail = 1) # 2.571384

# If we are going to use TOSTr, we can just use the standardised coefficients
Bft( 0.06819943, 0.12058103 , 10000, meanoftheory=0, sdtheory=0.140925576, dftheory=1000, tail=1) # 2.913739
BF_t(0, 0.140925576, 10000, 0.12058103, 0.06819943, 10000, tail = 1) # 2.922359

# Or if we are going to use the raw values, I need to know the correct SE of the obtained beta coefficient.
# In Zoltan's workings, this is: 0.06819943
Bft( 0.06819943, .20 , 10000, meanoftheory=0, sdtheory=0.19, dftheory=1000, tail=1) # 30.28237
BF_t(0, .19, 10000, .20, 0.06819943, 10000, tail = 1) # 30.4102

dev.off()



## Example 4: 
## Short delay condition
sd1 <- sqrt(0.17^2+0.14^2-(2*0.85*0.14*0.17))
sd2 <- sqrt(0.12^2+0.12^2-(2*0.85*0.12*0.12))
m1 <- 0.77-0.76 #From Table 3
m2 <- 0.76-0.750001 ##From Table 3. Added a tiny difference due to bug in code the can not deal with a difference of 0.
n1 <- 32 #From Table 1
n2 <- 32 #From Table 1
t.calc(m1, sd1, n1, m2, sd2, n2)
## experiment 1 results
prior_bf <- (0.61-0.54) - (0.64-0.61) #Table 2, young-old = 0.04
# TOSTER BF = 0.43819
Bft( 0.01965324, 0.000001 , 10000, meanoftheory=0, sdtheory=0.04, dftheory=1000, tail=1) # 0.4397402
BF_t(0, 0.04, 10000, 0.000001, 0.01965324, 10000, tail = 1) # 0.439867










