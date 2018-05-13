# Martins, Sheppes, Gross, and Mather (2016) explored the relationship between age and preference for distraction vs. reappraisal as a method for emotion regulation while participants were exposed to images of varying affective intensity.

source("TOSTtwo.bf.R")

### Determine the SESOI:
# Scheibe et al. (2015) previously used the same paradigm to compare distraction vs reappraisal choices in older and younger subjects. 
# They only report outcomes of their regression model and no mean proportions (of distraction choices), which would be necessary to calculate the size of their effect as Cohen's d.
# Instead, we could use the critical effect size approach and calculate how large an effect they *could have* detected in a t-test: 

# Previous study: Scheibe et al. (2015)
n.younger.scheibe <- 38
n.older.scheibe <- 39

# calculating critical effect size for Scheibe et al. (2015)
t.crit <- qt(1-0.05/2, (n.younger.scheibe + n.older.scheibe)-2)
d.crit <- t.crit * sqrt((1/n.younger.scheibe)+(1/n.older.scheibe))



### Martins et al.
# Distraction preferences as % of trials in which distraction was chosen (for NEGATIVE images)
# from the paper (p. 6): "age-related differences in strategy choice for negative images, (Myoung = 0.34 ? 0.03; Molder = 0.32 ? 0.03), t(62) = 0.35, p = .73, d = 0.09 (see Figure 2)."


n.younger <- 32
n.older <- 32
m.younger <- 0.338 # NB! The means and standard deviations used in this example are NOT perfectly equal to those reported in Martins, Sheppes, Gross, and Mather (2016). Probably due to rounding of reported values, the numbers provided in the paper does not lead us to arrive at the same t statistic as the one reported in the paper. We therefore changed the mean and standard deviation values to be as close as possible to numbers that would replicate the t statistic reported. 
sd.younger <- 0.0344 * sqrt(n.younger) 
m.older <- 0.321 
sd.older <- 0.0344 * sqrt(n.older) 



# Bayes factor - treating the proportions as two intervals and running a t-test
# Based on range (can't be bigger than 1)
TOSTtwo.bf(m1 = m.younger, 
           m2 = m.older, 
           sd1 = sd.younger, 
           sd2 = sd.older, 
           n1 = n.younger, 
           n2 = n.older, 
           low_eqbound_d = -d.crit, 
           high_eqbound_d = d.crit, 
           alpha = 0.05, 
           var.equal = FALSE, 
           prior_dist = "halfnormal", 
           effect_prior = 0, 
           se_prior = 0.5, 
           df_prior = 10000) # df from scheibe
# B = 0.13

# Identifying RR lower
TOSTtwo.bf(m1 = m.younger, 
           m2 = m.older, 
           sd1 = sd.younger, 
           sd2 = sd.older, 
           n1 = n.younger, 
           n2 = n.older, 
           low_eqbound_d = -d.crit, 
           high_eqbound_d = d.crit, 
           alpha = 0.05, 
           var.equal = FALSE, 
           prior_dist = "normal", 
           effect_prior = 0, 
           se_prior = 0.147, 
           df_prior = 10000)
# B = 0.13

# Identifying RR upper
TOSTtwo.bf(m1 = m.younger, 
           m2 = m.older, 
           sd1 = sd.younger, 
           sd2 = sd.older, 
           n1 = n.younger, 
           n2 = n.older, 
           low_eqbound_d = -d.crit, 
           high_eqbound_d = d.crit, 
           alpha = 0.05, 
           var.equal = FALSE, 
           prior_dist = "normal", 
           effect_prior = 0, 
           se_prior = 99999999999999999999999999999999999999999999999999999999999999999999999, 
           df_prior = 10000)
# B = 0.13


# Scheibe et al 2015 mean difference:
prop.young.scheibe <- 0.405
prop.old.scheibe <- 0.485
prior.dif <- prop.young.scheibe - prop.old.scheibe # -0.08
obtained.df <- m.younger-m.older # 0.017
TOSTtwo.bf(m1 = m.older, 
           m2 = m.younger, 
           sd1 = sd.older, 
           sd2 = sd.younger, 
           n1 = n.older, 
           n2 = n.younger, 
           low_eqbound_d = -d.crit, 
           high_eqbound_d = d.crit, 
           alpha = 0.05, 
           var.equal = FALSE, 
           prior_dist = "halfnormal", 
           effect_prior = 0, 
           se_prior = 0.08, 
           df_prior = 10000) # df from scheibe
# B = 0.42

TOSTtwo.bf(m1 = m.younger, 
           m2 = m.older, 
           sd1 = sd.younger, 
           sd2 = sd.older, 
           n1 = n.younger, 
           n2 = n.older, 
           low_eqbound_d = -d.crit, 
           high_eqbound_d = d.crit, 
           alpha = 0.05, 
           var.equal = FALSE, 
           prior_dist = "halfnormal", 
           effect_prior = 0, 
           se_prior = 0.000000000000000000000000000000000000000000000000000000000000000000000001,  # 0?
           df_prior = 10000)
# B = 1

# Identifying RR upper
TOSTtwo.bf(m1 = m.younger, 
           m2 = m.older, 
           sd1 = sd.younger, 
           sd2 = sd.older, 
           n1 = n.younger, 
           n2 = n.older, 
           low_eqbound_d = -d.crit, 
           high_eqbound_d = d.crit, 
           alpha = 0.05, 
           var.equal = FALSE, 
           prior_dist = "halfnormal", 
           effect_prior = 0, 
           se_prior = 0.188, 
           df_prior = 10000)
# B = 0.335229

## This is an example where the direction of the prediction (e.g. oldprop - young prop = +0.08) will
## affect the way that users will need to enter m1 and m2. In this example the obtained mdiff is in the 
## opposite direction to that predicted by Schiebe. If you enter m1=m.younger and m2=m.older, then
## the obtained mdiff = +0.017. The problem is we can't use a negative SD on the normal. 
## Not really a problem, just something to consider, and instructions we will have to provide.

