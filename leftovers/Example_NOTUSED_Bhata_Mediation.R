# BACKGROUND: Bhata et al (2017) looked at the extent to which parental education (coded as 0/1) predicts life satisfaction (Likert scale, from 1 = very dissatisfied to 5 = very satisfied) and how this might be mediated by the respondent's education (coded as 1= never been to school,  6 = postgraduate).
# They obtained a rather counter-intuitive set of findings that challenge the normal way of thinking about mediation. Let's start with some imaginary findings that fit a more usual expectations about how mediation works. 
# 
# So imagine: 
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
# 
# In fact the indirect effects, in raw units, are all as found by Bhata et al .  We have made up the total effect (i.e. simple regression of life satisfaction on parental education) and direct effect (i.e. raw regression weight for life satisfaction on parental educaiton when respondent's education is also in the equation).
# 
# STRATEGY:
#   Total effect = direct effect + indirect effect;
# Or In this case
# .10  = .04 + .06.
# 
# So standardly the total effect is taken as the largest of those three effects and the question is whether either the direct or indirect effect are still there or reduced, or indeed reduced to being not there.
# This gives us a simple heuristic for the size of the direct and total indirect effects we could expect: The maximum we could expect is the total effect. So for Bayes factors testing the direct effect we can model H1 with a uniform from 0 to the total effect.
# Given the number of subjects, for significance testing, everything becomes z-tests. For this total effect, z = .10/.02 = 5.00, significant.  For the direct effect, z = .04/.025 = 1.60, non-significant. A significant total effect has been reduced to a non-significant direct effect, so apparently there is complete mediation. Or is there?
#   By either Bayes factors or inference by intervals, we need to establish support for H0 for the direct effect to declare complete mediation.
# To model H1 we could can use heuristics already mentioned for helping in the absence of relevant past studies. For example, the average life satisfaction for uneducated parents in Bhata et al was 3.69.  Thus the most educated parents could raise life satisfaction is 5 - 3.69 = 1.3. So we could model H1 as a half-normal with SD = 1.3/2 = 0.65 Likert units. (In fact, we can use this to test the total effect, the direct effect and the total indirect effect.) 
# 
# ANALYSIS:
#   
#   For the total effect
# BH(0, 0.65) = 8165.33. This confirms the significance test. For inference by intervals, 95% CI[.06, .14], so there is a total effect with RR[0,0.06].
# Now we could test the direct effect in the same way, i.e. with SD = 0.65. But if we use the mediation specific heuristic, we model H1 with U[0, .10] (or with a half-normal with SD = .05).  The latter is more informed and restricts the possible effect size in a more plausible way. In general, the other default heuristics will be more vague, and one should feel pressure to use specific information when one has it. So using the mediation specific heuristic for the direct effect
# BU[0,.10] =  2.11, insensitive. That is, there is not clear support for H0; we can't conclude that there is evidence for complete mediation. For equivalence testing, 95% CI[-0.01, 0.09].  Now if we wanted to conclude there was a total effect, we are obliged to keep the minimally interesting effect size below 0.06; but that means we are obliged to conclude the test of the direct effect is insensitive. So again we couldn't conclude there is complete mediation.
# Is there any mediation?
#   In order to safely assume normality, we can stick to raw coefficients, and test each indirect effect separately. Then we can conclude there is mediation if there is evidence for each indirect effect separately. We can conclude that there is no mediation if there is evidence for H0 for either indirect effect. This is an improvement on the method of joint significance of the two paths. The latter method declares no mediation if either path is non-significant. But this is baseless. One should require evidence for no effect for either path before concluding no mediation.
# In the current example, consider the first path (first indirect effect). Using the heuristic that the maximum we could expect is indicated roughly by scale ranges, we have maximum  = (6-1)/(1-0) = 5. So the rule of thumb is use a half-normal with SD = 2.5.  (Consider the other method I just used. The mean respondent's education for uneducated parents was 1; so the improvement in respondent's education by having educated parents could be at most 5 units; so SD for half-normal should be 2.5, an identical answer to the previous method.)
# BH(0,2.5) = 9.67 x 10162. Bhata et al  have discovered unequivocally that children's educational level goes up with parent's!
#   For inference by intervals, 95%CI[1.78, 2.06]. So long as the minimally interesting effect size is smaller than 1.78, we can reject null region hypothesis.
# For the second indirect effect, we have a possible maximum of (5-1)/(6-1) = 0.8, so use a half-normal with SD = 0.4.,
# BH(0,0.4) = 12.59 x 104. Strong evidence for the second indirect effect.  For inference by intervals, 95%CI[0.020, 0.042]. Is a change of .04 Likert rating units per rating unit really too small to care about?  If you find you care even about .02, we have established the second indirect effect. But you might think that is really too small and we may have established a lack of any mediation effect we could care about. This makes an interesting contrast to the Bayes factor and significance test; I think both make sense for the jobs they do here.
# 
# To go back to the data, this is what they actually got:
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   The only values that have changed are the total effect and the direct effect. The interesting thing they find is that the total effect is reduced from very small (and maybe zero) to negative. So controlling for respondent's education, parental education may reduce life satisfaction (for Indians). From our point of view the only thing this changes is my mediation specific heuristic: It now doesn't make sense to treat the total effect as a maximum for the direct effect. And that illustrates that all these heuristics are always subject to the scientific judgment that they are appropriate ways of modelling the relevant scientific knowledge in each case.
# Now in modelling H1, if we want to get evidence for a surprising negative effect, we need to allow a negative effect. So one could test a half-normal in the negative direction,
# BH(0,0.65) = 0.55. Insensitive.  Given the effect is surprising and small, this strikes me as very reasonable.  We need more evidence, given how little we know on other grounds for expecting this size and direction, before being swayed much.  For inference by intervals, 95%CI[0, .10].  If my first made up data seemed interesting (where I put a total effect of .10), then the conclusion is also suspend judgment.
# 
# CONCLUSION: Now we are in a position to give guidelines for mediation. The overriding principle is: Favour H0 only when there are grounds for doing so; favour H1 only when there are grounds for doing so. Combine this with the method of joint significance for two paths, and we have a basis for Bayes factors or inference by intervals/equivalence testing for drawing conclusions about mediation in a principled way (backgrounding for now all other issues, e.g. the mediation model should be scientifically motivated etc etc):
#   
#   For the two indirect effects:
#   If B < 1/3 for either then evidence for no mediation; 
# if B > 3 for both then at least partial mediation;
# if either insensitive AND the other B > 1/3, then suspend judgment.
# 
# OR
# If CI in null region for either then conclude no mediation;
# If CI outside null region for both then at least partial mediation;
# if either insensitive AND the other does not show equivalence, then suspend judgment.
# 
# ALSO
# If B > 3 for the direct effect then there is not full mediation
# If B < 1/3 for the direct effect then there is full mediation.
# (Consider using a uniform from 0 to the total effect to model H1 in this case.)
# 
# OR
# If the CI is outside the null region for the direct effect then there is not full mediation
# If the confidence interval is within the null region for the direct effect then there is full mediation.
# 
# 


# direct effect (parental education -> quality of life)

sesoi <- 1.73 / 2  # Quality of life SESOI set to half a standard deviation based on Norman, Sloan and Wyrwich (2003)
bound.u <-  sesoi
bound.l <- -sesoi

m <- -0.05
se <- 0.025
z <- m/se

p.nhst <- 2*pnorm(q = z)
p.tost.u <- pnorm(q = z, mean = bound.u/se, lower.tail = TRUE)
p.tost.l <- pnorm(q = z, mean = bound.l/se, lower.tail = FALSE)

p.nhst  # NHST result
max(p.tost.l, p.tost.u)  # TOST result


# indirect effect 1 (parental education -> offspring education)

sesoi <- 0.01  # This effect does not concern QOL. Instead of using QOL SD/2, SESOI is here based on the effect corresponding to a one point difference in self-reported education between children of educated and uneducated parents, in 1/100 of cases.
bound.u <-  sesoi
bound.l <- -sesoi

m <- -1.92
se <- 0.07
z <- m/se

p.nhst <- 2*pnorm(q = z)
p.tost.u <- pnorm(q = z, mean = bound.u/se, lower.tail = TRUE)
p.tost.l <- pnorm(q = z, mean = bound.l/se, lower.tail = FALSE)

p.nhst  # NHST result
max(p.tost.l, p.tost.u)  # TOST result


# indirect effect 2 (offspring education -> quality of life)

sesoi <- 1.73 / 2  # Quality of life SESOI set to half a standard deviation based on Norman, Sloan and Wyrwich (2003)
bound.u <-  sesoi
bound.l <- -sesoi

m <- -0.031
se <- 0.0056
z <- m/se

p.nhst <- 2*pnorm(q = z)
p.tost.u <- pnorm(q = z, mean = bound.u/se, lower.tail = TRUE)
p.tost.l <- pnorm(q = z, mean = bound.l/se, lower.tail = FALSE)

p.nhst  # NHST result
max(p.tost.l, p.tost.u)  # TOST result


source("TOSTz2.raw.bf.R")

TOSTz2.raw.bf(es = m,
              se = se, 
              n = 6363, 
              k = 5, 
              low_eqbound = -0.1, 
              high_eqbound = 0.1, 
              prior_dist = "normal", 
              effect_prior = 0, 
              se_prior = 2.5, 
              df_prior = 10000)

