# BACKGROUND:  Sullivan et al 2017 state that "Young men and women's looking was similar, with young men spending 63.6% of their time looking at the eyes (and 36.4% of the time at the mouth) and young women spending 63.0% of the time looking at the eyes: t(58) = 0.10, p = .92. In contrast, older men and women differed in their looking patterns, with older men spending 56.9% of their time looking at the eyes and older women spending 70.7% of their time looking at the eyes: t(56) = 2.35, p = .02."
# 
# STRATEGY: Previous research has found women spend more time looking at eyes than mouth than men do, but using rather different paradigms than the current study; it may be hard to predict an effect size based on past research. So we illustrate one way of 'cheating' relevant information from the data set itself.
# First take the older people. Men looked at the eyes 57% of the time; this means if women looked more they could be between 0 and 43% greater. Assuming small effect sizes more likely, a half-normal could be used with SD = 22%.
# Actual mean difference = 13.8%; SE = 13.8/2.35 = 5.9%
#   BH(0,22) = 6.57, RR[3.4, 58], agreeing with the significant result
# The upper limit takes us outside the scale itself, so the conclusion is robust up to the highest SD we could plausibly take. On the lower end, the claim would be that a difference greater than about 7% is ruled out. This is implausibly restrictive in that we have no grounds for ruling out such changes. Thus, the conclusion is robust and we don't need to finesse the scaling factor any more than we have.
# Inference by intervals.
# 95% CI [2%, 29.6%]
# Now it matters what our minimally interesting effect size is. If it is greater than 2% then we need to suspend judgment. It may be hard to fix the minimally interesting effect size objectively in this case, but 1-2% seems intuitive, in which case we can reject the null region hypothesis, RR[0, 2]
# 
# Now the younger people. Difference = -0.6%, SE = 0.6/0.1 = 6%.  For simplicity we could use the same model of H1 as the previous test, which means it was chosen entirely independent of the data to be tested.
# BH(0,22) = 0.24, moderate evidence for H0 over the H1.
# RR[16, ???]
# The conclusion is not very robust in that we need only have reason to restrict the scaling factor below 16% and the evidence becomes weak.  Indeed in the case of the older people there was a difference of 14% and if we had used this as our scaling factor, the conclusion would have been that the evidence was weak. Thus, we should treat the non-significant result here tentatively.
# Equivalence testing
# 95% CI [-12.6%, 11.4%]
# Whatever the null region, it must surely be contained within this CI, so the conclusion is suspend judgment.  RR[0, 11].
# 
# CONCLUSION
# In this case, a relevant past study was hard to find to fix the scaling of the effect. However, one can sometimes extract information from the study itself to put rough limits on what size effects could be obtained. Note one thing we did not do: We did use the very difference we were testing as the scale factor for predicting that difference. That would be double counting the same mean difference twice, once for making the prediction and twice for the testing of the prediction. We used the data for constraining predictions, leaving degrees for freedom for the difference to match or mismatch those predictions.  Finally, a consideration of robustness turned out to be important for how seriously to take the conclusions.

source("TOSTtwo.bf.R") #Load function for standardized mean differences

m.oldm <- 0.673646266 #Percentage time spent looking at the eyes (recalculated values from table 2)
m.oldf <- 0.656557886
m.yngm <- 0.649131167
m.yngf <- 0.759139802
sd.oldm <- 
sd.oldf
sd.yngm
sd.yngf
n.oldm <- 28
n.oldf <- 30
n.yngm <- 30
n.yngf <- 30
