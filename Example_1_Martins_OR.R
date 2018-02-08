# BACKGROUND: Martins et al (2016) investigated emotion regulation and whether any age difference in preference for distraction over reappraisal occurred for positive stimuli or for negative stimuli. 
# 
# STRATEGY: Here we can base expectations on a past study. Schiebe et al (2015) found older adults preferred distraction over reappraisal (48.5% of the time) to a greater extent than younger adults (40.5%). This is an odds ratio of (48.5 x (1 - 40.5) ) / (40.5 x (1 - 48.5)) = 1.4. (So ln OR = 0.34.) Thus we have a rough estimate of the scale of the effect for any age difference in preference in Martins et al.
# 
# ANALYSIS: As presented in the target paper, for positive emotion, the difference in age preferences for distraction over reappraisal was non-significant, ln OR = -0.11, SE = 0.16, p = .48, with older adults choosing distraction less than younger, i.e. opposite to what one might expect from Schiebe. We might ignore direction, as previous work has found the preference for strategy can depend on e.g. intensity. So we should use the normal for modelling H1.
# Then BN(0,0.34) = 0.51, non-evidential with RR [0, ]
# 
# Equivalence testing:
#   We need to decide a minimally interesting value.  What would actually be minimally interesting?
#   If the preferences were
# Distraction  Reappraisal
# Old                    51%                  49%
#   Young                49%                  51%
#   OR = 1.1 and ln OR ??? 0.1
# 
# Or
# 
# Distraction  Reappraisal
# Old                    52%                  48%
#   Young                48%                  52%
#   OR = 1.2 and ln OR ??? 0.2
# What we really want is an objective reason for making a choice of the minimally interesting effect size: We didn't pick the number but pointed to it as it was found somewhere else. In this case it may be hard to specify a null region precisely. But we can specify a robustness region over which a conclusion holds. As the 95% CI is [0.65, 1.22] the conclusion is support for the null region with RR for the smallest OR of interest, [1.22, ???], and suspend judgement if the smallest effect of interest is anything lower than 1.22.
# 
# 
# CONCLUSION
# Taking a scale of effect from a past study was the easiest way to objectively characterize the predicted effect in this case, and a Bayes factor based on this scaling indicated the evidence was weak for H0.  A conclusion to suspend judgment matched an intuitive judgment based on equivalence testing.
