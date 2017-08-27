# 170601
# Stefan Wiens
# people.su.se/~swiens/

# https://prezi.com/view/ZRDBpcbMgBrjcZbDWyH8/

# Zoltan Dienes has several excellent papers on the use of Bayes Factor
# with informed priors. He distinguishes between uniform, half-normal, and
# normal priors. These can be used in the context of t tests with
# his online calculator (see below). This tool is simple to use and helpful,
# but it does not produce illustrations of the results. Also, to increase 
# reproducibility in open science, it would be beneficial to have
# scripted analyses. With scripts, it would also be easier to 
# check the robustness of the results by comparing effects of different priors, 
# as suggested by Dienes. Although some R scripts have already been provided earlier 
# (e.g., Baguley & Kaye, 2010, see Dienes' website), the present scripts are a humble
# attempt to improve these scripts.
#
# https://doi.org/10.3758/s13423-017-1266-z
# http://www.lifesci.sussex.ac.uk/home/Zoltan_Dienes/inference/Bayes.htm
# http://www.lifesci.sussex.ac.uk/home/Zoltan_Dienes/inference/bayes_factor.swf
#
# The present scripts are called "Aladins Bayes Factor in R".
# If you read "Aladins" out loud, it sounds like "a la Dienes", 
# to give credit to Zoltan Dienes and his work.
# The script also fulfills my little wish to have a visualization of the BF.
# (see JASP for beautiful figures, https://jasp-stats.org/ )
#
# BF_t.R and BF_U.R
# The guide below shows how to use these functions.
#
# Phrase everything on the same, unstandardized units. That is, mean and SEM 
# of the data, and the prediction of the H1 must all be in the same units.
# If you want to use standardized units, see Dienes' website on how to convert
# the scores. Alternatively, use JASP or the BayesFactor package in R (see links).
#
# When defining your H1, phrase it so that you would mainly expect a positive effect.
# This makes more sense intuitively. 
# (Another reason is that if you want to test one-tailed with tail = 1, the script assumes that you mean that the effect cannot
# be negative and cuts off the negative tail.)
# If your data are opposite (i.e., negative) to the predicted direction (which is positive), 
# enter a negative mean difference.

# Example (with many more in the user guide):
#  BF_t(10, 5, 10000, -3, 8, 119)
# The BF10 = 0.35 and BF01 = 2.86. The data tend to support the null hypothesis.
# In the calculation, the H1 is modeled as a t distribution centered at 10 (and an
# SEM of 5); however, because the df = 10000, the t distribution is close enough
# to a normal distribution centered at 10 with an SD = 5. 
# The actual data had a mean effect of -3 (i.e., opposite to the prediction), SEM = 8, and df = 119. 
# BF_t(10, 5, 10000, -3, 8, 119) is identical to BF_t(10, 5, 10000, -3, 8, 119,
# tail = 2). Thus, H1 allows for both positive and negative effects.
# 
# Use BF_t.R to run Dienes' half-normal or normal prior. The script is already
# designed to run according to Dienes & Mclatchie (2017)--that is, with a t distribution
# rather than a normal distribution--but if you want a normal distribution, simply
# defines the degrees of freedom (dftheory and/or dfobtained) as 10000. With this setting,
# the t distribution is like a normal distribution.
#
# Use BF_U.R to run Dienes' uniform prior.
#
# If you do not like the resulting figures, just open the scripts and 
# change them. To facilitate this process, I have marked some sections.
# It should be easy to:
# > change the title (which is maybe too informative now)
# > change the size of the figure (plotscale)
# > plot without the pie chart
# > plot the legend ("prior" etc.) on the left rather than right

# The guide below describes many of the examples presented here:  
# Dienes, Z., & Mclatchie, N. (2017). Four reasons to prefer Bayesian analyses
# over significance testing. Psychonomic Bulletin & Review, 1-12. doi:
# 10.3758/s13423-017-1266-z
# However, I have added and modified some examples to illustrate the use of the functions.

# Change this to the directory in which you have the BF_t.R and the BF_U.R
setwd("~/R Stefan/Aladins Bayes Factor in R/")
# You need to source each function. It will be in memory and can be called with a single command.
# That is, you do not actually need to open the BF_t.R and BF_U.R scripts.
# Remember: You can easily modify each script (e.g., if you want the legend on the left).
# To do that, open the script, edit it and save the changes.
# Once you are done, you MUST run the source command again! Otherwise, R will not implement the changes.
source('BF_t.R')

# BF_t.R
# ======
# The BF_t.R is modelled after the script in Dienes & Mclatchie (2017).
#
# It computes the BayesFactor(H1 vs H0) with the H1 defined as a t distribution 
# and the likelihood defined as a t distribution.
# It also plots the Prior and Posterior (and Likelihood) and adds a pie chart.
# As described by Dienes & Mclatchie, it requires six (or seven) inputs
#  Bt(meantheory, sdtheory, dftheory), L = (meanobtained, semobtained, dfobtained) 
#
# The seventh input is tail = 1 or tail = 2, default is tail = 2 (and thus can be omitted).
# If one-tailed, only effects larger than zero are expected.
#
# Example
# BF_t(13.3, 4.93, 72, 5.47, 32.18, 119)
#  same as
# BF_t(13.3, 4.93, 72, 5.47, 32.18, 119, tail = 2)
#
# Dienes online calculator allows for halfnormal and normal H1.
# To get similar effects, simply run as follows:
#
# halfnormal: meantheory=0, dftheory=10000, dfobtained=10000, tail = 1
# BF_t(0, 10, 10000, 20, 10, 10000, tail = 1)
# normal: meantheory=0, dftheory=10000, dfobtained=10000
# BF_t(0, 10, 10000, 20, 10, 10000, tail = 2)
#
# If you want to save the BF10, just assign it.
# BF10 = BF_t(0, 10, 10000, 20, 10, 10000, tail = 1)
# (You can even save the likelihoods for H1 and H0 by changing the return line
#  at the end of the script.)

source('BF_U.R')
# BF_U.R
# ======
# The BF_U.R is similar to BF_t.R but uses a uniform distribution to model the H1.
# BF_U(LL, UL, meanobtained, semobtained, dfobtained)
# BF_U(0, 10, 12, 5, 27)
# = 6.34
# if dfobtained = 10000, the likelihood is modelled by a normal distribution.
# this corresponds to Dienes online calculator.
# BF_U(0, 10, 8, 5, 10000) 
# = 2.71
# BF_U(5, 10, 8, 5, 10000) 
# UL starts at +5
# = 3.44
# BF_U(-5, 1, 8, 5, 10000) 
# UL starts at -5
# = 1.96
# BF_U(0, 10, -1, 5, 10000) 
# sample mean was in opposite direction (ie -1)
# = 0.52
# BF_U(0, 30, -1, 5, 10000) 
# vague H1 (ie UL = 30)
# = 0.18

# high powered nonsignificant
# ===========================
meantheory = 13.3
# t(72) = 2.70 -> SEM = 13.3/2.70 = 4.93
13.3/2.7
sdtheory = 4.93
dftheory = 72
meanobtained = 5.47
# t(119) = 0.17 -> SEM = 5.47/ 0.17 = 32.18
5.47/ 0.17
semobtained = 32.18
dfobtained = 119
#  Bt(meantheory, sdtheory, dftheory), L = (meanobtained, semobtained, dfobtained) 
BF_t(13.3, 4.93, 72, 5.47, 32.18, 119)
# 0.97
BF_U(0, 13.3, 5.47, 32.18, 10000) 
# 1.01
BF_U(0, 200, 5.47, 32.18, 10000) 
# 0.23

# Below is the original from Dienes
# I changed it because the original effect was not significant
# low powered nonsignificant
# ===========================
meantheory = 5
# t(30) = 0.36 -> SEM = 5/0.36 = 14
sdtheory = 14
dftheory = 30
meanobtained = -4
# t(99) = 1.15 -> SEM = 4/ 1.15 = 3.48
semobtained = 3.48
dfobtained = 99
#  Bt(meantheory, sdtheory, dftheory), L = (meanobtained, semobtained, dfobtained) 
BF_t(5, 14, 30, -4, 3.48, 99)
# 0.38
# with doubled SEMobtained?
BF_t(5, 14, 30, -4, 3.48*2, 99)
# 0.44

# Below is the changed version because the original effect was not significant
# low powered nonsignificant
# ===========================
meantheory = 5
# Goal: Effect size = 0.25 because this results in 25% power with 101 subjects in
# the replication.
# ES = mean effect/SD = 5/SD = 0.25
# SD = 20
# fiddled in Lakens (2013) independent samples ES with means, SD, and N
# t(258) = 2.02 -> SEM = 5/2.02 = 2.47
sdtheory = 2.47
dftheory = 258
meanobtained = -4
# t(99) = 1.15 -> SEM = 4/ 1.15 = 3.48
semobtained = 3.48
dfobtained = 99
#  Bt(meantheory, sdtheory, dftheory), L = (meanobtained, semobtained, dfobtained) 
BF_t(5, 2.47, 258, -4, 3.48, 99)
# 0.18
# with doubled SEMobtained?
BF_t(5, 2.47, 258, -4, 3.48*2, 99)
# 0.53
# to run halfnormal...
BF_t(0, 5, 10000, -4, 3.48, 10000, 1)
# 0.31

# high powered significant (large original effect)
# ================================================
meantheory = 13.3
# t(72) = 2.70 -> SEM = 13.3/2.70 = 4.93
sdtheory = 4.93
dftheory = 72
meanobtained = 3.00
# t(119) = 2.00 -> SEM = 3 / 2 = 1.5
semobtained = 1.5
dfobtained = 119
#  Bt(meantheory, sdtheory, dftheory), L = (meanobtained, semobtained, dfobtained) 
BF_t(13.3, 4.9, 72, 3, 1.5, 119)
# 0.29

# high powered significant (small original effect)
# ================================================
meantheory = 4.00
# t(72) = 2.70 -> SEM = 4/2.70 = 1.48
sdtheory = 1.48
dftheory = 72
meanobtained = 3.00
# t(119) = 2.00 -> SEM = 3 / 2 = 1.5
semobtained = 1.5
dfobtained = 119
#  Bt(meantheory, sdtheory, dftheory), L = (meanobtained, semobtained, dfobtained) 
BF_t(4, 1.48, 72, 3, 1.5, 119)
# BF = 4.60