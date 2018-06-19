## EXAMPLES FROM LAKENS, MCLATCHIE, ISAGER, SCHEEL & DIENES (2018)
## BAYES FACTORS USING DIENES & MCLATCHIE (2018) R SCRIPT





## Run the Bayes factor t-distribution function from Dienes and McLatchie (2018)
Bft<-function(sd, obtained, dfdata, meanoftheory, sdtheory, dftheory, tail = 2)
{
  area <- 0
  normarea <- 0
  theta <- meanoftheory - 10 * sdtheory
  incr <- sdtheory/200
  for (A in -2000:2000){
    theta <- theta + incr
    dist_theta <- dt((theta-meanoftheory)/sdtheory, df=dftheory)
    if(identical(tail, 1)){
      if (theta <= 0){
        dist_theta <- 0
      } else {
        dist_theta <- dist_theta * 2
      }
    }
    height <- dist_theta * dt((obtained-theta)/sd, df = dfdata)
    area <- area + height * incr
    normarea <- normarea + dist_theta*incr
  }
  LikelihoodTheory <- area/normarea
  Likelihoodnull <- dt(obtained/sd, df = dfdata)
  BayesFactor <- LikelihoodTheory/Likelihoodnull
  BayesFactor
}




# EXAMPLE 1 
# Martins, Sheppes, Gross and Mather (2016)

# Informantion re obtained data
0.338 - 0.321 # 0.017, obtained mean difference
0.017 / 0.35 # 0.04857143, Mdif / t = Standard Error

# Vague Bayes Factor
Bft( 0.04857143, 0.017 , 64-2, meanoftheory=0, sdtheory=0.5, dftheory=1000, tail=1) # 0.1329216
Bft( 0.04857143, 0.017 , 64-2, meanoftheory=0, sdtheory=0.19, dftheory=1000, tail=1) # 0.3342294, RR minimum
Bft( 0.04857143, 0.017 , 64-2, meanoftheory=0, sdtheory=999999999, dftheory=1000, tail=1) # 0.003982393, RR maximum (infinity)

# Informed Bayes Factor based on Scheibe, Sheppes & Staudinger (2015)
Bft( 0.04857143, -0.017 , 64-2, meanoftheory=0, sdtheory=0.08, dftheory=77-2, tail=1) # 0.4131769
Bft( 0.04857143, -0.017 , 64-2, meanoftheory=0, sdtheory=0.0000000001, dftheory=77-2, tail=1) # 1, , RR minimum
Bft( 0.04857143, -0.017 , 64-2, meanoftheory=0, sdtheory=0.10, dftheory=77-2, tail=1) # 0.3197193, RR maximum



# EXAMPLE 2
# Shega, Tiedt, Grant and Dale (2014)

# 62-69 vs 70-79: Information regarding obtained data
2.03 - 1.98 # 0.05, obtained mean difference 
0.05 / 0.4925451 # 0.1015135, Standard Error (t-test calculated from TOSTER function)
# Bayes Factor 
Bft( 0.1015135, 0.05 , 1791.71, meanoftheory=0, sdtheory=1.21, dftheory=10000, tail=2) # 0.0943194
Bft( 0.1015135, 0.05 , 1791.71, meanoftheory=0, sdtheory=0.33, dftheory=10000, tail=2) # 0.3285164, RR minimum
Bft( 0.1015135, 0.05 , 1791.71, meanoftheory=0, sdtheory=999999999, dftheory=10000, tail=2) # 0.001995296, RR maximum (infinity)

# 70-79 vs 79+: Information regarding obtained data
2.14 - 1.98 # 0.16, obtained mean difference 
0.16 / 1.108473 # 0.1443427, Standard Error (t-test calculated from TOSTER function)
# Bayes Factor 
Bft( 0.1443427, 0.16 , 657.5768, meanoftheory=0, sdtheory=1.21, dftheory=10000, tail=2) # 0.2172251
Bft( 0.1443427, 0.16 , 657.5768, meanoftheory=0, sdtheory=0.77, dftheory=10000, tail=2) # 0.3337694, RR minimum
Bft( 0.1443427, 0.16 , 657.5768, meanoftheory=0, sdtheory=999999999, dftheory=10000, tail=2) # 0.001995666, RR maximum (infinity)

# 62-69 vs 79+: Information regarding obtained data
2.14 - 2.03 # 0.11, obtained mean difference 
0.11 / 0.7255117 # 0.1516171, Standard Error (t-test calculated from TOSTER function)
# Bayes Factor 
Bft( 0.1516171, 0.11 , 779.4781, meanoftheory=0, sdtheory=1.21, dftheory=10000, tail=2) # 0.1611906
Bft( 0.1516171, 0.11 , 779.4781, meanoftheory=0, sdtheory=0.56, dftheory=10000, tail=2) # 0.3341117, RR minimum
Bft( 0.1516171, 0.11 , 779.4781, meanoftheory=0, sdtheory=999999999, dftheory=10000, tail=2) # 0.001995288, RR maximum (infinity)




# EXAMPLE 3
# Westerhof, Bohlmeijer and McAdams (2017)
# Is a r = .12 evidence if r = .14 is considered evidence elsewhere

# Information re obtained data
0.14 * 0.8/0.6 # 0.19, prior beta
0.12 * 1.0/0.6 # 0.20, obtained beta
0.114 
# Standard error of the raw slope obtained because Fisher's z transformation of .12 = .12 
# with a SE = 1/???(218-3) = .068, which means the z-test = 0.12/.068 = 1.76
# We can use this to determine that: .20/SE = 1.76, and so the SE of raw slope = 0.114

# Bayes Factor)
Bft( 0.114, 0.20 , 218-2, meanoftheory=0, sdtheory=0.19, dftheory=10000, tail=1) # 2.962831
Bft( 0.114, 0.20 , 218-2, meanoftheory=0, sdtheory=0.0000000001, dftheory=10000, tail=1) # 1, , RR minimum
Bft( 0.114, 0.20 , 218-2, meanoftheory=0, sdtheory=2.98, dftheory=10000, tail=1) # 0.3413824, RR maximum5





# EXAMPLE 4
# Spanol, Schain and Bowen (2014)

# Information re obtained data
(0.77-0.76) - (0.76-0.750001) # 1e-06
1e-06 / 5.088219e-05 # 0.01965324, SE (t-test calculated from TOSTER function)

# Bayes Factor based on the results of Experiment 1
(0.61-0.54) - (0.64-0.61) # 0.04, Experiment 1 mdif
Bft( 0.01965324, 1e-06 , 62-2, meanoftheory=0, sdtheory=0.04, dftheory=73, tail=1) # 0.438549
Bft( 0.01965324, 1e-06 , 62-2, meanoftheory=0, sdtheory=0.0000000001, dftheory=73, tail=1) # 1, , RR minimum
Bft( 0.01965324, 1e-06 , 62-2, meanoftheory=0, sdtheory=0.05, dftheory=73, tail=1) # 0.3638204, RR maximum5










## ESTIMATING SAMPLE SIZE
## Instructional available at: https://www.youtube.com/watch?v=10Lsm_o_GRg


# Example data from Gerontology Article:
# Bunce, Batterham & Mackinnon (2018)
# reported that non-frail adults (M=11.53, SD=3.52, N=304) recalled significantly more names of animals than frail adults (M=10.11, SD=3.20, N=154), 
# t(456)=4.20, p<.001

# Need: SE from a previous study 
bunce.se <- (11.53-10.11) / 4.20 # 0.3380952, calculated from Mdif / t

# Need: N from previous study 
bunce.n <- 304+154 # 458

# Predicted effect size for H1 
bunce.mdif <- 11.53-10.11 # 1.42
obtain.h1 <- bunce.mdif

# Predicted effect size for H0 (here = 0)
obtain.h0 <- 0


## Sample size estimation

# Number of subjects we are willing to test
n = seq(2, 250, 1)
nmax=250-2 # 185

# standard error of the estimate based on the obtained SD
se_diff = (bunce.se)*sqrt(bunce.n/n) # the standard error varies as the square root of n, or in this case, the change in the magnitude of n

# Calculate B for different sample sizes for H0
x <- 1/Bft(se_diff , obtain.h0  , n-1, meanoftheory=0, sdtheory=bunce.mdif, dftheory=100000, tail=1)
x # 207 participants needed, B = 3.005284

# Calculate B for different sample sizes for H1
y <- Bft(se_diff , obtain.h1  , n-1, meanoftheory=0, sdtheory=bunce.mdif, dftheory=100000, tail=1)
y # 82 participants needed, B =  3.025245



# plot H1 uniform

plot(x = x,
     type = "l",                            # Specify that you want to plot a line graph
     lwd = 2,                               # Thickness of line
     lty = 2,
     col = "black",                          # Colour of line
     xlim=c(0,250),                          # Set limit of x-axis
     ylim=c(0,10),                         # Set limit of y-axis
     frame.plot=TRUE,                       # Do plot the frame of the graph
     xlab="Sample Size",                    # Title for x-axis
     ylab= "Bayes Factor",                             # Title for y-axis
     axes = FALSE,                          # Don't plot the scales by default
     main=paste("Estimating Sample Size for Bayes Factors"))     # Main title
segments(0, 3, 250, 3, col= 'black', lwd=2, lty=1)          # Add a black horizontal line
lines(y[y<10 ], lwd=2, lty=3)
Axis(side=1,at=seq(0, 250, by = 10))
Axis(side=2,at=seq(0,10, by=1))
legend(0,10,legend=c("B for H0 (Mdiff = 0)","B for H1 (Mdiff = 1.42)", "Threshold for sensitive test (B = 3)"),
       text.col="black", lty = c(2,3,1))

dev.copy(png,'Estimating Sample Size Based on Bunce et al 2018.png')             # Save whatever is in displayed in the plot viewer in my wd
dev.off()







