# Estimating sample size
# Instructions from: https://www.youtube.com/watch?v=10Lsm_o_GRg



## Run the Dienes & McLatchie Bayes factor calculator
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
nmax=250-2 # 248

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
