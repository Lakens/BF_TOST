## Added Bayes factors and Daniels graphs to TOSTr

## Problem: 
## The Bs calculated across the TOSTr, Wienes & Baguley&Kay calculators appear identical for smaller effect (see lines: 189-247)
## But the Bs from the TOSTr differ greatly from the other two as the data differences get bigger (see lines 257-306)


TOSTr.bf<-function(n, r, low_eqbound_r, high_eqbound_r, alpha, plot = TRUE, prior_dist, effect_prior, se_prior, df_prior, uniform_lower_bound, uniform_upper_bound){
  if(missing(alpha)) {
    alpha <- 0.05
  }
  
  # Calculate TOST, t-test, 90% CIs and 95% CIs
  z1<-((log((1+abs(r))/(1-abs(r)))/2)-(log((1+low_eqbound_r)/(1-low_eqbound_r))/2))/(sqrt(1/(n-3)))
  z2<-((log((1+abs(r))/(1-abs(r)))/2)-(log((1+high_eqbound_r)/(1-high_eqbound_r))/2))/(sqrt(1/(n-3)))
  p1<-ifelse(low_eqbound_r<r,pnorm(-abs(z1)),1-pnorm(-abs(z1)))
  p2<-ifelse(high_eqbound_r>r,pnorm(-abs(z2)),1-pnorm(-abs(z2)))
  ptost<-max(p1,p2)
  pttest<-2*(1-pt(abs(r)*sqrt(n-2)/sqrt(1-abs(r)^2),n-2))
  zLL90<-(log((1+r)/(1-r))/2)-qnorm(1-alpha)*sqrt(1/(n-3))
  zUL90<-(log((1+r)/(1-r))/2)+qnorm(1-alpha)*sqrt(1/(n-3))
  LL90<-(exp(1)^(2*zLL90)-1)/(exp(1)^(2*zLL90)+1)
  UL90<-(exp(1)^(2*zUL90)-1)/(exp(1)^(2*zUL90)+1)
  zLL95<-(log((1+r)/(1-r))/2)-qnorm(1-(alpha/2))*sqrt(1/(n-3))
  zUL95<-(log((1+r)/(1-r))/2)+qnorm(1-(alpha/2))*sqrt(1/(n-3))
  LL95<-(exp(1)^(2*zLL95)-1)/(exp(1)^(2*zLL95)+1)
  UL95<-(exp(1)^(2*zUL95)-1)/(exp(1)^(2*zUL95)+1)
  testoutcome<-ifelse(pttest<alpha,"significant","non-significant")
  TOSToutcome<-ifelse(ptost<alpha,"significant","non-significant")
  
  # Plot results
  if (plot == TRUE) {
    plot(NA, ylim=c(0,1), xlim=c(min(LL90,low_eqbound_r)-max(UL90-LL90, high_eqbound_r-low_eqbound_r)/10, max(UL90,high_eqbound_r)+max(UL90-LL90, high_eqbound_r-low_eqbound_r)/10), bty="l", yaxt="n", ylab="",xlab="Correlation")
    points(x=r, y=0.5, pch=15, cex=2)
    abline(v=high_eqbound_r, lty=2)
    abline(v=low_eqbound_r, lty=2)
    abline(v=0, lty=2, col="grey")
    segments(LL90,0.5,UL90,0.5, lwd=3)
    segments(LL95,0.5,UL95,0.5, lwd=1)
    title(main=paste("Equivalence bounds ",round(low_eqbound_r,digits=3)," and ",round(high_eqbound_r,digits=3),"\nr = ",round(r,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome,sep=""), cex.main=1)
  }
  
  # Print TOST and t-test results in message form
  message(cat("Using alpha = ",alpha," the NHST t-test was ",testoutcome,", p = ",pttest,sep=""))
  cat("\n")
  message(cat("Using alpha = ",alpha, " the equivalence test was ",TOSToutcome,", p = ",ptost,sep=""))
  
  # Print TOST and t-test results in table form
  TOSTresults<-data.frame(p1,p2)
  colnames(TOSTresults) <- c("p-value 1","p-value 2")
  bound_r_results<-data.frame(low_eqbound_r,high_eqbound_r)
  colnames(bound_r_results) <- c("low bound r","high bound r")
  CIresults<-data.frame(LL90,UL90)
  colnames(CIresults) <- c(paste("Lower Limit ",100*(1-alpha*2),"% CI raw",sep=""),paste("Upper Limit ",100*(1-alpha*2),"% CI raw",sep=""))
  cat("TOST results:\n")
  print(TOSTresults)
  cat("\n")
  cat("Equivalence bounds (r):\n")
  print(bound_r_results)
  cat("\n")
  cat("TOST confidence interval:\n")
  print(CIresults)
  
  #below added BF calc
  bayes<-TRUE #expect to provide bayes
  effect_prior<-  0.5*log((1 + effect_prior)/(1 - effect_prior)) #transform the effect_prior from r to Fisher's z
  r_fisher <- 0.5*log((1 + r)/(1 - r)) #transform the obtained r to Fisher's z
  df=n-2
  sem_fisher <- 1/sqrt(df-1)
  if(missing(prior_dist)) {
    bayes<-FALSE #if no prior effect size is provided, BF not calculated
  }
  if(bayes==TRUE){
    if(prior_dist=="normal"){
      if(missing(se_prior)){
        se_prior<-effect_prior/2 #if not specified otherwise, default SE is effect/2
      }
    }
    if(prior_dist=="halfnormal"){
      if(missing(se_prior)){
        se_prior<-effect_prior #if not specified otherwise, default SE is effect
        effect_prior<-0 #halfnormal is centered on 0
      } }
    if(prior_dist=="cauchy"){
      df_prior<-1
      if(missing(se_prior)){
        df_prior<-1
        se_prior<-effect_prior/2} #if not specified otherwise, default SE is effect
    }
    if(prior_dist=="halfcauchy"){
      df_prior<-1
      if(missing(se_prior)){
        df_prior<-1
        se_prior<-effect_prior #if not specified otherwise, default SE is effect
        effect_prior<-0 #halfcauchy is centered on 0
      }
    }
    if(missing(df_prior)){
      df_prior<-1000 #if not specified otherwise, default df = 100000 (practically normal)
    }
    if(prior_dist=="uniform"){
      theta = ((uniform_upper_bound + uniform_lower_bound)/2) - (2 * (uniform_upper_bound - uniform_lower_bound))
      tLL <- ((uniform_upper_bound + uniform_lower_bound)/2) - (2 * (uniform_upper_bound - uniform_lower_bound))
      tUL <- ((uniform_upper_bound + uniform_lower_bound)/2) + (2 * (uniform_upper_bound - uniform_lower_bound))
      incr <- (tUL - tLL) / 4000
      theta=seq(from = theta, by = incr, length = 4001)
      dist_theta = numeric(4001)
      dist_theta[theta >= uniform_lower_bound & theta <= uniform_upper_bound] = 1
    } else {
      theta <- effect_prior - 10 * se_prior
      incr <- se_prior / 200
      theta=seq(from = effect_prior - 10 * se_prior, by = incr, length = 4001)
      dist_theta <- dt(x = (theta-effect_prior)/se_prior, df=df_prior)
      if(prior_dist=="halfnormal"){
        dist_theta[theta <= 0] = 0
      }
      if(prior_dist=="halfcauchy"){
        dist_theta[theta <= 0] = 0
      }
    }
    dist_theta_alt = dist_theta/sum(dist_theta)
    likelihood <- dt((r_fisher-theta)/(sem_fisher), df = df) 
    likelihood_alt = likelihood/sum(likelihood) # alternative computation with normalized vectors
    height <- dist_theta * likelihood # Multiply prior with likelihood, this gives the unstandardized posterior
    area <- sum(height * incr)
    normarea <- sum(dist_theta * incr)
    height_alt = dist_theta_alt * likelihood_alt
    height_alt = height_alt/sum(height_alt)
    LikelihoodTheory <- area/normarea
    LikelihoodNull <- dt(r_fisher/(sem_fisher), df = df)
    BayesFactor <- round(LikelihoodTheory / LikelihoodNull, 6)
    bayes_results <- data.frame(BayesFactor, LikelihoodTheory, LikelihoodNull)
    colnames(bayes_results) <- c("Bayes Factor","Likelihood (alternative)","Likelihood (null)")
    cat("Bayes Results:\n")
    print(bayes_results)
    cat("\n")
    # Print TOST and t-test results in table form
    invisible(list(r=r,TOST_p1=p1,TOST_p2=p2,alpha=alpha,low_eqbound_r=low_eqbound_r,high_eqbound_r=high_eqbound_r, LL_CI_TOST=LL90,UL_CI_TOST=UL90,LL_CI_TTEST=LL95, UL_CI_TTEST=UL95,bf=BayesFactor, ll_theory=LikelihoodTheory, ll_null=LikelihoodNull))
    #plot (adapted from Wiens by DL)
    myminY = 1
    # rescale prior and posterior to sum = 1 (density)
    dist_theta_alt = dist_theta_alt / (sum(dist_theta_alt)*incr)
    height_alt = height_alt/(sum(height_alt)*incr)
    # rescale likelood to maximum = 1
    likelihood_alt = likelihood_alt / max(likelihood_alt)
    data = cbind(dist_theta_alt, height_alt)
    maxy = max(data)
    max_per_x = apply(data,1,max)
    max_x_keep = max_per_x/maxy*100 > myminY  # threshold (1%) here
    x_keep = which(max_x_keep==1)
    #png(file=paste("Fig1.png",sep=""),width=2300,height=2000, units = "px", res = 300)
    plot(NA, ylim=c(0,maxy), xlim=c(min(LL90,low_eqbound_r)-max(UL90-LL90, high_eqbound_r-low_eqbound_r)/5, max(UL90,high_eqbound_r)+max(UL90-LL90, high_eqbound_r-low_eqbound_r)/5), bty="l", yaxt="n", ylab="",xlab="Mean Difference")
    points(x=r, y=maxy/2, pch=15, cex=2)
    abline(v=high_eqbound_r, lty=2)
    abline(v=low_eqbound_r, lty=2)
    abline(v=0, lty=2, col="grey")
    segments(LL90,maxy/2,UL90,maxy/2, lwd=3)
    segments(LL95,maxy/2,UL95,maxy/2, lwd=1)
    if(bayes==FALSE) {
      title(main=paste("Equivalence bounds ",round(low_eqbound_r,digits=3)," and ",round(high_eqbound_r,digits=3),"\nr = ",round(r,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome, sep=""), cex.main=1)
    }
    if(bayes==TRUE){
      par(new=TRUE)
      plot(theta, dist_theta_alt, type = "l",
           ylim = c(0, maxy),
           xlim=c(min(LL90,low_eqbound_r)-max(UL90-LL90, high_eqbound_r-low_eqbound_r)/5, max(UL90,high_eqbound_r)+max(UL90-LL90, high_eqbound_r-low_eqbound_r)/5),
           ylab = "Density (for Prior and Posterior)", xlab = "", col = "grey46", lwd = 2, lty = 2)
      lines(theta, height_alt, type = "l", col = "black", lwd = 3, lty = 1)
      theta0 = which(theta == min(theta[theta>0]))
      points(theta[theta0],dist_theta_alt[theta0], pch = 19, col = "grey46", cex = 1.5)
      points(theta[theta0],height_alt[theta0], pch = 19, col = "black", cex = 1.5)
      par(new = T)
      plot(theta, likelihood_alt, type = "l",
           ylim = c(0, 1),
           xlim=c(min(LL90,low_eqbound_r)-max(UL90-LL90, high_eqbound_r-low_eqbound_r)/5, max(UL90,high_eqbound_r)+max(UL90-LL90, high_eqbound_r-low_eqbound_r)/5),     col = "dodgerblue", lwd = 2, lty = 3, axes = F, xlab = NA, ylab = NA)
      axis(side = 4)
      mtext(side = 4, line = 3, 'Likelihood')
      abline(v = theta[theta0], lwd = 2, lty = 3)
      if(bayes==TRUE){
        title(main=paste("Bounds ",round(low_eqbound_r,digits=3)," and ",round(high_eqbound_r,digits=3),", r = ",round(r,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome,"\n Bayes Factor = ", BayesFactor, sep=""), cex.main=1)
      }
      #dev.off()
    }  }  }






########### Small raw effects seem to work fine:


# Using Example 5 data from Lakens, Scheel & Isager
# Kahane reported r(229)=-0.04,p=.525,N=231
# This Bayes factor predicts an r = 0.2 (average effect in Social Psych)

# HalfNormal
TOSTr.bf(n = 231, 
         r = -0.04, 
         low_eqbound_r = -0.1830961, 
         high_eqbound_r = 0.1830961, 
         alpha = 0.05, 
         prior_dist = "halfnormal", 
         effect_prior = 0.2, 
         ) 
# B = 0.21
library(psych)
fisherz(-0.04) # -0.04002135, 
1/sqrt(229 -1) # 0.06622662, sem
BF_t(meantheory=0, sdtheory=.2, dftheory=10000, meanobtained=-0.04, semobtained=0.06622662, dfobtained=10000, tail = 1) # 0.21, matches Wiens
Bf( 0.06622662, -0.04, 0, meanoftheory=0, sdtheory=.2, tail=1) # 0.2118514, matches Baguley & Kaye


# Normal
TOSTr.bf(n = 231, 
         r = -0.04, 
         low_eqbound_r = -0.1830961, 
         high_eqbound_r = 0.1830961, 
         alpha = 0.05, 
         prior_dist = "normal", 
         effect_prior = 0.2, 
) 
# B = 0.09
library(psych)
fisherz(-0.04) # -0.04002135, 
1/sqrt(229 -1) # 0.06622662, sem
BF_t(meantheory=.2, sdtheory=.2/2, dftheory=10000, meanobtained=-0.04002135, semobtained=0.06622662, dfobtained=10000, tail = 2) # 0.09, matches Wiens
Bf( 0.06622662, -0.04002135, 0, meanoftheory=.2, sdtheory=.2/2, tail=2) # 0.08948947, matches Baguley & Kaye



# Uniform
TOSTr.bf(n = 231, 
         r = -0.04, 
         low_eqbound_r = -0.1830961, 
         high_eqbound_r = 0.1830961, 
         alpha = 0.05, 
         prior_dist = "uniform", 
         effect_prior = 0.2,
         uniform_lower_bound= 0,
         uniform_upper_bound=.5
) 
# B = 0.11
library(psych)
fisherz(-0.04) # -0.04002135, 
1/sqrt(229 -1) # 0.06622662, sem
BF_U(LL=0, UL=.5, meanobtained=-0.04002135, semobtained=0.06622662, dfobtained=100000) # 0.11
Bf(0.06622662 , -0.04002135 , 1, lower=0, upper=.5) # B=0.1084745









########### Larger raw effects generate different Bs to those obtained by Wienes and Baguley&Kaye


# HalfNormal
TOSTr.bf(n = 50, 
         r = 0.5, 
         low_eqbound_r = -0.1830961, 
         high_eqbound_r = 0.1830961, 
         alpha = 0.05, 
         prior_dist = "halfnormal", 
         effect_prior = 0.2
) 
# B = 61.01
fisherz(0.5) # 0.5493061, 
1/sqrt(48 -1) # 0.145865, sem
BF_t(meantheory=0, sdtheory=.2, dftheory=10000, meanobtained=0.5493061, semobtained=0.145865, dfobtained=10000, tail = 1) # 120.28, 
Bf( 0.145865, 0.5493061, 0, meanoftheory=0, sdtheory=.2, tail=1) # 120.5213, matches Baguley & Kaye


# Normal
TOSTr.bf(n = 50, 
         r = 0.5, 
         low_eqbound_r = -0.1830961, 
         high_eqbound_r = 0.1830961, 
         alpha = 0.05, 
         prior_dist = "normal", 
         effect_prior = 0.2 
) 
# B = 71.74
fisherz(0.5) # 0.5493061, 
1/sqrt(48 -1) # 0.145865, sem
BF_t(meantheory=.2, sdtheory=.2/2, dftheory=10000, meanobtained=0.5493061, semobtained=0.145865, dfobtained=10000, tail = 2) # 
Bf( 0.145865, 0.5493061, 0, meanoftheory=.2, sdtheory=.2/2, tail=2) # 140.8387, matches Baguley & Kaye



# Uniform
TOSTr.bf(n = 50, 
         r = 0.5, 
         low_eqbound_r = -0.1830961, 
         high_eqbound_r = 0.1830961, 
         alpha = 0.05, 
         prior_dist = "uniform", 
         effect_prior = 0.2 ,
         uniform_lower_bound= 0,
         uniform_upper_bound=.5
) 
# B = 153.83
library(psych)
fisherz(-0.04) # 0.04002135
BF_U(LL=0, UL=.5, meanobtained=0.5493061, semobtained=0.145865, dfobtained=100000) # 322.91
Bf(0.145865 , 0.5493061 , 1, lower=0, upper=.5) # B=323.6547

















