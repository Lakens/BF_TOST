TOSTtwo.raw.bf<-function(m1,m2,sd1,sd2,n1,n2,low_eqbound, high_eqbound, alpha, var.equal, prior_dist, effect_prior, se_prior, df_prior, uniform_lower_bound, uniform_upper_bound)
  {
  if(missing(alpha)) {
    alpha<-0.05
  }
  if(missing(var.equal)) {
    var.equal<-FALSE
  }
  if(var.equal==TRUE) {
    sdpooled<-sqrt((((n1 - 1)*(sd1^2)) + (n2 - 1)*(sd2^2))/((n1+n2)-2)) #calculate sd pooled
    t1<-((m1-m2)-low_eqbound)/(sdpooled*sqrt(1/n1 + 1/n2))
    degree_f<-n1+n2-2
    p1<-pt(t1, degree_f, lower.tail=FALSE)
    t2<-((m1-m2)-high_eqbound)/(sdpooled*sqrt(1/n1 + 1/n2))
    p2<-pt(t2, degree_f, lower.tail=TRUE)
    LL90<-(m1-m2)-qt(1-alpha, degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
    UL90<-(m1-m2)+qt(1-alpha, degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
    LL95<-(m1-m2)-qt(1-(alpha/2), degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
    UL95<-(m1-m2)+qt(1-(alpha/2), degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
    t<-(m1-m2)/(sdpooled*sqrt(1/n1 + 1/n2))
    pttest<-2*pt(-abs(t), df=degree_f)
  } else {
    sdpooled<-sqrt((sd1^2 + sd2^2)/2) #calculate sd root mean squared for Welch's t-test
    t1<-((m1-m2)-low_eqbound)/sqrt(sd1^2/n1 + sd2^2/n2) #welch's t-test lower bound
    degree_f<-(sd1^2/n1+sd2^2/n2)^2/(((sd1^2/n1)^2/(n1-1))+((sd2^2/n2)^2/(n2-1))) #degrees of freedom for Welch's t-test
    p1<-pt(t1, degree_f, lower.tail=FALSE) #p-value for Welch's TOST t-test
    t2<-((m1-m2)-high_eqbound)/sqrt(sd1^2/n1 + sd2^2/n2) #welch's t-test upper bound
    p2<-pt(t2, degree_f, lower.tail=TRUE) #p-value for Welch's TOST t-test
    t<-(m1-m2)/sqrt(sd1^2/n1 + sd2^2/n2) #welch's t-test NHST
    pttest<-2*pt(-abs(t), df=degree_f) #p-value for Welch's t-test
    LL90<-(m1-m2)-qt(1-alpha, degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Lower limit for CI Welch's t-test
    UL90<-(m1-m2)+qt(1-alpha, degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Upper limit for CI Welch's t-test
    LL95<-(m1-m2)-qt(1-(alpha/2), degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Lower limit for CI Welch's t-test
    UL95<-(m1-m2)+qt(1-(alpha/2), degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Upper limit for CI Welch's t-test
  }
  ptost<-max(p1,p2) #Get highest p-value for summary TOST result
  ttost<-ifelse(abs(t1) < abs(t2), t1, t2) #Get lowest t-value for summary TOST result
  dif<-(m1-m2)
  testoutcome<-ifelse(pttest<alpha,"significant","non-significant")
  TOSToutcome<-ifelse(ptost<alpha,"significant","non-significant")
  if(var.equal == TRUE) {
    message(cat("Using alpha = ",alpha," Student's t-test was ",testoutcome,", t(",degree_f,") = ",t,", p = ",pttest,sep=""))
    cat("\n")
    message(cat("Using alpha = ",alpha," the equivalence test based on Student's t-test was ",TOSToutcome,", t(",degree_f,") = ",ttost,", p = ",ptost,sep=""))
  } else {
    message(cat("Using alpha = ",alpha," Welch's t-test was ",testoutcome,", t(",degree_f,") = ",t,", p = ",pttest,sep=""))
    cat("\n")
    message(cat("Using alpha = ",alpha," the equivalence test based on Welch's t-test  was ",TOSToutcome,", t(",degree_f,") = ",ttost,", p = ",ptost,sep=""))
  }
  TOSTresults<-data.frame(t1,p1,t2,p2,degree_f)
  colnames(TOSTresults) <- c("t-value 1","p-value 1","t-value 2","p-value 2","df")
  bound_results<-data.frame(low_eqbound,high_eqbound)
  colnames(bound_results) <- c("low bound raw","high bound raw")
  CIresults<-data.frame(LL90,UL90)
  colnames(CIresults) <- c(paste("Lower Limit ",100*(1-alpha*2),"% CI raw",sep=""),paste("Upper Limit ",100*(1-alpha*2),"% CI raw",sep=""))
  cat("TOST results:\n")
  print(TOSTresults)
  cat("\n")
  cat("Equivalence bounds (raw scores):\n")
  print(bound_results)
  cat("\n")
  cat("TOST confidence interval:\n")
  print(CIresults)
  #below added BF calc
  bayes<-TRUE #expect to provide bayes
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
    likelihood <- dt((dif-theta)/(dif/t), df = degree_f) #use dif - can be set to d Create likelihood, for each theta, compute how well it predicts the obtained mean, given the obtained SEM and the obtained dfs.
    likelihood_alt = likelihood/sum(likelihood) # alternative computation with normalized vectors
    height <- dist_theta * likelihood # Multiply prior with likelihood, this gives the unstandardized posterior
    area <- sum(height * incr)
    normarea <- sum(dist_theta * incr)
    height_alt = dist_theta_alt * likelihood_alt
    height_alt = height_alt/sum(height_alt)
    LikelihoodTheory <- area/normarea
    LikelihoodNull <- dt(dif/(dif/t), df = degree_f)
    BayesFactor <- round(LikelihoodTheory / LikelihoodNull, 2)
    bayes_results <- data.frame(BayesFactor, LikelihoodTheory, LikelihoodNull)
    colnames(bayes_results) <- c("Bayes Factor","Likelihood (alternative)","Likelihood (null)")
    cat("Bayes Results:\n")
    print(bayes_results)
    cat("\n")
    invisible(list(TOST_t1=t1,TOST_p1=p1,TOST_t2=t2,TOST_p2=p2, TOST_df=degree_f,alpha=alpha,low_eqbound=low_eqbound,high_eqbound=high_eqbound,low_eqbound=low_eqbound,high_eqbound=high_eqbound, LL_CI_TOST=LL90,UL_CI_TOST=UL90,bf=BayesFactor, ll_theory=LikelihoodTheory, ll_null=LikelihoodNull))
    #plot (adapted from Wienes by DL)
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
    plot(NA, ylim=c(0,maxy), xlim=c(min(LL90,low_eqbound)-max(UL90-LL90, high_eqbound-low_eqbound)/5, max(UL90,high_eqbound)+max(UL90-LL90, high_eqbound-low_eqbound)/5), bty="l", yaxt="n", ylab="",xlab="Mean Difference")
    points(x=dif, y=maxy/2, pch=15, cex=2)
    abline(v=high_eqbound, lty=2)
    abline(v=low_eqbound, lty=2)
    abline(v=0, lty=2, col="grey")
    segments(LL90,maxy/2,UL90,maxy/2, lwd=3)
    segments(LL95,maxy/2,UL95,maxy/2, lwd=1)
    if(bayes==FALSE) {
      title(main=paste("Equivalence bounds ",round(low_eqbound,digits=3)," and ",round(high_eqbound,digits=3),"\nMean difference = ",round(dif,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome, sep=""), cex.main=1)
    }
    if(bayes==TRUE){
      par(new=TRUE)
      plot(theta, dist_theta_alt, type = "l",
           ylim = c(0, maxy),
           xlim=c(min(LL90,low_eqbound)-max(UL90-LL90, high_eqbound-low_eqbound)/5, max(UL90,high_eqbound)+max(UL90-LL90, high_eqbound-low_eqbound)/5),
           ylab = "Density (for Prior and Posterior)", xlab = "", col = "grey46", lwd = 2, lty = 2)
      lines(theta, height_alt, type = "l", col = "black", lwd = 3, lty = 1)
      theta0 = which(theta == min(theta[theta>0]))
      points(theta[theta0],dist_theta_alt[theta0], pch = 19, col = "grey46", cex = 1.5)
      points(theta[theta0],height_alt[theta0], pch = 19, col = "black", cex = 1.5)
      par(new = T)
      plot(theta, likelihood_alt, type = "l",
           ylim = c(0, 1),
           xlim=c(min(LL90,low_eqbound)-max(UL90-LL90, high_eqbound-low_eqbound)/5, max(UL90,high_eqbound)+max(UL90-LL90, high_eqbound-low_eqbound)/5),     col = "dodgerblue", lwd = 2, lty = 3, axes = F, xlab = NA, ylab = NA)
      axis(side = 4)
      mtext(side = 4, line = 3, 'Likelihood')
      abline(v = theta[theta0], lwd = 2, lty = 3)
      if(bayes==TRUE){
        title(main=paste("Bounds ",round(low_eqbound,digits=3)," and ",round(high_eqbound,digits=3),", Mean difference = ",round(dif,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome,"\n Bayes Factor = ", BayesFactor, sep=""), cex.main=1)
      }
      #dev.off()
    }
  }
}

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
# Wienes calculator:
BF_t(0, 0.5, 100000, 0.03, 0.132, (89+95)-2, tail=1) # 0.31
BF_t(0.5, 0.25, 100000, 0.03, 0.132, (89+95)-2, tail=2) # 0.12


## t-distributions
# TOST raw calculator original
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.433, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0, se_prior=0.5, df_prior=(20+20)-2) # B = 0.30
TOSTtwo.raw.bf(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.4,high_eqbound=0.4, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.5, se_prior=0.191, df_prior=(20+20)-2) # B = 0.08
# Dienes & McLatchie calculator:
Bft(0.132, 0.03, (95+89)-2, meanoftheory=0, sdtheory=0.50, dftheory=(20+20)-2, tail = 1) # 0.3063493
Bft(0.132, 0.03, (95+89)-2, meanoftheory=0.50, sdtheory=0.191, dftheory=(20+20)-2, tail = 2) # 0.07860583
# Wienes calculator:
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
# Wienes calculator:
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



