TOSTtwo.raw.nm<-function(m1,m2,sd1,sd2,n1,n2,low_eqbound, high_eqbound, alpha, var.equal, prior_dist, effect_prior, se_prior, df_prior)
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
  plot(NA, ylim=c(0,1), xlim=c(min(LL90,low_eqbound)-max(UL90-LL90, high_eqbound-low_eqbound)/10, max(UL90,high_eqbound)+max(UL90-LL90, high_eqbound-low_eqbound)/10), bty="l", yaxt="n", ylab="",xlab="Mean Difference")
  points(x=dif, y=0.5, pch=15, cex=2)
  abline(v=high_eqbound, lty=2)
  abline(v=low_eqbound, lty=2)
  abline(v=0, lty=2, col="grey")
  segments(LL90,0.5,UL90,0.5, lwd=3)
  segments(LL95,0.5,UL95,0.5, lwd=1)
  title(main=paste("Equivalence bounds ",round(low_eqbound,digits=3)," and ",round(high_eqbound,digits=3),"\nMean difference = ",round(dif,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome,sep=""), cex.main=1)
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
  if(missing(effect_prior)) {
    bayes<-FALSE #if no prior effect size is provided, BF not calculated
  }
  if(bayes==TRUE) {
    if(prior_dist=="normal") {
      if(missing(se_prior)) {
        se_prior<-effect_prior/2 #if not specified otherwise, default SE is effect/2
      }
    }
    if(prior_dist=="halfnormal") {
      if(missing(se_prior)) {
        se_prior<-effect_prior #if not specified otherwise, default SE is effect
        effect_prior<-0 #halfnormal is centered on 0
      } }
        if(prior_dist=="cauchy") {
          df_prior<-1
          {
          if(missing(se_prior)) 
          {df_prior<-1
            se_prior<-effect_prior/2} #if not specified otherwise, default SE is effect
          }}
          if(prior_dist=="halfcauchy") {
            {df_prior<-1}
             if(missing(se_prior)) {
              df_prior<-1
              se_prior<-effect_prior #if not specified otherwise, default SE is effect
              effect_prior<-0 #halfcauchy is centered on 0
             }}
      
  if(missing(df_prior)) {
    df_prior<-1000 #if not specified otherwise, default df = 100000 (practically normal)
  }
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
  dist_theta_alt = dist_theta/sum(dist_theta)
  likelihood <- dt((abs(dif)-theta)/(abs(dif)/abs(t)), df = degree_f) #use abs(dif) - can be set to d
  likelihood_alt = likelihood/sum(likelihood)
  height <- dist_theta * likelihood
  area <- sum(height * incr)
  normarea <- sum(dist_theta * incr)
  height_alt = dist_theta_alt * likelihood_alt
  height_alt = height_alt/sum(height_alt)
  LikelihoodTheory <- area/normarea
  LikelihoodNull <- dt(abs(dif)/(abs(dif)/abs(t)), df = degree_f)
  BayesFactor <- round(LikelihoodTheory / LikelihoodNull, 2)
  bayes_results <- data.frame(BayesFactor, LikelihoodTheory, LikelihoodNull)
  colnames(bayes_results) <- c("Bayes Factor","Likelihood (alternative)","Likelihood (null)")
  cat("Bayes Results:\n")
  print(bayes_results)
  cat("\n")
  invisible(list(TOST_t1=t1,TOST_p1=p1,TOST_t2=t2,TOST_p2=p2, TOST_df=degree_f,alpha=alpha,low_eqbound=low_eqbound,high_eqbound=high_eqbound,low_eqbound=low_eqbound,high_eqbound=high_eqbound, LL_CI_TOST=LL90,UL_CI_TOST=UL90,bf=BayesFactor, ll_theory=LikelihoodTheory, ll_null=LikelihoodNull))
  
  }}
  





##### Correct Understanding of TOST?
TOSTtwo.raw.neil(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0.5) # B = 0.31
# Nonsignificantly different from zero t=0.23, p=0.82, and is equivalent t=3.50, p=.0002.
# B = 0.3

TOSTtwo.raw.neil(m1=5.25,m2=4.75,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0.5) # B = 0.31
# Significantly different from zero t=3.81, p<.001, and is not equivalent t=0.53, p=.70
# B = 354.6

TOSTtwo.raw.neil(m1=5.25,m2=5.05,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0.5) # B = 0.31
# Not significantly different from zero t=1.52, p<.001, and was equivalent t=-1.75, p=.04
# B = 1.4





# Calculate SE for original study
# In the future, could just run the TOST function and divide Mdiff by t.
(sqrt((((20 - 1)*(0.59^2)) + (20 - 1)*(0.62^2))/((20+20)-2))) * sqrt(1/20 + 1/20) # 0.1913766, SE for original study
(sqrt((((95 - 1)*(0.95^2)) + (89 - 1)*(0.83^2))/((95+89)-2))) * sqrt(1/95 + 1/89) # 0.131882, SE for replication study





## Normals
# TOST raw calculator original
TOSTtwo.raw(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0.5) # B = 0.31
TOSTtwo.raw(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.5) # B = 0.12
# Baguely & Kaye's calculator:
Bf(0.132, 0.03, 0, meanoftheory=0, sdtheory=0.5, tail=1) # 0.30   
Bf(0.132, 0.03, 0, meanoftheory=0.5, sdtheory=0.25, tail=2) # 0.12
# Wiens calculator:
BF_t(0, 0.5, 100000, 0.03, 0.132, (89+95)-2, tail=1) # 0.31
BF_t(0.5, 0.25, 100000, 0.03, 0.132, (89+95)-2, tail=2) # 0.12


## t-distributions
# TOST raw calculator original
TOSTtwo.raw(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.433, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0, se_prior=0.5, df_prior=(20+20)-2) # B = 0.30
TOSTtwo.raw(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.5, se_prior=0.191, df_prior=(20+20)-2) # B = 0.08
# Dienes & McLatchie calculator:
Bft(0.132, 0.03, (95+89)-2, meanoftheory=0, sdtheory=0.50, dftheory=(20+20)-2, tail = 1) # 0.3063493
Bft(0.132, 0.03, (95+89)-2, meanoftheory=0.50, sdtheory=0.191, dftheory=(20+20)-2, tail = 2) # 0.07860583
# Wiens calculator:
BF_t(0, 0.5, (20+20)-2, 0.03, 0.132, (89+95)-2, tail=1) # 0.30
BF_t(0.5, 0.191, 100000, 0.03, 0.132, (89+95)-2, tail=2) # 0.08


## Cauchy
# TOST raw calculator original
TOSTtwo.raw(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "halfnormal", effect_prior = 0, se_prior=0.5, df_prior=1) # B = 0.25
TOSTtwo.raw(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.5, se_prior=0.25, df_prior=1) # B = 0.12, using se_prior = 0.5/2
TOSTtwo.raw(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "normal", effect_prior = 0.5, se_prior=0.191, df_prior=1) # B = 0.10, using actual se_prior
# TOST raw calculator neil
TOSTtwo.raw.nm(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "halfcauchy", effect_prior = 0.5) # B = 0.25
TOSTtwo.raw.nm(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "cauchy", effect_prior = 0.5) # B = 0.12, using se_prior = 0.5/2 (default)
TOSTtwo.raw.nm(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.43,high_eqbound=0.43, var.equal=FALSE, prior_dist = "cauchy", effect_prior = 0.5, se_prior=0.191) # B = 0.10, using actual se_prior
# Dienes & McLatchie calculator:
Bft(0.132, 0.03, (95+89)-2, meanoftheory=0, sdtheory=0.50, dftheory=1, tail = 1) # 0.2549421
Bft(0.132, 0.03, (95+89)-2, meanoftheory=0.50, sdtheory=0.25, dftheory=1, tail = 2) # 0.1029683
# Wiens calculator:
BF_t(0, 0.5, 1, 0.03, 0.132, (95+89)-2, tail=1) # 0.25
BF_t(0.5, 0.191, 1, 0.03, 0.132, (95+89)-2, tail=2) # 0.10




