TOSTtwo.prop.bf <- function(prop1, prop2, n1, n2, low_eqbound, high_eqbound, alpha, plot = TRUE) {
  if(missing(alpha)) {
    alpha <- 0.05
  }
  prop_dif <- prop1 - prop2
  prop_se <- sqrt((prop1*(1-prop1))/n1 + (prop2*(1-prop2))/n2)

  #calculating z-statistic
  z1 <- (prop_dif - low_eqbound)/prop_se
  z2 <- (prop_dif - high_eqbound)/prop_se
  z  <- prop_dif / prop_se
  ztest <- 1 - pnorm(abs(z))

  #calculating p-value for both one-sided tests
  p1 <- 1 - pnorm(z1)
  p2 <- pnorm(z2)
  ptost <- max(p1,p2) #Get highest p-value for summary TOST result
  ztost <- ifelse(abs(z1) < abs(z2), z1, z2) #Get lowest z-value for summary TOST result
  TOSToutcome <- ifelse(ptost<alpha,"significant","non-significant")
  ZTESToutcome <- ifelse(ztest<(alpha/2), "significant","non-significant")

  #calculating CIs
  CI_lb <- prop_dif - (qnorm(1-alpha) * prop_se)
  CI_ub <- prop_dif + (qnorm(1-alpha) * prop_se)
  CI_lb95 <- prop_dif - (qnorm(1-(alpha/2)) * prop_se)
  CI_ub95 <- prop_dif + (qnorm(1-(alpha/2)) * prop_se)

  #plot results
  if (plot == TRUE) {
  plot(NA, ylim=c(0,1), xlim=c(min(CI_lb,(low_eqbound))-max(CI_ub-CI_lb, high_eqbound-(low_eqbound))/10, max(CI_ub,high_eqbound)+max(CI_ub-CI_lb, high_eqbound-(low_eqbound))/10), bty="l", yaxt="n", ylab="",xlab="Proportion Difference")
  points(x=prop_dif, y=0.5, pch=15, cex=2)
  abline(v=high_eqbound, lty=2)
  abline(v=low_eqbound, lty=2)
  abline(v=0, lty=2, col="grey")
  segments(CI_lb,0.5,CI_ub,0.5, lwd=3)
  segments(CI_lb95,0.5,CI_ub95,0.5, lwd=1)
  title(main=paste("Equivalence bounds ",round(low_eqbound,digits=3)," and ",round(high_eqbound,digits=3),"\nProportion Difference = ",round(prop_dif,3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(CI_lb,digits=3),";",round(CI_ub,digits=3),"] ", TOSToutcome, " \n NHST: ", 100*(1-alpha),"% CI [",round(CI_lb95,digits=3),";",round(CI_ub95,digits=3),"] ", ZTESToutcome, sep=""), cex.main=1)
  }

  # Print TOST and t-test results in message form
  message(cat("Using alpha = ", alpha," Fishers exact z-test was ", ZTESToutcome,", z = ", z,", p = ", ztest * 2, sep=""))
  cat("\n")
  message(cat("Using alpha = ",alpha," the equivalence test based on Fishers exact z-test was ",TOSToutcome,", z = ",ztost,", p = ",ptost,sep=""))
  cat("\n")

  # Print TOST and t-test results in table form
  TOSTresults<-data.frame(z1,p1,z2,p2)
  colnames(TOSTresults) <- c("z-value 1","p-value 1","z-value 2","p-value 2")
  bound_results<-data.frame(low_eqbound,high_eqbound)
  colnames(bound_results) <- c("low bound","high bound")
  CIresults<-data.frame(CI_lb,CI_ub)
  colnames(CIresults) <- c(paste("Lower Limit ",100*(1-alpha*2),"% CI",sep=""),paste("Upper Limit ",100*(1-alpha*2),"% CI",sep=""))
  cat("TOST results:\n")
  print(TOSTresults)
  cat("\n")
  cat("Equivalence bounds:\n")
  print(bound_results)
  cat("\n")
  cat("TOST confidence interval:\n")
  print(CIresults)

  # Print TOST and t-test results in table form
  invisible(list(dif=prop_dif,TOST_z1=z1,TOST_p1=p1,TOST_z2=z2,TOST_p2=p2, alpha=alpha,low_eqbound=low_eqbound,high_eqbound=high_eqbound, LL_CI_TOST=CI_lb,UL_CI_TOST=CI_ub, LL_CI_ZTEST=CI_lb95,UL_CI_ZTEST=CI_ub95, TOST_outcome = TOSToutcome, NHST_outcome = ZTESToutcome, NHST_z = z, NHST_p = (ztest * 2)))
}
