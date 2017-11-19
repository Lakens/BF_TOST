m1=5.25
m2=5.22
sd1=0.95
sd2=0.83
n1=95
n2=89
alpha=0.05
low_eqbound=-0.43
high_eqbound=0.43
var.equal=FALSE
prior_dist = "halfnormal"
effect_prior = 0.43


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

ptost<-max(p1,p2) #Get highest p-value for summary TOST result
ttost<-ifelse(abs(t1) < abs(t2), t1, t2) #Get lowest t-value for summary TOST result
dif<-(m1-m2)
testoutcome<-ifelse(pttest<alpha,"significant","non-significant")
TOSToutcome<-ifelse(ptost<alpha,"significant","non-significant")



se_prior<-effect_prior #if not specified otherwise, default SE is effect
effect_prior<-0 #halfnormal is centered on 0

df_prior<-1000 #if not specified otherwise, default df = 100000 (practically normal)

theta <- effect_prior - 10 * se_prior
incr <- se_prior / 200
theta=seq(from = effect_prior - 10 * se_prior, by = incr, length = 4001)
dist_theta <- dt(x = (theta-effect_prior)/se_prior, df=df_prior)
dist_theta[theta <= 0] = 0
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
par(bg = "aliceblue")
#png(file=paste("Fig1.png",sep=""),width=2300,height=1500, units = "px", res = 300)
#par(mar=c(5, 5, 5, 5))
plot(NA, ylim=c(0,maxy), xlim=c(min(LL90,low_eqbound)-max(UL90-LL90, high_eqbound-low_eqbound)/5, max(UL90,high_eqbound)+max(UL90-LL90, high_eqbound-low_eqbound)/5), bty="l", yaxt="n", ylab="",xlab="Mean Difference")
points(x=dif, y=maxy/2, pch=15, cex=2)
abline(v=high_eqbound, lty=2)
abline(v=low_eqbound, lty=2)
abline(v=0, lty=2, col="grey")
segments(LL90,maxy/2,UL90,maxy/2, lwd=3)
segments(LL95,maxy/2,UL95,maxy/2, lwd=1)
title(main=paste("Equivalence bounds ",round(low_eqbound,digits=3)," and ",round(high_eqbound,digits=3),"\nMean difference = ",round(dif,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome,"\n Bayes Factor = ", BayesFactor, sep=""), cex.main=1)

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
#dev.off()
