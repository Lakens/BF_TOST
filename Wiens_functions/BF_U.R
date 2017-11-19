#Example
LL = 0
UL = 10
meanobtained = 12
semobtained = 5
dfobtained = 27

BF_U(0, 10, 12, 5, 27)


BF_U<-function(LL, UL, meanobtained, semobtained, dfobtained)
# similar to BF_t (see there for more info) but the H1 is modelled as a uniform.
# LL = lower limit of uniform
# UL = upper limit of uniform
#  
# Computes the BayesFactor(H1 vs H0) with the H1 defined as a uniform distribution 
# and the likelihood defined as a t distribution.
# It also plots the Prior and Posterior (and Likelihood) and adds a pie chart.
#  
#  This is a modified version of the R script presented here:  
#  Dienes, Z., & Mclatchie, N. (2017). Four reasons to prefer Bayesian analyses
#  over significance testing. Psychonomic Bulletin & Review, 1-12. doi:
#  10.3758/s13423-017-1266-z
#
# 170601 -- Stefan Wiens
# people.su.se/~swiens/
# Thanks to Henrik Nordström, Mats Nilsson, Marco Tullio Liuzza, Anders Sand
  
# #Example
# LL = 0
# UL = 10
# meanobtained = 12
# semobtained = 5
# dfobtained = 27
# 
# BF_U(0, 10, 12, 5, 27)
# should give 6.34
#
# dfobtained = 10000
# use this to have a normal distribution as likelihood (as in Dienes online calculator)
# BF_U(0, 10, 12, 5, 10000)
# should give 7.51

{
  # Create theta (ie parameter)
  # ===========================
  theta = ((UL+LL)/2) - (2 * (UL-LL))
  tLL <- ((UL+LL)/2) - (2 * (UL-LL))
  tUL <- ((UL+LL)/2) + (2 * (UL-LL))
  incr <- (tUL - tLL) / 4000
  theta=seq(from = theta, by = incr, length = 4001)
  # The original calculator is not centered on meantheory (because the loop starts with theta + incr)
  # ie, value at position 2001 in loop does not give the meantheory
  # theta[2001]

  # Create dist_theta (ie density of prior model)
  # =============================================
  dist_theta = numeric(4001)
  dist_theta[theta>=LL & theta<=UL] = 1

  # alternative computation with normalized vectors
  dist_theta_alt = dist_theta/sum(dist_theta)
  
  # Create likelihood
  # For each theta, compute how well it predicts the obtained mean, 
  # given the obtained SEM and the obtained dfs.
  # Note that the distribution is symmetric, it does not matter if one computes
  # meanobtained-theta or theta-meanobtained
  likelihood <- dt((meanobtained-theta)/semobtained, df = dfobtained)
  # alternative computation with normalized vectors
  likelihood_alt = likelihood/sum(likelihood)

  # Multiply prior with likelihood
  # this gives the unstandardized posterior
  height <- dist_theta * likelihood
  area <- sum(height * incr)
  # area <- sum(dist_height * incr * likelihood)
  normarea <- sum(dist_theta * incr)

  # alternative computation with normalized vectors
  height_alt = dist_theta_alt * likelihood_alt
  height_alt = height_alt/sum(height_alt)

  LikelihoodTheory <- area/normarea
  LikelihoodNull <- dt(meanobtained/semobtained, df = dfobtained)
  BayesFactor <- round(LikelihoodTheory / LikelihoodNull, 2)

  
  # ####
  # Plot
  # ####
  # create a new window
  plotscale = 0.7
  dev.new(width = 16 * plotscale, height = 9 * plotscale, noRStudioGD = T)
  
  # define title
  mytitle = paste0("BF for U(LL = ",LL,", UL = ", UL,
               "), L = (",round(meanobtained, 2),", ",round(semobtained, 2),", ", dfobtained,  
               ")\nBF10 = ", format(BayesFactor, digits = 2, nsmall = 2), ", BF01 = ", format(1/BayesFactor, digits = 2, nsmall = 2))
  
  mylegend = "R"   # <---- define legend on right ("R") or left
  # ===========================================================
  
  mypie = T  # <---- include pie chart, T or F
  # ==========================================
  if (mypie == T) {
    layout(cbind(1,2), widths = c(4,1))
  }
  
  # for many x values, the ys are very small.
  # define minimum y threshold that is plotted, in percent of the Y maximum in the whole plot.
  # Example: 1 means that only x values are plotted in which the y values are above 1% of the maximum of Y in the whole plot.
  myminY = 1
  # ====================================================
  
  # rescale prior and posterior to sum = 1 (density)
  dist_theta_alt = dist_theta_alt / (sum(dist_theta_alt)*incr)
  height_alt = height_alt/ (sum(height_alt)*incr)

  # rescale likelood to maximum = 1
  likelihood_alt = likelihood_alt / max(likelihood_alt)

  data = cbind(dist_theta_alt, height_alt)
  maxy = max(data)
  max_per_x = apply(data,1,max)
  max_x_keep = max_per_x/maxy*100 > myminY  # threshold (1%) here
  x_keep = which(max_x_keep==1)
  #plot(theta,max_x_keep)
  if (mylegend == "R") {  # right
    legend_coor = theta[tail(x_keep,1)-20]
    legend_adj = 1
    } else { # left
    legend_coor = theta[head(x_keep,1)+20]
    legend_adj = 0
    }
  
  plot(theta, dist_theta_alt, type = "l", 
       ylim = c(0, maxy),
       xlim = c(theta[head(x_keep,1)], theta[tail(x_keep,1)]),  # change X limits here
       ylab = "Density (for Prior and Posterior)", xlab = "Theta", col = "blue", lwd = 7, lty = 5)
  lines(theta, height_alt, type = "l", col = "red", lwd = 7, lty = 5)
  text(legend_coor,maxy-(maxy/10*1), "Prior (dotted)", col = "blue", adj = legend_adj, font = 2)
  text(legend_coor,maxy-(maxy/10*2), "Posterior (dashed)", col = "red", adj = legend_adj, font = 2)
  text(legend_coor,maxy-(maxy/10*3), "Likelihood", col = "black", adj = legend_adj, font = 2)
  title(mytitle)

  theta0 = which(theta == min(theta[theta>0]))
  cat("Theta is sampled discretely (and thus, zero may be missed).\n",
      "BF10 at theta =", theta[theta0], " is ", format(1/(height_alt[theta0]/dist_theta_alt[theta0]), digits = 2, nsmall = 2),"\n\n")
  if (LL <= 0 & UL >= 0) { # Plot dots only if zero is included in prior
    points(theta[theta0], dist_theta_alt[theta0], pch = 19, col = "blue", cex = 3)
    points(theta[theta0], height_alt[theta0], pch = 19, col = "red", cex = 3)
    abline(v = theta[theta0], lwd = 2, lty = 3)}
  
  par(new = T)
  plot(theta, likelihood_alt, type = "l", 
       ylim = c(0, 1),
       xlim = c(theta[head(x_keep,1)], theta[tail(x_keep,1)]),  # change X limits here
       col = "black", lwd = 5, lty = 3, axes = F, xlab = NA, ylab = NA)
  axis(side = 4)
  mtext(side = 4, line = 3, 'Likelihood')
  
  if (mypie == T) {
    # Pie chart of BF
    rotpie = BayesFactor/(BayesFactor+1)/2
    pie(c(BayesFactor, 1), labels = NA, col = c("red", "white"), init.angle = 90 - rotpie*360, clockwise = F)
    legend("top", c("data|H1", "data|H0"), fill = c("red", "white"), bty = "n")
    cat("Results:\nBF10 = ", format(BayesFactor, digits = 2, nsmall = 2), "\nBF01 = ", format(1/BayesFactor, digits = 2, nsmall = 2), "\n\n")}
  
  return(BayesFactor)
  # return(c(BayesFactor, LikelihoodTheory, LikelihoodNull))

}

