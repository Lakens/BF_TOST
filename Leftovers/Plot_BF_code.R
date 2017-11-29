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
png(file=paste("Fig1.png",sep=""),width=2300,height=1500, units = "px", res = 300)
par(mar=c(5, 5, 5, 5))
plot(theta, dist_theta_alt, type = "l",
     ylim = c(0, maxy),
     xlim = c(theta[head(x_keep,1)], theta[tail(x_keep,1)]),  # change X limits here
     ylab = "Density (for Prior and Posterior)", xlab = "Theta", col = "grey46", lwd = 2, lty = 2)
lines(theta, height_alt, type = "l", col = "black", lwd = 3, lty = 1)
theta0 = which(theta == min(theta[theta>0]))
points(theta[theta0],dist_theta_alt[theta0], pch = 19, col = "grey46", cex = 1.5)
points(theta[theta0],height_alt[theta0], pch = 19, col = "black", cex = 1.5)
par(new = T)
plot(theta, likelihood_alt, type = "l",
     ylim = c(0, 1),
     xlim = c(theta[head(x_keep,1)], theta[tail(x_keep,1)]),  # change X limits here
     col = "dodgerblue", lwd = 2, lty = 3, axes = F, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 3, 'Likelihood')
abline(v = theta[theta0], lwd = 2, lty = 3)
dev.off()
