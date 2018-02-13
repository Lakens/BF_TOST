# Tell R to create four graphs that I'd like to appear in a 2x2 format
par(mfrow = c(2, 2))
# Note: if I wanted a different layout:
# par(mfrow = c(1,2)) # two graphs side-by-side
# par(mfrow = c(2,1)) # two graphs, one on top of the other

# plot H0
par(bg = "white")
plot(-10,                                   # Honestly, not sure what this does (would normally included data here)
     xlim=c(-6,6),                          # Set limits for x-axis
     ylim=c(0,0.40),                        # Set limits for y-axis
     ylab="probability",                    # Title for y-axis
     xlab="effect size",                    # Title for x-axis
     main=paste("Point Null Hypothesis"),   # Main title
     axes=FALSE,                            # Don't plot the scales by default
     frame.plot=TRUE)                       # Do plot the frame of the graph

segments(0, 0, 0, 2, col= 'blue', lwd=2)    # Add a blue line at x=0 to show where what the null hypothesis predicts. Note: (x-startingpoint, y-startingpoint, x-endpoint, y-endpoint)
segments(-6,0,6,0, col='blue', lwd=2)       # Add a blue line at y=0 to show that all other effects are considered impossible under H0
Axis(side=1,at=0,labels=0,lty=0)            # Adds '0' at the mid point of x-axis
Axis(side=2,at=0,labels=0,lty=0)            # Adds '0' at the mid point of y-axis
Axis(side=1, labels=FALSE)                  # Adds tags but not labels to x axis
Axis(side=2, labels=FALSE)                  # Adds tags but not labels to y axis

# plot H1 uniform
x <- seq(-6,6,length=100)                   # Create a series of 100 numbers from -6 to 6
uniform <- dunif(x,0, 3)                    # Create a normal distribution, m=0, sd=3 (large sd to keep y-axis in line with other graphs)
plot(x = x,
     y = uniform,
     type = "l",                            # Specify that you want to plot a line graph
     lwd = 2,                               # Thickness of line
     col = "blue",                          # Colour of line
     xlim=c(-6,6),                          # Set limit of x-axis
     ylim=c(0,0.4),                         # Set limit of y-axis
     frame.plot=TRUE,                       # Do plot the frame of the graph
     xlab="effect size",                    # Title for x-axis
     ylab= " ",                             # Title for y-axis
     axes = FALSE,                          # Don't plot the scales by default
     main=paste("Uniform Alternative"))     # Main title
segments(-6,0,6,0, col='black', lwd=1)      # Add a line for x-axis
segments(0, 0, 0, 2, col= 'black', lwd=1)                # Add a black line at x=0
segments(0, 0.35, 3, 0.35, col= 'black', lwd=2)          # Add a black horizontal line
segments(0, 0.35, 0.15, 0.355, col= 'black', lwd=2)      # Arrow bits
segments(0, 0.35, 0.15, 0.345, col= 'black', lwd=2)      # Arrow bits
segments(3, 0.35, 2.85, 0.355, col= 'black', lwd=2)      # Arrow bits
segments(3, 0.35, 2.85, 0.345, col= 'black', lwd=2)      # Arrow bits
text(1.70, 0.37, paste("Expected Effects"), cex = 0.6)   # Add label
Axis(side=1,at=0,labels=0,lty=0)
Axis(side=2,at=0,labels=0,lty=0)
Axis(side=1, labels=FALSE)
Axis(side=2, labels=FALSE)


# plot H1 half-normal
x <- seq(0,6,length=100)          
half <- dnorm(x,mean=0, sd=1)
#jpeg("halfplot.jpg")
plot(x = x,
     y = half,
     type = "l",
     lwd = 2,
     col="blue",
     xlim=c(-6,6), 
     axes = FALSE,
     frame.plot=TRUE,
     ylab="probability", 
     xlab="effect size", 
     main=paste("Half-Normal Alternative"))
segments(-6,0,6,0, col='black', lwd=1)      
segments(0, 0, 0, 2, col= 'black', lwd=1)
segments(0, 0, 0, 0.398, col= 'blue', lwd=2)
segments(-6, 0, 0, 0, col= 'blue', lwd=2)
segments(1, 0, 1, 0.24, col= 'black', lwd=2)
text(2.60, 0.25, paste("Expected Effect"), cex = 0.6)
Axis(side=1,at=0,labels=0,lty=0)
Axis(side=2,at=0,labels=0,lty=0)
Axis(side=1, labels=FALSE)
Axis(side=2, labels=FALSE)
#dev.off()


# plot H1 normal
x <- seq(-6,6,length=100) 
#jpeg("normplot.jpg")
plot(x = x,                   
     y = dnorm(x, 2, 1),              
     type = "l",     
     col = "blue",
     axes = FALSE,
     frame.plot=TRUE,
     xlab = "effect size",                         
     ylab = " ",
     main=paste("Normal Alternative"),
     lwd=2)      
segments(-6,0,6,0, col='black', lwd=1)  
segments(2, 0, 2, 0.396, col= 'black', lwd=2)
segments(0, 0, 0, 2, col= 'black', lwd=1)
text(4, 0.40, paste("Expected Effect"), cex = 0.6)
Axis(side=1,at=0,labels=0,lty=0)
Axis(side=2,at=0,labels=0,lty=0)
Axis(side=1, labels=FALSE)
Axis(side=2, labels=FALSE)

dev.copy(png,'bayes_hypotheses_1.png')             # Save whatever is in displayed in the plot viewer in my wd
dev.off()                                          # Remember to execture this after saving as picture
getwd()
