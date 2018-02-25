# Tell R to create four graphs that I'd like to appear in a 2x2 format

#install.packages('extrafont')
library(extrafont)
#font_import() # This takes some time! But you only have to do it once.
loadfonts()
fonts()

jpeg(file="Fig2.jpg",width=4000,height=2500, res = 500)

par(mfrow = c(2, 2), mar=c(3.5,3,1.5,1), family = "Segoe UI")
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
     font.main = 1,
     main=paste("Point Null Hypothesis"),   # Main title
     axes=FALSE,                            # Don't plot the scales by default
     mgp = c(2, 2, 2), 
     frame.plot=TRUE)                       # Do plot the frame of the graph

segments(0, 0, 0, 2, col= 'darkgrey', lwd=2)    # Add a darkgrey line at x=0 to show where what the null hypothesis predicts. Note: (x-startingpoint, y-startingpoint, x-endpoint, y-endpoint)
segments(-6,0,6,0, col='darkgrey', lwd=2)       # Add a darkgrey line at y=0 to show that all other effects are considered impossible under H0
Axis(side=1,at=0,labels=0,lty=0)            # Adds '0' at the mid point of x-axis
#Axis(side=2,at=0,labels=0,lty=0)            # Adds '0' at the mid point of y-axis
Axis(side=1, labels=FALSE)                  # Adds tags but not labels to x axis
#Axis(side=2, labels=FALSE)                  # Adds tags but not labels to y axis

# plot H1 uniform
x <- seq(-6,6,length=1000)                   # Create a series of 100 numbers from -6 to 6
uniform <- dunif(x,0, 3)                    # Create a normal distribution, m=0, sd=3 (large sd to keep y-axis in line with other graphs)
plot(x = x,
     y = uniform,
     type = "l",                            # Specify that you want to plot a line graph
     lwd = 2,                               # Thickness of line
     col = "darkgrey",                          # Colour of line
     xlim=c(-6,6),                          # Set limit of x-axis
     ylim=c(0,0.45),                         # Set limit of y-axis
     frame.plot=TRUE,                       # Do plot the frame of the graph
     xlab="effect size",                    # Title for x-axis
     ylab= " ",                             # Title for y-axis
     axes = FALSE,                          # Don't plot the scales by default
     mgp = c(2, 1, 0), 
     font.main = 1,
     main=paste("Uniform Alternative"))     # Main title
segments(-6,0,6,0, col='black', lwd=1)      # Add a line for x-axis
#segments(0, 0, 0, 2, col= 'black', lwd=1)                # Add a black line at x=0
arrows(0, 0.37, 3, 0.37, col= 'black', lwd=2, length = 0.05, code = 3)
text(1.5, 0.42, paste("Expected Effect"), cex = 0.9)   # Add label
Axis(side=1,at=0,labels=0,lty=0)
#Axis(side=2,at=0,labels=0,lty=0)
Axis(side=1, labels=FALSE)
#Axis(side=2, labels=FALSE)
polygon(x,uniform,col=rgb(0.5, 0.5, 0.5, 0.5),border = NULL)


# plot H1 half-normal
x <- seq(0,6,length=1000)          
half <- dnorm(x,mean=0, sd=1)
#jpeg("halfplot.jpg")
plot(x = x,
     y = half,
     type = "l",
     lwd = 2,
     col="darkgrey",
     xlim=c(-6,6), 
     ylim=c(0,0.5),                         # Set limit of y-axis
     axes = FALSE,
     mgp = c(2, 2, 2), 
     frame.plot=TRUE,
     ylab="probability", 
     xlab="effect size", 
     font.main = 1,
     main=paste("Half-Normal Alternative"))
segments(-6,0,6,0, col='black', lwd=1)      
#segments(0, 0, 0, 2, col= 'black', lwd=1)
segments(0, 0, 0, 0.398, col= 'darkgrey', lwd=2)
segments(-6, 0, 0, 0, col= 'darkgrey', lwd=2)
segments(1, 0, 1, 0.42, col= 'black', lwd=2)
text(1, 0.47, paste("Expected Effect"), cex = 0.9)
Axis(side=1,at=0,labels=0,lty=0)
#Axis(side=2,at=0,labels=0,lty=0)
Axis(side=1, labels=FALSE)
#Axis(side=2, labels=FALSE)
polygon(c(0,x,10),c(0,half,0),col=rgb(0.5, 0.5, 0.5, 0.5))


# plot H1 normal
x <- seq(-6,6,length=1000) 
#jpeg("normplot.jpg")
plot(x = x,                   
     y = dnorm(x, 2, 1),              
     type = "l",     
     col = "white",
     axes = FALSE,
     mgp = c(2, 2, 2), 
     ylim=c(0,0.5),                         # Set limit of y-axis
     frame.plot=TRUE,
     xlab = "effect size",                         
     ylab = " ",
     font.main = 1,
     main=paste("Normal Alternative"),
     lwd=2,
     )      
segments(-6,0,6,0, col='black', lwd=1)  
segments(2, 0, 2, 0.42, col= 'black', lwd=2)
#segments(0, 0, 0, 2, col= 'black', lwd=1)
text(2, 0.47, paste("Expected Effect"), cex = 0.9)
Axis(side=1,at=0,labels=0,lty=0)
#Axis(side=2,at=0,labels=0,lty=0)
Axis(side=1, labels=FALSE)
#Axis(side=2, labels=FALSE)
polygon(x,dnorm(x, 2, 1),col=rgb(0.5, 0.5, 0.5, 0.5),border = NULL)

dev.off()                                          # Remember to execture this after saving as picture
