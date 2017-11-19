# Load the BayesFactor package
library(BayesFactor)

# Read in the data from the Data and Story Library
randDotStereo = read.table(url("http://lib.stat.cmu.edu/DASL/Datafiles/FusionTime.html"), 
                           header = FALSE, skip = 33, nrows = 78)
colnames(randDotStereo) = c("fuseTime", "condition")
randDotStereo$logFuseTime = log(randDotStereo$fuseTime)


bf.signed = ttestBF(formula = logFuseTime ~ condition, data = randDotStereo, 
                    nullInterval = c(-Inf, 0))
bf.signed
bf = ttestBF(formula = logFuseTime ~ condition, data = randDotStereo)
bf