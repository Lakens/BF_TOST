Bf(sd=5, obtained=12, dfdata=10000000, uniform=0, lower=0, upper=1, meanoftheory=0, sdtheory=11, tail=2)

Bf<-function(sd, obtained, dfdata, uniform, lower=0, upper=1, meanoftheory=0, sdtheory=1, tail=2)
{
  area <- 0
  if(identical(uniform, 1)){
    theta <- lower
    range <- upper - lower
    incr <- range / 2000
    for (A in -1000:1000){
      theta <- theta + incr
      dist_theta <- 1 / range
      height <- dist_theta * dnorm(obtained, theta, sd)
      area <- area + height * incr
    }
  }else{
    theta <- meanoftheory - 5 * sdtheory
    incr <- sdtheory / 200
    for (A in -1000:1000){
      theta <- theta + incr
      dist_theta <- dnorm(theta, meanoftheory, sdtheory)
      if(identical(tail, 1)){
        if (theta <= 0){
          dist_theta <- 0
        } else {
          dist_theta <- dist_theta * 2
        }
      }
      height <- dist_theta * dt((obtained-theta)/sd, df=dfdata)
      area <- area + height * incr
    }
  }
  LikelihoodTheory <- area
  Likelihoodnull <- dt(obtained/sd, df = dfdata)
  BayesFactor <- LikelihoodTheory / Likelihoodnull
  ret <- list("LikelihoodTheory" = LikelihoodTheory, "Likelihoodnull" = Likelihoodnull, "BayesFactor" = BayesFactor)
  ret
}
