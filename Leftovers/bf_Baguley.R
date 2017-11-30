Bf<-function(sd, obtained, uniform, lower=0, upper=1, meanoftheory=0, sdtheory=1, tail=2)
{
  #Authors Danny Kaye & Thom Baguley
  #Version 1.0
  #19/10/2009
  #test data can be found starting at p100
  #
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
      height <- dist_theta * dnorm(obtained, theta, sd)
      area <- area + height * incr
    }
  }
  LikelihoodTheory <- area
  Likelihoodnull <- dnorm(obtained, 0, sd)
  BayesFactor <- LikelihoodTheory / Likelihoodnull
  ret <- list("LikelihoodTheory" = LikelihoodTheory, "Likelihoodnull" = Likelihoodnull, "BayesFactor" = BayesFactor)
  ret
}


contrast<-function(SEd, meand, nu, theta1, theta2)
{
  #Authors Danny Kaye & Thom Baguley
  #19/10/2009
  #The test data is found in Box 3.7 pp 74-5
  #SEd - contrast standard error
  #meand - contrast mean
  #nu - contrast  degrees of freedom
  #
  likelihoodmax <- 0
  theta <- rep(0, 1000)
  likelihood <- rep(0, 1000)
  theta[1] <- meand - 5 * SEd
  inc <- SEd / 100
  
  B <- 0
  
  for(B in 1:1000){
    theta[B] <- theta[1] + (B - 1) * inc
    likelihood[B] <- (1 + (meand - theta[B])^2 / (nu * SEd^2))^(-(nu + 1) / 2)
    if(likelihood[B] > likelihoodmax){
      likelihoodmax <- likelihood[B]
    }
  }
  for(B in 1:1000){
    likelihood[B] <- likelihood[B] / likelihoodmax
  }
  
  B <- 1
  while(likelihood[B] < 1/32){
    B <- B + 1
  }
  begin32 <- theta[B]
  while(likelihood[B] < 1/8){
    B <- B + 1
  }
  begin8 <- theta[B]
  while(likelihood[B] > 1/8){
    B <- B + 1
  }
  end8 <- theta[B]
  while(likelihood[B] > 1/32){
    B <- B + 1
  }
  end32 <- theta[B]
  
  B1 <- round((theta1 - theta[1]) / inc + 1, 0)
  B2 <- round((theta2 - theta[1]) / inc + 1, 0)
  likelihoodratio <- likelihood[B1] / likelihood[B2]
  ret <- list("begin8"=begin8, "end8"=end8, "begin32"=begin32, "end32"=end32, "likelihoodratio"=likelihoodratio)
  ret
}

meanUV<-function(SEd, meand, n, theta1, theta2)
{
  #Authors Danny Kaye & Thom Baguley
  #Version 1.0
  #19/10/2009
  #The test data is from page 138
  #SEd - sample standard error
  #meand - sample mean
  #n - number of subjects
  #theta1 - population mean assumed by first hypothesis
  #theta2 - population mean assumed by second hypothesis
  #
  on.exit({B})
  Vard <- n * SEd^2
  SSd <- Vard * (n - 1)
  
  likelihoodmax <- 0
  theta <- rep(0, 1000)
  likelihood <- rep(0, 1000)
  theta[1] <- meand - 5 * SEd
  inc <- SEd / 100
  
  B <- 1
  
  for(B in 1:1000){
    theta[B] <-theta[1] + (B - 1) * inc
    likelihood[B] <- (SSd + n * (meand - theta[B])^2)^(-(n - 2) / 2)
    if(likelihood[B] > likelihoodmax){
      likelihoodmax <- likelihood[B]
    }
  }
  for(B in 1:1000){
    likelihood[B] <-likelihood[B] / likelihoodmax
  }
  
  B <- 1
  while(likelihood[B] < 1/32 ){
    B <- B + 1
  }
  begin32 <- theta[B]
  while(likelihood[B] < 1/8){
    B <- B + 1
  }
  begin8 <- theta[B]
  while(likelihood[B] > 1/8){
    B <- B + 1
  }
  end8 <- theta[B]
  while(likelihood[B] > 1/32){
    B <- B + 1
  }
  end32 <- theta[B]
  
  B1 <- round((theta1 - theta[1]) / inc + 1, 0)
  B2 <- round((theta2 - theta[1]) / inc + 1, 0)
  likelihoodratio <- likelihood[B1] / likelihood[B2]
  ret <- list("begin8"=begin8, "end8"=end8, "begin32"=begin32, "end32"=end32, "likelihoodratio"=likelihoodratio)
  ret
}


cross2x2_odds<-function(a, b, c, d, psi1, psi2)
{
  #Authors Danny Kaye & Thom Baguley
  #Version 1.0
  #19/10/2009
  #The test data is from figure 5.4 p 135
  #
  m <- a + b
  n <- c + d
  k <- a + c
  l <- b + d
  
  psimax <- 0
  likelihoodmax <- 0
  A <- 0
  B <- 0
  likelihood <- rep(0, 10000)
  psi <- rep(0, 10000)
  starting <- 0
  ending <- 0
  
  for(B in 1:10000){
    psi[B] <- B / 100
    starting <- 0
    if((a - d) > 0){
      starting <- a - d
    }
    ending <- m
    if((a + c) < m){
      ending <- a + c
    }
    for(A in starting:ending){
      likelihood[B] <- likelihood[B] + choose(m, A) * choose(n, (a + c - A)) * psi[B]^(A - a)
    }
    likelihood[B] <- likelihood[B]^-1
    if(likelihood[B] > likelihoodmax){
      likelihoodmax <- likelihood[B]
      psimax <- psi[B]
    }
  }
  for(B in 1:10000){
    likelihood[B] <- likelihood[B] / likelihoodmax
  }
  
  B <- 1
  while(likelihood[B] < 1/32){
    B <- B + 1
  }
  begin32 <- psi[B]
  while(likelihood[B] < 1/8){
    B <- B + 1
  }
  begin8 <- psi[B]
  while(likelihood[B] > 1/8){
    B <- B + 1
  }
  end8 <- psi[B]
  while(likelihood[B] > 1/32){
    B <- B + 1
  }
  end32 <- psi[B]
  
  psi1 <- psi1 * 100
  psi2 <- psi2 * 100
  likelihoodratio <- likelihood[psi1] / likelihood[psi2]
  ret <- list("psimax"=psimax, "begin8"=begin8, "end8"=end8, "begin32"=begin32, "end32"=end32, "likelihoodratio"=likelihoodratio)
  ret
}

cross2x2<-function(a, b, c, d, gamma1, gamma2)
{
  #Authors Danny Kaye & Thom Baguley
  #Version 1.0
  #19/10/2009
  #The test data is from figure 5.4 p 135
  #
  m <- a + b
  n <- c + d
  k <- a + c
  l <- b + d
  
  gammamax <- 0
  likelihoodmax <- 0
  A <- 0
  B <- 0
  likelihood <- rep(0, 10000)
  gamma <- rep(0, 10000)
  
  for(B in 1:10000){
    gamma[B] <- B / 100
    for(A in b:(m + n - d)){
      likelihood[B] <- likelihood[B] + choose(A - 1, b - 1) * choose(m + n - A - 1, d - 1) * gamma[B]^(A - m)
    }
    likelihood[B] <- likelihood[B]^-1
    if(likelihood[B] > likelihoodmax){
      likelihoodmax <- likelihood[B]
      gammamax <- gamma[B]
    }
  }
  for(B in 1:10000){
    likelihood[B] <- likelihood[B] / likelihoodmax
  }
  
  B <- 1
  while(likelihood[B] < 1/32){
    B <- B + 1
  }
  begin32 <- gamma[B]
  while(likelihood[B] < 1/8){
    B <- B + 1
  }
  begin8 <- gamma[B]
  while(likelihood[B] > 1/8){
    B <- B + 1
  }
  end8 <- gamma[B]
  while(likelihood[B] > 1/32){
    B <- B + 1
  }
  end32 <- gamma[B]
  
  gamma1 <- gamma1 * 10
  gamma2 <- gamma2 * 10
  likelihoodratio <- likelihood[gamma1] / likelihood[gamma2]
  ret <- list("gammamax"=gammamax, "begin8"=begin8, "end8"=end8, "begin32"=begin32, "end32"=end32, "likelihoodratio"=likelihoodratio)
  ret
}

proportion<-function(suc, fail)
{
  #Authors Danny Kaye & Thom Baguley
  #Version 1.0
  #19/10/2009
  #The test data is from figure 5.3 p131
  #not sure this does what he says it does, no theta input is asked for
  #in the book code, as implied in the text and no ratio is returned
  #note that the function is called with successes and failures not 
  #successes and trials
  #
  thetamax <- 0
  likelihoodmax <- 0
  theta <- rep(0,1000)
  likelihood <- rep(0,1000)
  B <- 0
  
  for(B in 1:1000){
    theta[B] <- B/1000
    likelihood[B] <- theta[B]^suc*(1-theta[B])^fail
    if(likelihood[B] > likelihoodmax){
      likelihoodmax <- likelihood[B]
      thetamax <- theta[B]
    }
  }
  for(B in 1:1000){
    likelihood[B] <- likelihood[B] / likelihoodmax
  }
  
  B <- 1
  
  while(likelihood[B] < 1/32){
    B <- B + 1
  }
  begin32 <- theta[B]
  while(likelihood[B] < 1/8){
    B <- B + 1
  }
  begin8 <- theta[B]
  while(likelihood[B] > 1/8){
    B <- B + 1
  }
  end8 <- theta[B]
  while(likelihood[B] > 1/32){
    B <- B + 1
  }
  end32 <- theta[B]
  
  ret <- list("thetamax"=thetamax, "begin8"=begin8, "end8"=end8,
              "begin32"=begin32, "end32"=end32)
  ret
}