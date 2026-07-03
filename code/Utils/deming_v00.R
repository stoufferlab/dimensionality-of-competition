
#######  Estimates and covariance matrix #########
deming.estim <- function(x,y,lambda=1){  # lambda=sigmay²/sigmax²
  n <- length(x)
  my <- mean(y)
  mx <- mean(x)
  SSDy <- crossprod(y-my)[,]
  SSDx <- crossprod(x-mx)[,]
  SPDxy <- crossprod(x-mx,y-my)[,] 
  A <- sqrt((SSDy - lambda*SSDx)^2 + 4*lambda*SPDxy^2)
  B <- SSDy - lambda*SSDx
  beta <- (B + A) / (2*SPDxy)
  alpha <- my - mx*beta
  sigma.uu <- ( (SSDy + lambda*SSDx) - A ) /(2*lambda) / (n-1)
  s.vv <- crossprod(y-my-beta*(x-mx))/(n-2) # = (lambda+beta^2)*sigma.uu * (n-1)/(n-2)
  # formula Gilard & Iles 
  sbeta2.Fuller <- (SSDx*SSDy-SPDxy^2)/n/(SPDxy^2/beta^2)
  sbeta.Fuller <- sqrt(sbeta2.Fuller)
  # standard error alpha Fuller 
  salpha2.Fuller <- s.vv/n + mx^2*sbeta2.Fuller  
  salpha.Fuller <- sqrt(salpha2.Fuller)
  # 
  V <- rbind( c(salpha2.Fuller, -mx*sbeta2.Fuller), c(-mx*sbeta2.Fuller, sbeta2.Fuller) )	
  return(list(alpha=alpha,beta=beta, salpha.Fuller=salpha.Fuller, sbeta.Fuller=sbeta.Fuller, 
              V=V, 
              sigma=sqrt(sigma.uu*(n-1)/(n-2)))
  ) 
}


######## Confidence intervals about slope and intercept #########
deming.ci <- function(x, y, lambda=1, level=95/100){
  n <- length(x)
  fit <- deming.estim(x,y, lambda=lambda) 
  sigma <- fit$sigma
  V <- fit$V
  a <- fit$alpha
  b <- fit$beta
  t <- qt(level,df=n-2)
  out <- rbind(
    a=c(a, a + c(-1,1)*t*sqrt(V[1,1])),
    b=c(b, b + c(-1,1)*t*sqrt(V[2,2]))
  )
  colnames(out) <- c("estimate", "lower", "upper")
  names(dimnames(out)) <- c("parameter", "")
  return(out)
}


######## Confidence interval about a+b*xnew  ###########
deming.ci.xnew <- function(x, y, lambda=1, xnew, level=95/100){
  n <- length(x)
  fit <- deming.estim(x,y, lambda=lambda) 
  sigma <- fit$sigma*sqrt((n-2)/(n-1))
  H <- fit$V
  xx <- c(1,xnew)
  V <- xx%*%H%*%xx
  T <- fit$alpha+fit$beta*xnew
  t <- qt(1-(1-level)/2, df=n-2)
  bounds <- T + c(-1,1)*t*as.vector(sqrt(V))
  return(c(estimate=T, lwr=bounds[1], upr=bounds[2]))
}


#########  Prediction intervals ###############
deming.predict <-  function(x, y, lambda=1, xnew,  level=95/100){
  n <- length(x)
  fit <- deming.estim(x,y,lambda=lambda) 
  sigma <- fit$sigma
  sigma.uu <- sigma^2*(n-2)/(n-1)
  V <- fit$V
  a <- fit$alpha
  b <- fit$beta
  ynew <- a+b*xnew
  Xnew <- as.matrix(c(1,xnew))
  sigma.ee <- lambda*sigma.uu 
  t <- qt(1-(1-level)/2, n-2)
  ##  predict observed ynew given theoretical xnew
  sd.ynew.Fuller <- sqrt( sigma.ee + t(Xnew)%*%V%*%Xnew )
  Lynew.Fuller <- ynew - t*sd.ynew.Fuller 
  Uynew.Fuller <- ynew + t*sd.ynew.Fuller 
  # predict from an observed x_new
  sd.ynew.Fuller2 <- sqrt( sigma.ee + t(Xnew)%*%V%*%Xnew + (b^2+V[2,2])*sigma.uu)
  Lynew.Fuller2 <- ynew - t*sd.ynew.Fuller2 
  Uynew.Fuller2 <- ynew + t*sd.ynew.Fuller2 
  #
  # predict difference (y-x) given theoretical xnew
  sd.ynew.Fuller.diff <- sqrt( sigma.ee + t(Xnew)%*%V%*%Xnew + sigma.uu) 
  Lynew.Fuller.diff <- ynew - xnew - t*sd.ynew.Fuller.diff
  Uynew.Fuller.diff <- ynew - xnew + t*sd.ynew.Fuller.diff
  #
  out <- rbind(
    pred1=c(ynew, Lynew.Fuller, Uynew.Fuller),
    pred2=c(ynew, Lynew.Fuller2, Uynew.Fuller2),
    diff=c(ynew-xnew, Lynew.Fuller.diff, Uynew.Fuller.diff)
  )
  colnames(out) <- c("estimate", "lower", "upper")
  out
}

