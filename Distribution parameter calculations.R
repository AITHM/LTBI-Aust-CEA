### BETA DISTRIBUTIONS
# Functions for generating alpha and beta values
ssqs4findbeta2 <- function(a, landu95, targetmean){
  b <- (a/targetmean) - a
  landu <- qbeta(c(0.025, 0.975), a, b)
  return((landu[1] - landu95[1]) ^ 2 + (landu[2] - landu95[2]) ^ 2) 
}

findbeta2<-function(tmean, l95, u95){
  astart <- 1
  bstart <- (astart/tmean)-astart
  result <- optimize(f = ssqs4findbeta2, interval = c(0, 1000),
                   landu95 = c(l95, u95), targetmean = tmean)
  a <- result$minimum
  b <- (a/tmean) - a
  #print("ssqs")
  print(result$objective)
  print("intervals with beta params")
  print(qbeta(c(0.025, .975), a, b))
  print("returning beta params")
  return(c(a, b))
} 

### GAMMA DISTRIBUTIONS
# Functions for generating alpha and beta values
ssqs4findgamma2 <- function(shape, landu95, targetmean){
  scale <- targetmean/shape
  landu <- qgamma(c(0.025, 0.975), shape = shape, scale = scale)
  return((landu[1] - landu95[1]) ^ 2 + (landu[2] - landu95[2]) ^ 2) 
}

findgamma2 <- function(tmean, l95, u95){
  shapestart <- 1
  scalestart <- tmean/shapestart
  result <- optimize(f = ssqs4findgamma2, interval = c(0, 1000),
                     landu95 = c(l95, u95),
                     targetmean = tmean)
  shape <- result$minimum
  scale <- (tmean/shape)
  #print("ssqs")
  print(result$objective)
  print("intervals with gamma params")
  print(qgamma(c(0.025, .975), shape = shape, scale = scale))
  print("returning gamma shape and scale params")
  return(c(shape,scale))
  #print("returning gamma shape and rate params (use these in winbugs)")
  #return(c(shape, 1/scale))
} 

