#' getPulsedResource

getPulsedResource <- function(t, x, par){
  
  t.peak <- par["t.peak"] 
  t.sd <- par["t.sd"] 
  x.peak <- par["x.peak"]
  x.sd <- par["x.sd"]
  
  x.scaled <- x/max(x)
  x.peak <- x.peak/max(x)
  x.sd <- x.sd/max(x)
  
  t1 <- t[1:(length(t)/2)]
  
  m1 <- (dnorm(t1, mean = t.peak, sd = t.sd) %>% {./max(.)}) * (x.peak - .5) + .5
  m2 <- 1-m1[length(m1):1]
  m <- c(m1, m2)
  
  # spline the sigma
  k <- 1/sqrt(12)
  s1 <- k - ((dnorm(t1, t.peak, t.sd) %>% {./max(.)}) * (k - x.sd))
  s <- c(s1, s1[length(s1):1])
  
  a = -(m*s^2+m^3-m^2)/s^2
  b = ((m-1)*s^2+m^3-2*m^2+m)/s^2
  
  R <- sapply(1:length(t), function(i) dbeta(x.scaled, a[i], b[i])) %>% t
  R[,1] <- 1
  R[,length(x)] <- 1
  colnames(R) <- x
  rownames(R) <- t
  R <- apply(R, 1, function(x) x/sum(x)) %>% t
  return(R)
}
