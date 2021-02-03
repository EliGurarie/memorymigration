#' Get Pulsed Resource
#' 
#' This function generates a a seasonal resource function with the following properties:
#' 1. The total amount of resource across space is constant throughout the year.
#' 2. At the beginning, middle, and end of the year the resource is uniformly distributed. 
#' 3. At some peak time $t_r < tau/2$, the resource concentrates at a location $x_r < chi/2$
#'  with a spatial deviation $sigma_x$ and a temporal deviation $sigma_t$ (where $tau$ 
#'  is the length of the year and $chi$ is the extent of the spatial domain).
#' 4. The resource peaks exactly symmetrically at time $tau - t_r$ and location 
#' $chi - x_r$ with the same variance $sigma_r$.
#' 
#' To generate a resource with these properties, we distributing the resource in 
#' space as a beta distribution, where the two shape and scale parameters vary 
#' sinusoidally in such a way as to fulfill the criteria above. 
#' 
#' @param t vector with time values 
#' @param x vector with X-axis values
#' @param par vector with four values: t.peak (which is the maximum peak of t), 
#' t.sd (which is the standard deviation of t), x.peak (which is the maximum peak of x),
#' and x.sd (which is the standard deviation of x)
#' @return A X x X matrix containing values of the resource distribution


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
