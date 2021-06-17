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
#' @param world object (e.g. from getSinePop) containing at minimum tau, X and X.max
#' @param par vector with four values: t.peak (which is the maximum peak of t), 
#' t.sd (which is the standard deviation of t), x.peak (which is the maximum peak of x),
#' and x.sd (which is the standard deviation of x), psi_x (psochasticity in spatial peak), 
#' psi_t (psochasticity in temporal peak). can be created by getCCpars for multiple years
#' @return A X x X matrix containing values of the resource distribution
#' @example examples/WorldsandResources.R
#' @export
#' @seealso \link{getResource_island}, \link{getResource_drifting}

getCCpars <- function(mu_x0, mu_t0, sigma_x, sigma_t, 
                      n.years, 
                      beta_x = 0, beta_t = 0, 
                      psi_x = 0, psi_t = 0){
  cbind(x.peak = mu_x0 - beta_x*1:n.years + rnorm(n.years,sd = psi_x),
        t.peak = mu_t0 + beta_t*1:n.years + rnorm(n.years,sd = psi_t),
        x.sd = rep(sigma_x, n.years),
        t.sd = rep(sigma_t, n.years))
}


#' Get Resource Drifting 
#' 
#' This function generates a seasonal resource function with the following properties:
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
#' @param world world object (e.g. from getSinePop/getOptimalPop); list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#' @param par vector with four values: t.peak (which is the maximum peak of t), 
#' t.sd (which is the standard deviation of t), x.peak (which is the maximum peak of x),
#' and x.sd (which is the standard deviation of x).
#' @param x.null extent of "dead area" - needed to keep the process from extending too far beyond the limits of the range.
#' @return A X x X matrix containing values of the resource distribution
#' @example examples/WorldsandResources.R
#' @export
#' @seealso \link{getResource_island}, \link{getCCpars}
#' 
getResource_drifting <- function(world, par, x.null = 0){
  
  t <- 1:world$tau
  x <- world$X
  x.max <- world$X.max - x.null
  x.min <- world$X.min + x.null
  x.range <- x.max - x.min
  
  t.peak <- par["t.peak"] 
  t.sd <- par["t.sd"] 
  
  x.peak <- par["x.peak"]
  x.sd <- par["x.sd"]
  
  x.scaled <- (x - x.min)/x.range
  x.peak <- (x.peak - x.min)/x.range
  x.sd <- x.sd/x.range
  
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
  #R[,1] <- 1
  #R[,length(x)] <- 1
  colnames(R) <- x
  rownames(R) <- t
  R <- apply(R, 1, function(x) x/sum(x)) %>% t
  attr(R, "par") <- par
  return(R)
}


#'
#'  #' Get Island
#' 
#' This function generates a seasonal pulsed resource function.
#' 
#' 
#' @param world world object (e.g. from getSinePop/getOptimalPop); list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#' @param par vector with four values: t.peak (which is the maximum peak of t), 
#' t.sd (which is the standard deviation of t), x.peak (which is the maximum peak of x),
#' and x.sd (which is the standard deviation of x). can be created by getCCpars for multiple years
#' @return A X x X matrix containing values of the resource distribution
#' @example examples/WorldsandResources.R
#' @export
#' @seealso \link{getResource_drifting}, \link{getCCpars}
#' 
getResource_island <- function(world, par){
  
  # par <- c(t.peak = 25, x.peak = 40, x.sd = 5, t.sd = 12)
  
  t <- world$time
  x <- world$X
  x.range <- world$X.max - world$X.min
  
  t.peak <- par["t.peak"] 
  t.sd <- par["t.sd"] 
  x.peak <- par["x.peak"]
  x.sd <- par["x.sd"]
  
  #x.peak.scaled <- (x.peak-world$X.min)/x.range
  
  season2 <- season1 <- sapply(t, function(t1){
                   dmvnorm(cbind(t1,x), 
                     mean = c(t.peak, x.peak), 
                     sigma = diag(c(t.sd^2, x.sd^2)))}) %>% t
  
  season2[nrow(season1):1, ncol(season1):1] <- season1
  
  R <- season1 + season2
  colnames(R) <- x
  rownames(R) <- t
  R <- R/sum(R) * world$tau
  attr(R, "par") <- par
  return(R)
}
