#' Get Sine Pop
#' 
#' Initializes sinusoidal year 0 of a migratory population distribution 
#' via a sinusoidal migration pattern
#' 
#' @param tau maximum time in days for migratory population in one year
#' @param X.min minimum value of population distribution
#' @param X.max maximum value of population distribution
#' @param dx time step
#' @param peak.max minimum value of sinusoidal peak
#' @param peak.min maximum value of sinusoidal peak
#' @param sd standard deviation of population distribution
#' @return list of 5 which includes a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau,
#'  the dx value and the tau value.
#' @export
#' 
getSinePop <- function(tau, X.min = 0, X.max = 100, dx, 
                       peak.max, peak.min, sd){
  Time <- 1:tau
  
  X.edge <- seq(X.min, X.max, dx)
  X <- (X.edge[-1] + X.edge[-length(X.edge)])/2
  pop <- outer(Time, X,
               function(t,x) 
                 dnorm(x, mean = X.max/2 + (peak.max-peak.min) / 2 * sin( 2*(t * pi)/max(Time)),sd = sd))
  pop <- apply(pop, 1, function(x) x/sum(x)/dx) %>% t
  list(pop = pop, X = X, time = Time, dx = dx, tau = tau, X.max = X.max)
}

#' Get Optimal Pop
#' Initilaizes optimal initial population distribution based on resource
#' 
#' @param tau maximum time in days for migratory population in one year
#' @param X.min minimum value of population distribution
#' @param X.max maximum value of population distribution
#' @param dx time step
#' @param x.peak where the resource peak is in space
#' @param t.peak where the resource peak is in time
#' @param x.sd standard deviation of resource peak in space
#' @param t.sd standard deviation of resource peak in time
#' @export
getOptimalPop <- function(tau, X.min = 0, X.max = 100, dx, 
                          x.peak, t.peak, x.sd, t.sd){
  Time <- 1:tau
  
  t1 <- t.peak - t.sd
  t2 <- t.peak + t.sd
  t3 <- tau - t.peak - t.sd
  t4 <- tau - t.peak + t.sd
  x2 <-  X.max - x.peak
  x1 <-  x.peak
  slope <-  (x2 - x1) / (t3-t2)
  
  getX.mean <- function(t){
    ifelse(t > t1 & t < t2, x1, 
                 ifelse(t > t3 & t < t4, x2, 
                        ifelse(t >= t2 & t <= t3, x1 + (t-t2)*slope, 
                               ifelse(t <= t1, X.max/2 - slope*t, 
                                      x2 - (t - t4)*slope))))
  }
  
  X.edge <- seq(X.min, X.max, dx)
  X <- (X.edge[-1] + X.edge[-length(X.edge)])/2
  
  pop <- t(sapply(Time, function(t) dnorm(X, mean = getX.mean(t), sd = x.sd)))
  pop <- t(apply(pop, 1, function(x) x/sum(x)/dx))
  list(pop = pop, X = X, time = Time, dx = dx, tau = tau, X.max = X.max)
}
