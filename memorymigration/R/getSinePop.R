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
  list(pop = pop, X = X, time = Time, dx = dx, tau = tau)
}