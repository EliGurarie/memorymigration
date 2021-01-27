#' Initialize sinusoidal year 0

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