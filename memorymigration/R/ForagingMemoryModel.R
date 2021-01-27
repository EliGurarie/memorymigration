#' Foraging with memory model
#' 
#' This function sets up the diffusion advection model 
#' 
#' @param t vector of times
#' @param pop a T x X matrix describing the population distribution 
#' across the time period.
#' @param parms named vector of parameters. THese are \code{alpha} - resource 
#' following coefficient; \code{beta0} - social cohesion coefficient; \code{beta1} - 
#' memory coefficient
#' @param pop_lag the T x X population distribution over time in the previous cycle (year)
#' @param resource a T x X distribution of the (potentially) dynamic resource.
#' @param dx time step
#' @return Model setup; output of \code{\link{tran.1D}} function. 

ForagingMemoryModel <- function(t, pop, parms, pop_lag, resource, 
                                dx = dx){
  tran.1D(C = pop, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * diff(resource)/dx + 
            parms["beta0"] * diff(pop)/dx + 
            parms["beta1"] * diff(pop_lag)/dx, 
          dx = dx)
}