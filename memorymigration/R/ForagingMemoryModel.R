#' Foraging with memory model
#' 
#' This function sets up the diffusion advection model 
#' 
#' @param t vector of times
#' @param pop a T x X matrix describing the population distribution 
#' across the time period.
#' @param parms named vector of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - resource 
#' following coefficient; \code{beta0} - social cohesion coefficient; \code{beta1} - 
#' memory coefficient
#' @param memory the memory score
#' @param resource a T x X distribution of the (potentially) dynamic resource.
#' @param dx time step
#' @return Model setup; output of \code{\link{tran.1D}} function. 
#' @export

ForagingMemoryModel <- function(t, pop, parms, memory, resource, 
                                dx = dx){
  tran.1D(C = pop, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * diff(resource)/dx + 
            parms["beta"] * diff(memory)/dx, 
          dx = dx)
}


ForagingMemoryModel_v0 <- function(t, pop, parms, memory, resource, 
                                dx = dx){
  tran.1D(C = pop, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * diff(resource)/dx + 
            parms["beta0"] * diff(c(0,pop,0))/dx + 
            parms["beta1"] * diff(memory)/dx, 
          dx = dx)
}