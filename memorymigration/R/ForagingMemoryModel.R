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

ForagingMemoryModel <- function(t, pop, parms, 
                                memory, dh, dx = dx){
  tran.1D(C = pop, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * dh/dx + memory,
          dx = dx)
}


#' @export
getMemoryVelocity <- function(x, m.hat, dm, beta, lambda){
  if(dm > 0){
    x0 <- m.hat - log(lambda/dm - 1)*beta  
    v <- lambda / (1 + exp((x - x0)/beta))
  } else
    if(dm < 0){
      x0 <- m.hat + log(lambda/(-dm) - 1)*beta 
      v <- -lambda / (1 + exp(-(x - x0)/beta))
    } else 
      v <- rep(0, length(x))
}


#' @export
getMem <- function(pop, world){
  apply(t(pop)*world$X, 2, sum) * world$dx
}

#' @export
extend <- function(x){
  x1 <- c(x[length(x)],x,x[1])
  (x1[-1] + x1[-length(x1)])/2
}

#' @export
ddx.edge <- function(x)
  diff(c(x[1], x, x[length(x)])) 