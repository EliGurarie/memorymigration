#' Run Next Year
#' 
#' Based on a migratory population's set up (the World) and the values \code{epsilon}, 
#' \code{alpha}, \code{beta}, \code{kappa} and \code{lambda}, this function determines the population 
#'distribution for the following year. 
#' 
#' @param World world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @param Parameters named vector of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - resource following coefficient; \code{beta} - spatial scale of sociality; \code{kappa} - 
#' memory following coefficient; \code{lambda} - maximum speed
#' @return a T x X matrix describing the population distribution for the next year
#' @seealso \link{getSinePop}, \link{getOptimalPop}, \link{runManyYears}
#' @export
#' 
#' 
#' 

runNextYear <- function(world, Parameters, Pop_lastyear, Year){
  pop0 <- world$pop
  pop1 <- Pop_lastyear
  pop2 <- pop0*0
  Time <- world$time
  
  memory <- Parameters["kappa"]^Year * getMem(pop0, world) + 
    (1 - Parameters["kappa"]^Year) * getMem(pop1, world)
  dmemory <- diff(extend(memory))
  
  X.edge <- seq(world$X.min, world$X.max, world$dx)
  memory_velocity <- sapply(1:100, 
                           function(t) getMemoryVelocity(X.edge, memory[t], dmemory[t], 
                                                         Parameters["beta"], Parameters["lambda"])) %>% t
  
  if(length(dim(world$resource)) == 3)
    resource <- world$resource[Year,,] else
      resource <- world$resource
  
  dresource <- apply(resource, 1, ddx.edge) %>% t
  for(t in Time){
    if(t == 1) 
      pop_now <- Pop_lastyear[nrow(Pop_lastyear),] else 
        pop_now <- pop2[t-1,]
      pop2[t,] <- ode(y = pop_now, 
                      times = 0:1, 
                      parms = Parameters,
                      func = ForagingMemoryModel, 
                      dh = dresource[t,],
                      memory = memory_velocity[t,],
                      dx = world$dx)[2,1:ncol(pop2)+1]
  }
  if(any(pop2 < 0)){
    warning("Some negative population values will be coerced to 0.")
  }
  pop2[pop2 < 0] <- 0
  pop2 <- apply(pop2, 1, function(x) x/sum(x)) %>% t
  pop2 
}
