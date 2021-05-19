#' Run Next Year
#' 
#' Based on a migratory population's set up (the World) and the values \code{alpha},
#'\code{beta0} and \code{beta1}, this function determines the population 
#'distribution for the following year. 
#' 
#' @param World List of 5: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau,
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 6.
#'  Set up by the getSinePop function. 
#' @param Parameters named vector of parameters. \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - resource 
#' following coefficient; \code{beta0} - social cohesion coefficient; \code{beta1} - 
#' memory coefficient
#' @return a T x X matrix describing the population distribution for the next year
#' @seealso \link{getSinePop}, \link{runManyYears}
#' @export
#' 
#' 

runNextYear <- function(World, Parameters, Pop_lastyear, Year){
  pop0 <- World$pop
  pop1 <- Pop_lastyear
  pop2 <- pop0*0
  Time <- World$time
  memory <- Parameters["kappa"] * getMem(pop0, World) + 
    (1 - Parameters["kappa"]) * getMem(pop1, World)
  dmemory <- diff(extend(memory))
  
  X.edge <- seq(0,World$X.max, World$dx)
  X.matrix <- matrix(rep(X.edge, times = World$tau), byrow = TRUE, nrow = World$tau)
  memory.matrix <- matrix(rep(memory, length(X.edge)), byrow = FALSE, nrow = World$tau)
  dmemory.matrix <- matrix(rep(dmemory, length(X.edge)), byrow = FALSE, nrow = World$tau)
  
  dm.matrix <- dm.adjust(X.matrix, memory.matrix, dmemory.matrix,
                         Parameters["beta"], Parameters["lambda"])
  
  if(length(dim(World$resource)) == 3)
    resource <- World$resource[Year,,] else
      resource <- World$resource
  
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
                      memory = dm.matrix[t,],
                      dx = World$dx)[2,1:ncol(pop2)+1]
  }
  if(any(pop2 < 0)){
    warning("Some negative population values will be coerced to 0.")
  }
  pop2[pop2 < 0] <- 0
  pop2 <- apply(pop2, 1, function(x) x/sum(x)) %>% t
  pop2 
}