#' Run Next Year
#' 
#' Based on a migratory population's set up (the World) and the values \code{alpha},
#'\code{beta0} and \code{beta1}, this function determines the population 
#'distribution for the following year. 
#' 
#' @param World List of 5 a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau,
#'  the dx value and the tau value. Set up by the getSinePop function. 
#' @param Parameters named vector of parameters. \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - resource 
#' following coefficient; \code{beta0} - social cohesion coefficient; \code{beta1} - 
#' memory coefficient
#' @return a T x X matrix describing the population distribution for the next year
#' @seealso \link{getSinePop}, \link{runManyYears}

runNextYear <- function(World, Parameters){
  pop1 <- World$pop
  pop2 <- pop1*0
  #pop2[1,] <- pop1[nrow(pop1),]
  
  Time <- World$time
  Resource <- World$resource
  
  for(t in Time){
    pop_lastyear <- c(0,pop1[t,],0) 
    if(t == 1) pop_now <- pop1[nrow(pop1),] else pop_now <- pop2[t-1,]
    pop2[t,] <- ode(y = pop_now, 
                    times = 0:1, 
                    parms = Parameters,
                    func = ForagingMemoryModel, 
                    resource = c(0,Resource[t,],0),
                    pop_lag = pop_lastyear,
                    dx = World$dx)[2,1:ncol(pop2)+1]
  }
  pop2
}

#' Run Many Years 
#' 
#' Based on a migratory population's set up (the World) and the values \code{alpha},
#'\code{beta0} and \code{beta1}, this function determines the population 
#'distribution after several years. 
#'
#' @param World List of 5 a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau,
#'  the dx value and the tau value. Set up by the getSinePop function. 
#' @param Parameters named vector of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - resource 
#' following coefficient; \code{beta0} - social cohesion coefficient; \code{beta1} - 
#' memory coefficient
#' @param n.years number of years the population migrates 
#' @param threshold the threshold for the social cohesion between two population 
#' distributions from two consecutive years 
#' @return a list of n.years containing T x X matrices describing the population 
#' distribution for each year after initial population
#' @seealso \link{getSinePop}, \link{runManyYears}

runManyYears <- function(World, Parameters, n.years, threshold){
  pop.list <- list(Year1 = World$pop)
  i <- 1
  cat(paste("running year ", i, "\n"))
  World$pop <- pop.list[[i]]
  pop.list[[i+1]] <- runNextYear(World, 
                                 Parameters = parameters)
  while(computeEfficiency(pop.list[[i]], pop.list[[i+1]], World)<threshold & i<n.years){
    i <- i+1
    cat(paste("running year ", i, "\n"))
    World$pop <- pop.list[[i]]
    pop.list[[i+1]] <- runNextYear(World, 
                                   Parameters = parameters)
    
  }
  names(pop.list) <- paste0("Year",0:(length(pop.list)-1))
  pop.list
}

