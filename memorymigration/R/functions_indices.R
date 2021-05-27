#' Compute social cohesiveness
#' 
#' Returns a cohesiveness index of a single cycle population 
#' process: CI = 1 - MSD(X) / SD_max, where 
#' SD_max = X_max/sqrt(12), i.e. the standard deviation of a 
#' uniform distribution in the domain 0-X_max; and MSD(X) is 
#' the mean standard deviation of the population.
#'  
#' @param pop One cycle of population process
#' @param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @return a names vector of mean and standard deviation of SC
#' @seealso \link{computeEfficiency}, \link{computeMigratoriness}, \link{computeIndices}
#' @example examples/indices_examples.R
#' @export
#' 
computeCohesiveness <- function(pop, world){
  
  getSD <- function(p, world){
    EX <- sum( world$X * p) * world$dx
    EX2 <- sum( world$X^2 * p) * world$dx
    sqrt(EX2 - EX^2)
  }
  
  SDs <- apply(pop, 1, getSD, world = world)
  SD.max <- (1/sqrt(12))*max(world$X)
  SC <- 1 - SDs/SD.max
  c(SC.mean = mean(SC), SC.sd = sd(SC))
}

#' Compute Efficiency
#' 
#' Returns an efficiency of foraging index of a single cycle population 
#' process, based on comparing the population distribution to a resource
#' distribution.
#' 
#' @param pop One cycle of population process
#' @param resource The resource in an given cycle to compare to pop
#' @param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @return a single foraging efficiecy (FE) index
#' @seealso \link{computeCohesiveness}, \link{computeMigratoriness}, \link{computeIndices}
#' @example examples/indices_examples.R
#' @export

computeEfficiency <- function(pop, resource, world){
  dx <- world$dx
  tau <- world$tau
  if(any(pop < 0)){
    warning("Some of first distribution less than zero.")
    pop[pop < 0] <- 0
  }
  if(any(resource < 0)){
    warning("Some of second distribution less than zero.")
    resource[resource < 0] <- 0
  }
  sum(sqrt(pop*resource))*dx/tau
}

#' Compute Migratoriness
#' 
#' Returns an migratoriness index of a population by comparing the overlap 
#' overlap of the population from two different times within the year, 
#' choosing those times to minimize the overlap, and reporting the 
#' overall overlap.
#' 
#' @param pop One cycle of population process
#' @param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @return a list with the time of the minimal overlap and the Migratoriness 
#' (MI) index 
#' @seealso \link{computeEfficiency}, \link{computeCohesiveness}, \link{computeIndices}
#' @example examples/indices_examples.R
#' @export


computeMigratoriness <- function(pop, world){

  dx <- world$dx
  tau <- world$tau
  
  Overlap <- matrix(NA, tau, tau)
  for(i in 1:(tau-1))
    for(j in (i+1):tau)
      Overlap[i,j] <- sum(sqrt(pop[i,] * pop[j,])) * dx
  
  minOverlap <-  1-min(Overlap, na.rm = TRUE)
  list(times = which(Overlap == minOverlap, arr.ind = TRUE),
       overlap = minOverlap)
}

#' Compute Indicies
#' 
#' Returns the three migratoriness indeces calculated by the cumputeCohesiveness,
#' commputeEfficiency, and computeMigratoriness functions for a population and 
#' distribution and a resource distribution. 
#' 
#'
#' @param pop One cycle of population process
#' @param resource The resource in an given cycle to compare to pop
#' @param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @return a data frame containing the Social Cohesiveness (SC) index,
#' the Migratoriness (MI) index, and the Foraging Efficiency (FE) index
#' @seealso \link{computeEfficiency}, \link{computeMigratoriness}, \link{computeCohesiveness}
#' @example examples/indices_examples.R
#' @export
#' 
computeIndices <- function(pop, resource, world){
  SC = computeCohesiveness(pop, world)["SC.mean"]
  MI = computeMigratoriness(pop, world)$overlap
  FE = computeEfficiency(pop, resource, world)
  data.frame(SC, MI, FE)
}
