#' Compute social cohesiveness
#' 
#' Returns a cohesiveness index of a single cycle population 
#' process: CI = 1 - MSD(X) / SD_max, where 
#' SD_max = X_max/sqrt(12), i.e. the standard deviation of a 
#' uniform distribution in the domain 0-X_max; and MSD(X) is 
#' the mean standard deviation of the population.
#'  
#' @param pop One cycle of population process
#' @return a names vector of mean and standard deviation of SC
#' @seealso \link{computeEfficiency}, \link{computeMigratoriness}, \link{computeIndices}
#' @example examples/indices_examples.R
#' 
computeCohesiveness <- function(pop){
  
  getSD <- function(p, world){
    EX <- sum( world$X * p) * world$dx
    EX2 <- sum( world$X^2 * p) * world$dx
    sd  = sqrt(EX2 - EX^2)
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
#' @param world world object
#' @return a single foraging efficiecy (FE) index
#' @example examples/indices_examples.

computeEfficiency <- function(pop, resource, world){
  dx <- world$dx
  tau <- world$tau
  sum(sqrt(pop*resource))*dx/tau
}

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

computeIndices <- function(pop, resource, world){
  SC = computeCohesiveness(pop, world)["SC.mean"]
  MI = computeMigratoriness(pop, world)$overlap
  FE = computeEfficiency(pop, resource, world)
  data.frame(SC, MI, FE)
}