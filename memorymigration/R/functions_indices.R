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

#' Average Efficiency
#' 
#' Returns an efficiency of foraging index of the last ten years of a population 
#' process, based on comparing the population distribution to a resource
#' distribution. Uses computeEfficiency and averages the FE value of the last 20 years 
#' of simulation run.
#' 
#' @param sim A simulation run from runManyYears. Use sim$pop for the population.  
#' @param resource The resource in an given cycle to compare to pop. Resource must have at least as many years as sim.
#' @param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @return a single average foraging efficiecy (avgFE) index
#' @seealso \link{computeEfficiency}, \link{computeCohesiveness}, \link{computeMigratoriness}, \link{computeIndices}
#' @example examples/indices_examples.R
#' @export
#' 
computeAvgEfficiency <- function(sim, resource, world){
  average <- data.frame()
  if (length(sim) <= 20){
    for(i in 2:length(sim)){
      efficiency <- computeEfficiency(sim[[i]], resource[i-1,,], world)
      average <- rbind(average, efficiency)
    }}
    
    if (length(sim) > 20){
      for(i in length(sim)-20:length(sim)){
        efficiency <- computeEfficiency(sim[[i]], resource[i-1,,], world)
        average <- rbind(average, efficiency)
      }}
  mean(average[,1])
}
#' 
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



#' Compute Migration Indices
#' 
#' Computes the spatial and the temporal mismatch of the population with the resource. 
#' 
#' @param sim A simulation run from runManyYears. 
#' @param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @return a dataframe with the four errors of the two seasonal peaks in space and time
#' @seealso \link{computeEfficiency}, \link{computeCohesiveness}, \link{computeMigratoriness}, \link{computeIndices}
#' @example examples/indices_examples.R
#' @export

computeMigrationIndices <- function(sim, world){
  
  dx <- world$dx
  tau <- world$tau
  par <- attributes(world$resource)$par
  m.hat <- tail(sim$migration.hat, 1)
  
  if(par["t.peak"] < m.hat$t1 | par["t.peak"] > m.hat$t1 + m.hat$dt1)
    t1.error <- min( abs(par["t.peak"] - m.hat$t1), 
                     abs(par["t.peak"] - m.hat$t1  - m.hat$dt1)) else 
                       t1.error = 0
  
  if(world$tau - par["t.peak"] < m.hat$t2 | 
     world$tau - par["t.peak"] > m.hat$t2 + m.hat$dt2)
    t2.error <- min( abs(world$tau - par["t.peak"] - m.hat$t2), 
                     abs( world$tau - par["t.peak"] - m.hat$t2  - m.hat$dt2)) else 
                       t2.error = 0
  
  x1.error <- par["x.peak"] - m.hat$x1
  x2.error <- - par["x.peak"] - m.hat$x2
  
  data.frame(t1.error, t2.error, x1.error, x2.error)
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

#' Compute Annual Efficiency
#' 
#' Computes the foraging efficiency of a simulation for each population year with its respective resource. 
#' 
#' @param sim A simulation run from runManyYears. 
#' @param resource The resource in an given cycle to compare to pop. Resource must have at least as many years as sim.
#' @param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @return a data frame with the foraging efficiecy (FE) index for each year of the simulation
#' @seealso \link{computeEfficiency}
#' @export
computeAnnualEfficiency <- function(sim, resource, world){
  annual <- data.frame()
  for(i in 2:length(sim)){
    efficiency <- computeEfficiency(sim[[i]], resource[i-1,,], world)
    annual <- rbind(annual, efficiency)
  }
  annual_row <- as.data.frame(t(annual))
  colnames(annual_row) <- paste("FE", 1:nrow(annual), sep = "")
  annual_row
  sapply(2:length(sim), function(i) computeEfficiency(sim[[i]], resource[i-1,,], world))
  
}

#' Keeping up with Climate Change - Spatial Adaptation Index
#' 
#' Computes the spatial adaptation (SA) index which is the slope of the migration location parameter
#' against the resource drift 
#' 
#' @param sim A simulation run from runManyYears.
#' @param cc.params Data frame containing resource peak timing and space information of x.peak, t.peak, x.sd
#' and t.sd. Generated by the function getCCpars
#' @seealso \link{getCCpars}
#' @return a single spatial adaptation (SA) index
#' 
#' @export

computeSpatialAdaptationIndex <- function(sim, cc.params, trim = 1){
  m.hat <- sim$migration.hat
  beta1_hat <- lm(x1~year, data = m.hat[-(1:trim),])$coef[2]
  beta2_hat <- lm(x2~year, data = m.hat[-(1:trim),])$coef[2]
  mean(c(beta1_hat/-cc.params$beta_x, beta2_hat/cc.params$beta_x))
}


#' Keeping up with Climate Change - Temporal Adaptation Index
#' 
#' Computes the temporal adaptation (TA) index which is the slope of the migration timing parameter
#' against the resource drift 
#' 
#' @param sim A simulation run from runManyYears.
#' @param cc.params Data frame containing resource peak timing and space information of x.peak, t.peak, x.sd
#' and t.sd. Generated by the function getCCpars
#' @seealso \link{getCCpars}
#' @return a single temporal adaptation (TA) index
#' @export

computeTemporalAdaptationIndex <- function(sim, cc.params, trim = 1){
  m.hat <- sim$migration.hat
  beta1_hat <- lm(t1~year, data = m.hat[-(1:trim),])$coef[2]
  beta2_hat <- lm(t2~year, data = m.hat[-(1:trim),])$coef[2]
  mean(c(beta1_hat/-cc.params$beta_t, beta2_hat/cc.params$beta_t))
}

#' Computing Total Mismatch
#' 
#' Computes the total mismatch (TE) of a migratory population and a drifting/stochastic resource
#' 
#' @param sim A simulation run from runManyYears.
#' @param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#'  @return a single total mismatch (TE) index
#'  @seealso \link{computeAvgTotalError}, \link{computeMigrationIndices}
#'  @example examples/indices_examples.R
#' @export
#' 
#' 
computeTotalError <- function(sim, world){
  MI <- computeMigrationIndices(sim,world)
  abs(MI$t1.error) + abs(MI$t2.error) + abs(MI$x1.error) + abs(MI$x1.error)
  
}

#' Computing Average Total Mismatch
#' 
#' Computes the average total mismatch (TE) of a migratory population and a drifting/stochastic resource
#' by computing the average of the mismatch for each population year with its respective resource 
#' for the last 20 years of a simulation run.
#' 
#' 
#' @param sim A simulation run from runManyYears.
#' @param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#'  @param par0 Data frame containing resource peak timing and space information of x.peak, t.peak, x.sd
#' and t.sd for each year Generated by the function getCCpars
#'  @return a single average total mismatch (avgTE) index
#'  @seealso \link{getCCpars}, \link{computeTotalError}, \link{computeMigrationIndices}
#'  @example examples/indices_examples.R
#' @export
computeAvgTotalError <- function(sim, world, par0){
  dx <- world$dx
  tau <- world$tau
  average <- data.frame()
  
  if (length(sim) <= 20){
    for(i in 2:length(sim$pop)){
      par <- par0[i,]
      m.hat <- sim$migration.hat[i,]
      if(par["t.peak"] < m.hat$t1 | par["t.peak"] > m.hat$t1 + m.hat$dt1)
        t1.error <- min( abs(par["t.peak"] - m.hat$t1), 
                         abs(par["t.peak"] - m.hat$t1  - m.hat$dt1)) else 
                           t1.error = 0
      
      if(world$tau - par["t.peak"] < m.hat$t2 | 
         world$tau - par["t.peak"] > m.hat$t2 + m.hat$dt2)
        t2.error <- min( abs(world$tau - par["t.peak"] - m.hat$t2), 
                         abs( world$tau - par["t.peak"] - m.hat$t2  - m.hat$dt2)) else 
                           t2.error = 0
      
      x1.error <- par["x.peak"] - m.hat$x1
      x2.error <- - par["x.peak"] - m.hat$x2
      
      error <- abs(t1.error) + abs(t2.error) + abs(x1.error) + abs(x1.error)
      
      average <- rbind(average, error)
    }}
  
  if (length(sim) > 20){
    for(i in length(sim$pop)-20:length(sim$pop)){
      par <- par0[i,]
      m.hat <- sim$migration.hat[i,]
      if(par["t.peak"] < m.hat$t1 | par["t.peak"] > m.hat$t1 + m.hat$dt1)
        t1.error <- min( abs(par["t.peak"] - m.hat$t1), 
                         abs(par["t.peak"] - m.hat$t1  - m.hat$dt1)) else 
                           t1.error = 0
      
      if(world$tau - par["t.peak"] < m.hat$t2 | 
         world$tau - par["t.peak"] > m.hat$t2 + m.hat$dt2)
        t2.error <- min( abs(world$tau - par["t.peak"] - m.hat$t2), 
                         abs( world$tau - par["t.peak"] - m.hat$t2  - m.hat$dt2)) else 
                           t2.error = 0
      
      x1.error <- par["x.peak"] - m.hat$x1
      x2.error <- - par["x.peak"] - m.hat$x2
      
      error <- abs(t1.error) + abs(t2.error) + abs(x1.error) + abs(x1.error)
      
      average <- rbind(average, error)
    }}
  
  mean(average[,1])
  }

  
  
  
  


