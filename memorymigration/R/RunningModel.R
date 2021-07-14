
#' Run Many Years with Stabilization
#' 
#' A simulation run to reach a until a quasi-equilibrium
#' state was achieved, i.e. where the Bhattacharya index of the population distribution
#' across subsequent years reached a value of 1-1e-5
#' 
#' @param World world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @param Parameters named vector of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - forage following coefficient; \code{beta} - strength of sociality; \code{lambda} - 
#' spatial sclae of sociality; \code{kappa} - propotion of reference versus working memory
#' @param n.years number of years the population migrates 
#' @param threshold the threshold for the social cohesion between two population 
#' distributions from two consecutive years. This is a number between 0 and 1. 
#' @param m0 the inital migration estimates for the population; created by fitMigration function
#' @param n.years.null number of years the population should stabilize for 
#' @param resource.null Data frame containing the stable resource peak timing and space
#'  information of x.peak, t.peak, x.sd and t.sd
#' @return a list of n.years containing T x X matrices describing the population 
#' distribution for each year after initial population
#' @seealso \link{getSinePop},\link{getOptimalPop}, \link{runNextYear}, \link{runManyYears}, \link{fitMigration}
#' @example examples/creatingClimateChangeWorlds.R
#'  @export

runManyYearsWithStabilization <- function(world, parameters, 
                                          n.years = dim(world$resource)[1], 
                                          threshold = threshold, 
                                          verbose = FALSE, m0 = world$m0,
                                          n.years.null = 20, resource.null){
  
  # run to stabilizatoin
  cat("Stabilization run:\n")
  world.null <- world
  world.null$resource <- resource.null
  parameters.null <- parameters
  parameters.null['kappa'] <- 0
  sim.null <- runManyYears(world.null, parameters = parameters.null, 
                           n.years = n.years.null, threshold = threshold, 
                           verbose = verbose, m0 = world$m0)
  
  # extract pieces to run for the remainder
  
  cat("Dynamic resource run:\n")
  
  world$m0 <-  (sim.null$migration.hat[nrow(sim.null$migration.hat),1:6] %>% 
                  as.matrix)[1,]
  world$pop <- sim.null$pop[[length(sim.null$pop)]]
  sim <- runManyYears(world, parameters = parameters, 
                      n.years = n.years, threshold = 1, 
                      verbose = verbose, m0 = world$m0)
  return(sim)
}


#' Run Many Years 
#' 
#' Based on a migratory population's set up (the World) and the values \code{alpha},
#'\code{beta}, \code{epsilon}, \code{lambda} and \code{kappa} this function determines the population 
#'distribution after several years. 
#'
#' @param World world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @param Parameters named vector of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - forage following coefficient; \code{beta} - strength of sociality; \code{lambda} - 
#' spatial sclae of sociality; \code{kappa} - propotion of reference versus working memory
#' @param n.years number of years the population migrates 
#' @param threshold the threshold for the social cohesion between two population 
#' distributions from two consecutive years. This is a number between 0 and 1. 
#' @param m0 the inital migration estimates for the population; created by fitMigration function
#' @return a list of n.years containing T x X matrices describing the population 
#' distribution for each year after initial population
#' @seealso \link{getSinePop},\link{getOptimalPop}, \link{runNextYear}
#'  @example examples/indices_examples.R
#' @export
#' 


runManyYears <- function(world, parameters, n.years = 20, 
                         threshold= 0.9999, verbose = FALSE, 
                         m0 = world$m0){
  cat("\n")
  cat(paste(names(parameters), parameters, collapse = "; "))
  
  migration.list <- list(m0)
  memory.list <- list(m0)
  
  pop.list <- list(world$pop)
  similarity <- 0
  i <- 1 
  while((similarity < threshold) & (i < n.years)){
    if(verbose){cat("\n"); cat(paste("running year ", i))}
    i <- i+1
    pop.list[[i]] <- runFWM(world = world, parameters = parameters, 
                            pop.lastyear = pop.list[[length(pop.list)]], 
                            Year = i, m.hat = memory.list[[i-1]])
    
    # m.hat.lastyear  <- migration.list[[i-1]]
    if(abs(migration.list[[i-1]]["x2"] - migration.list[[i-1]]["x1"]) < 1){
      migration.list[[i]] <- migration.list[[i-1]]
    } else{
      
      m.hat <- try(fitMigration(t = world$time, x = getMem(pop.list[[i]], world), 
                                m.start = migration.list[[i-1]]))
      if(inherits(m.hat, "try-error")){
        cat("\nRetry migration fitting 1")
        m.hat <- try(fitMigration(t = world$time, x = getMem(pop.list[[i]], world), 
                                  lower = c(-100,-20,0,0,-100,-100), 
                                  m.start = migration.list[[i-1]]))
      }
      if(inherits(m.hat, "try-error")){
        cat("\nRetry migration fitting 2")
        m.hat <- try(fitMigration(t = world$time, x = getMem(pop.list[[i]], world), 
                                  lower = c(-100,-50,0,-20,-100,-100), 
                                  m.start = migration.list[[i-1]]))
      }
      if(inherits(m.hat, "try-error")) stop(cat("\n We tried, and failed at year", i)) else
        migration.list[[i]] <- m.hat
      
      if(migration.list[[i]]["dt1"] < 0){
        migration.list[[i]]["dt1"] <- 0
        warning("\nnegative dt1 estimated!!!  coerced to 0")
      }
      if(migration.list[[i]]["dt2"] < 0){
        migration.list[[i]]["dt2"] <- 0
        warning("\nnegative dt2 estimated!!!  coerced to 0")
      }
    }
    memory.list[[i]] <- parameters["kappa"]^(i-1) * world$m0 + 
      (1 - parameters["kappa"]^(i-1)) * migration.list[[i]]
    
    similarity <- computeEfficiency(pop.list[[i-1]], pop.list[[i]], world)
  }
  names(pop.list) <- paste0("Year",0:(length(pop.list)-1))
  attr(pop.list, "parameters") <- parameters
  
  migration.hat <- ldply(migration.list, .id = "year") %>% mutate(year = 1:length(pop.list)-1)
  memory.hat <- ldply(memory.list, .id = "year") %>% mutate(year = 1:length(pop.list)-1)
  final <- list(pop = pop.list, migration.hat = migration.hat, memory.hat = memory.hat)
  final
}

#' @export
#' 
buildOnRuns <- function(M, world, ...){
  world$pop <- M$pop[[length(M$pop)]]
  
  M2 <- runManyYears(world, threshold = 1, m0 = M0$memory.hat[1,1:6], ...)
  M2$pop[[1]] <- NULL
  
  M3 <- list()
  M3$pop <- c(M$pop, M2$pop)
  
  M3$migration.hat <- rbind(M$migration.hat, M2$migration.hat[-1,]) %>% 
    mutate(year = 1:length(M3$pop)-1)
  
  M3$memory.hat <- rbind(M$memory.hat, M2$memory.hat[-1,]) %>% 
    mutate(year = 1:length(M3$pop)-1)
  
  names(M3$pop) <- paste0("Year",1:length(M3$pop)-1)
  M3
}


#' Run Many Runs for a set of parameters
#' 
#' Based on a migratory population's set up (the World) and the values \code{alpha},
#'\code{beta0} and \code{beta1}, this function determines the population 
#'distribution after several years for many runs of the population and the resource.
#'
#'
#'@param parameters.df data frame with combination values of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - forage following coefficient; \code{beta} - strength of sociality; \code{lambda} - 
#' spatial sclae of sociality; \code{kappa} - propotion of reference versus working memory
#' @param resource_param data frame with combination values of parameters for resource. These values will be passed through 
#' the getCCPars function. These are mu_x0 (spatial coordinate of resource peark for summer), mu_t0 (timing of 
#' resource peak for the summer), beta_x (rate of change of peak location), beta_t (rate of change of peak timing), 
#' n.years (how many years the resource should be created), sigma_x (spatial scale of resource pulse), 
#' sigma_t (time duration of resource pulse), psi_x (standard deviation of peak location),  
#' psi_t (standard deviation of peak timing), n.years.null (how many years the resource remains stable i.e.
#' no drift or stochasticity).
#'@param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#'  @param resource string of either "island" or "drifting" to indicate the type of resource 
#' @param filename string containing base name of the file to be created 
#' @param results.dir string containing directory of where results from the R scripts will be stored
#' @return data frame of indices and information about each simulation run
#' @export
#' 
#' 
runManyRuns <- function (parameters.df, resource_param, world, resource, 
                         filename = NULL, results.dir = NULL, ...) {
  newresults <- data.frame()
  FE.matrix <- matrix(NA, nrow=nrow(parameters.df)*nrow(resource_param), ncol = resource_param$n.years.null[1]+resource_param$n.years[1])
  
  for(i in 1:nrow(parameters.df)) {
    for(j in 1:nrow(resource_param)){
      par0 <- with(resource_param, 
                   getCCpars(mu_x0 = mu_x0[j],
                             mu_t0 = mu_t0[j],
                             beta_x = beta_x[j],
                             beta_t = beta_t[j],
                             n.years = n.years[j],
                             sigma_x = sigma_x[j],
                             sigma_t = sigma_t[j],
                             psi_x = psi_x[j], 
                             psi_t = psi_t[j],
                             n.years.null = n.years.null[j]))
      
      if(resource == "drifting")
        world$resource <- aaply(par0, 1, function(p) getResource_drifting(world, p, x.null=50)) 
      if(resource == "island")
        world$resource <- aaply(par0, 1, function(p) getResource_island(world, p)) 
      
      
      attr(world$resource, "par") <- par0[nrow(par0),]
      
      myparams <- with(parameters.df[i,], 
                       c(epsilon = epsilon, 
                         alpha = alpha, 
                         beta = beta, 
                         kappa = kappa, 
                         lambda = lambda))
      
      M <- try(runManyYears(world, parameters = myparams, 
                            n.years = 30, threshold = 1))
      
      if(!inherits(M, "try-error")){
        myFE <- computeAnnualEfficiency(M$pop, world$resource, world)
        FE.matrix[nrow(newresults)+1, 1:length(myFE)] <- myFE
        
        myR <- data.frame(parameters.df[i, ], computeIndices(M$pop[[length(M$pop)]], 
                                                             world$resource[length(M$pop)-1,,], world),
                          computeMigrationIndices(M, world),
                          avgFE = computeAvgEfficiency(M$pop, world$resource, world),
                          TE = computeTotalError(M, world),
                          avgTE = computeAvgTotalError(M, world, par0),
                          n.runs = length(M$pop) - 1,
                          final_similarity = computeEfficiency(M$pop[[length(M$pop)-1]], 
                                                               M$pop[[length(M$pop)]], world), 
                          resource_param[j,],
                          resource = resource)
        if(resource_param[j,]$beta_x != 0){ 
          myR$SAI_total <- computeSpatialAdaptationIndex(M, resource_param[j,])
          myR$SAI_recent <- computeSpatialAdaptationIndex(M, resource_param[j,], trim = 10)
        } else {myR$SAI_total <- NA
        myR$SAI_recent <- NA}
        if(resource_param[j,]$beta_t != 0){ 
          myR$TAI_total <- computeTemporalAdaptationIndex(M, resource_param[j,])
          myR$TAI_recent <- computeTemporalAdaptationIndex(M, resource_param[j,], trim = 10)
        } else {myR$TAI_total <- NA
        myR$TAI_recent <- NA}
        
        
        newresults <- rbind(newresults, c(myR))
      }
      
      if(!is.null(results.dir) & (i %% 10 == 0 | i == max(i)))  
        save(newresults, file =paste0(results.dir,"/",filename,".rda"))
    }}
  newresults$annualFE <- FE.matrix[1:nrow(newresults),]
  return(newresults) 
}



#' Run Many Runs Resource
#' 
#' Running many runs of simulation that starts with an optimal initial population state which matches the resource
#'@param parameters.df data frame with combination values of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - forage following coefficient; \code{beta} - strength of sociality; \code{lambda} - 
#' spatial sclae of sociality; \code{kappa} - propotion of reference versus working memory
#' @param resource_param data frame with combination values of parameters for resource. These values will be passed through 
#' the getCCPars function. These are mu_x0 (spatial coordinate of resource peark for summer), mu_t0 (timing of 
#' resource peak for the summer), beta_x (rate of change of peak location), beta_t (rate of change of peak timing), 
#' n.years (how many years the resource should be created), sigma_x (spatial scale of resource pulse), 
#' sigma_t (time duration of resource pulse), psi_x (standard deviation of peak location),  
#' psi_t (standard deviation of peak timing), n.years.null (how many years the resource remains stable i.e.
#' no drift or stochasticity).
#'@param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#'  @param resource string of either "island" or "drifting" to indicate the type of resource 
#' @param filename string containing base name of the file to be created 
#' @param results.dir string containing directory of where results from the R scripts will be stored
#' @return data frame of indices and information about each simulation run
#' 
#' @export
#' 
runManyRuns_res <- function (world_param, parameters.df, resource_param, world, resource, 
                             filename = NULL, results.dir = NULL, ...) {
  newresults <- data.frame()
  FE.matrix <- matrix(NA, nrow=nrow(parameters.df)*nrow(resource_param), ncol = resource_param$n.years[1])
  
  for(i in 1:nrow(parameters.df)) {
    for(j in 1:nrow(resource_param)){
      par0 <- with(resource_param, 
                   getCCpars(mu_x0 = mu_x0[j],
                             mu_t0 = mu_t0[j],
                             beta_x = beta_x[j],
                             beta_t = beta_t[j],
                             n.years = n.years[j],
                             sigma_x = sigma_x[j],
                             sigma_t = sigma_t[j],
                             psi_x = psi_x[j], 
                             psi_t = psi_t[j]))
      
      world <- with(world_param, getOptimalPop(tau = tau, X.min = X.min,
                                               X.max = X.max, dx = dx,
                                               x1 = x1, x2 = x2, t.peak = t.peak,
                                               x.sd = resource_param$sigma_x[j],
                                               t.sd = resource_param$sigma_t[j]))
      
      world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
      
      if(resource == "drifting")
        world$resource <- aaply(par0, 1, function(p) getResource_drifting(world, p, x.null=50)) 
      if(resource == "island")
        world$resource <- aaply(par0, 1, function(p) getResource_island(world, p)) 
      
      attr(world$resource, "par") <- par0[nrow(par0),]
      myparams <- with(parameters.df[i,], 
                       c(epsilon = epsilon, 
                         alpha = alpha, 
                         beta = beta, 
                         kappa = kappa, 
                         lambda = lambda))
      
      M <- try(runManyYears(world, parameters = myparams, 
                            n.years = 100, threshold = 0.9999))
      
      if(!inherits(M, "try-error")){
        myFE <- computeAnnualEfficiency(M$pop, world$resource, world)
        FE.matrix[nrow(newresults)+1, 1:length(myFE)] <- myFE
        
        myR <- data.frame(parameters.df[i, ], computeIndices(M$pop[[length(M$pop)]], 
                                                             world$resource[length(M$pop)-1,,], world),
                          computeMigrationIndices(M, world),
                          avgFE = computeAvgEfficiency(M$pop, world$resource, world),
                          n.runs = length(M$pop) - 1,
                          final_similarity = computeEfficiency(M$pop[[length(M$pop)-1]], 
                                                               M$pop[[length(M$pop)]], world), 
                          
                          resource_param[j,],
                          resource = resource)
        newresults <- rbind(newresults, c(myR))
      }
      
      if(!is.null(results.dir) & (i %% 10 == 0 | i == max(i)))  
        save(newresults, file =paste0(results.dir,"/",filename,".rda"))
    }}
  newresults$annualFE <- FE.matrix[1:nrow(newresults),]
  return(newresults) 
}

#' Run Missed Runs
#' 
#' @export

runMissedRuns <- function (parameters.df, resource_param, world, resource, 
                           filename = NULL, results.dir = NULL, ...) {
  newresults <- data.frame()
  FE.matrix <- matrix(NA, nrow=nrow(parameters.df)*nrow(resource_param), ncol = resource_param$n.years[1])
  
  for(i in 1:nrow(parameters.df)) {
    par0 <- with(resource_param, 
                 getCCpars(mu_x0 = mu_x0[i],
                           mu_t0 = mu_t0[i],
                           beta_x = beta_x[i],
                           beta_t = beta_t[i],
                           n.years = n.years[i],
                           sigma_x = sigma_x[i],
                           sigma_t = sigma_t[i],
                           psi_x = psi_x[i], 
                           psi_t = psi_t[i]))
    
    
    if(resource == "drifting")
      world$resource <- aaply(par0, 1, function(p) getResource_drifting(world, p, x.null=50)) 
    if(resource == "island")
      world$resource <- aaply(par0, 1, function(p) getResource_island(world, p)) 
    
    
    attr(world$resource, "par") <- par0[nrow(par0),]
    
    myparams <- with(parameters.df[i,], 
                     c(epsilon = epsilon, 
                       alpha = alpha, 
                       beta = beta, 
                       kappa = kappa, 
                       lambda = lambda))
    
    M <- try(runManyYears(world, parameters = myparams, 
                          n.years = 150, threshold = 0.9999))
    
    
    
    if(!inherits(M, "try-error")){
      myFE <- computeAnnualEfficiency(M$pop, world$resource, world)
      FE.matrix[nrow(newresults)+1, 1:length(myFE)] <- myFE
      
      myR <- data.frame(parameters.df[i, ], computeIndices(M$pop[[length(M$pop)]], 
                                                           world$resource[length(M$pop)-1,,], world),
                        computeMigrationIndices(M, world),
                        avgFE = computeAvgEfficiency(M$pop, world$resource, world),
                        n.runs = length(M$pop) - 1,
                        final_similarity = computeEfficiency(M$pop[[length(M$pop)-1]], 
                                                             M$pop[[length(M$pop)]], world), 
                        
                        resource_param[i,],
                        resource = resource)
      
      newresults <- rbind(newresults, c(myR))
    }
    
    if(!is.null(results.dir) & (i %% 10 == 0 | i == max(i)))  
      save(newresults, file =paste0(results.dir,"/",filename,".rda"))
  }
  newresults$annualFE <- FE.matrix[1:nrow(newresults),]
  return(newresults) 
}

#' Run Missed Runs Resource
#' 
#' @export

runMissedRuns_res <- function (world_param, parameters.df, resource_param, world, resource, 
                               filename = NULL, results.dir = NULL, ...) {
  newresults <- data.frame()
  FE.matrix <- matrix(NA, nrow=nrow(parameters.df)*nrow(resource_param), ncol = resource_param$n.years[1])
  
  for(i in 1:nrow(parameters.df)) {
    par0 <- with(resource_param, 
                 getCCpars(mu_x0 = mu_x0[i],
                           mu_t0 = mu_t0[i],
                           beta_x = beta_x[i],
                           beta_t = beta_t[i],
                           n.years = n.years[i],
                           sigma_x = sigma_x[i],
                           sigma_t = sigma_t[i],
                           psi_x = psi_x[i], 
                           psi_t = psi_t[i]))
    
    
    world <- with(world_param, getOptimalPop(tau = tau, X.min = X.min,
                                             X.max = X.max, dx = dx,
                                             x1 = x1, x2 = x2, t.peak = t.peak,
                                             x.sd = resource_param$sigma_x[i],
                                             t.sd = resource_param$sigma_t[i]))
    
    world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
    
    if(resource == "drifting")
      world$resource <- aaply(par0, 1, function(p) getResource_drifting(world, p, x.null=50)) 
    if(resource == "island")
      world$resource <- aaply(par0, 1, function(p) getResource_island(world, p)) 
    
    
    attr(world$resource, "par") <- par0[nrow(par0),]
    
    myparams <- with(parameters.df[i,], 
                     c(epsilon = epsilon, 
                       alpha = alpha, 
                       beta = beta, 
                       kappa = kappa, 
                       lambda = lambda))
    
    M <- try(runManyYears(world, parameters = myparams, 
                          n.years = 100, threshold = 0.9999))
    
    
    
    if(!inherits(M, "try-error")){
      myFE <- computeAnnualEfficiency(M$pop, world$resource, world)
      FE.matrix[nrow(newresults)+1, 1:length(myFE)] <- myFE
      
      myR <- data.frame(parameters.df[i, ], computeIndices(M$pop[[length(M$pop)]], 
                                                           world$resource[length(M$pop)-1,,], world),
                        computeMigrationIndices(M, world),
                        avgFE = computeAvgEfficiency(M$pop, world$resource, world),
                        TE = computeTotalError(M, world),
                        avgTE = computeAvgTotalError(M, world, par0),
                        n.runs = length(M$pop) - 1,
                        final_similarity = computeEfficiency(M$pop[[length(M$pop)-1]], 
                                                             M$pop[[length(M$pop)]], world), 
                        
                        resource_param[i,],
                        resource = resource)
      
      newresults <- rbind(newresults, c(myR))
    }
    
    if(!is.null(results.dir) & (i %% 10 == 0 | i == max(i)))  
      save(newresults, file =paste0(results.dir,"/",filename,".rda"))
  }
  newresults$annualFE <- FE.matrix[1:nrow(newresults),]
  return(newresults) 
}