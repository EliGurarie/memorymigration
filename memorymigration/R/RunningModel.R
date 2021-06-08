#' Run Many Years 
#' 
#' Based on a migratory population's set up (the World) and the values \code{alpha},
#'\code{beta0} and \code{beta1}, this function determines the population 
#'distribution after several years. 
#'
#' @param World world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @param Parameters named vector of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - resource 
#' following coefficient; \code{beta0} - social cohesion coefficient; \code{beta1} - 
#' memory coefficient
#' @param n.years number of years the population migrates 
#' @param threshold the threshold for the social cohesion between two population 
#' distributions from two consecutive years. This is a number between 0 and 1. 
#' @return a list of n.years containing T x X matrices describing the population 
#' distribution for each year after initial population
#' @seealso \link{getSinePop},\link{getOptimalPop}, \link{runNextYear}
#'  @example examples/indices_examples.R
#' @export
#' 
runManyYears <- function(world, parameters, n.years = 20, 
                         threshold= 0.9999, verbose = FALSE, 
                         FUN = runNextYear){
  cat("\n")
  cat(paste(names(parameters), parameters, collapse = "; "))
  
  pop.list <- list(Year1 = world$pop)
  i <- 1
  pop.list[[i+1]] <- FUN(world, Parameters = parameters, 
                                 Pop_lastyear = world$pop, Year = 1)
  similarity <- computeEfficiency(pop.list[[i]], pop.list[[i+1]], world)
  
  while((similarity < threshold) & (i < n.years)){
    if(verbose){cat("\n"); cat(paste("running year ", i))}
    
    i <- i+1
    pop.list[[i+1]] <- FUN(world = world, 
                                   Parameters = parameters, 
                                   Pop_lastyear = pop.list[[i]], 
                                   Year = i)
    similarity <- computeEfficiency(pop.list$pop[[i]], pop.list$pop[[i+1]], world)
  }
  names(pop.list) <- paste0("Year",0:(length(pop.list)-1))
  attr(pop.list, "parameters") <- parameters
  return(pop.list)
}

#' Run Many Runs for a set of parameters
#' 
#' Based on a migratory population's set up (the World) and the values \code{alpha},
#'\code{beta0} and \code{beta1}, this function determines the population 
#'distribution after several years for many runs of the population and the resource.
#'
#'
#'@param parameters.df list of data frames with values of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - resource following coefficient; \code{beta} - spatial scale of sociality; \code{kappa} - 
#' memory following coefficient; \code{lambda} - maximum speed
#'@param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function.  
#' @param filename string containing base name of the file to be created 
#' @param results.dir string containing directory of where results from the R scripts will be stored
#' @export
#' 
runManyRuns <- function (parameters.df, resource_param, world, resource, 
                         filename = NULL, results.dir = NULL, ...) {
  newresults <- data.frame()
  
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
      
      if(resource == "drifting")
      Resource.CC <- aaply(par0, 1, function(p) getResource_drifting(world, p)) 
      if(resource == "island")
      Resource.CC <- aaply(par0, 1, function(p) getResource_island(world, p)) 
      
      
      world$resource <- Resource.CC
      
      myparams <- with(parameters.df[i,], 
                       c(epsilon = epsilon, 
                         alpha = alpha, 
                         beta = beta, 
                         kappa = kappa, 
                         lambda = lambda))
      
      M <- try(runManyYears(world, parameters = myparams, 
                            n.years = 20, threshold = 0.9999))
      
      if(!inherits(M, "try-error")){
        myR <- data.frame(parameters.df[i, ], computeIndices(M[[length(M)]], 
                                                             world$resource[length(M)-1,,], world), 
                          avgFE = computeAvgEfficiency(M, world$resource, world),
                          n.runs = length(M) - 1,
                          final_similarity = computeEfficiency(M[[length(M)-1]], 
                                                               M[[length(M)]], world), 
                          resource_param[j,],
                          resource = resource)
        newresults <- rbind(newresults, c(myR))
      }
      if(!is.null(results.dir) & (i %% 10 == 0 | i == max(i)))  
        save(newresults, file =paste0(results.dir,"/",filename,".rda"))
    }}
  return(newresults) 
}


#' @export
buildOnRuns <- function(M, world, ...){
  world$pop <- M[[length(M)]]
  M2 <- runManyYears(world, threshold = 1, ...)
  M2[[1]] <- NULL
  M3 <- c(M, M2)
  names(M3) <- paste0("Year",1:length(M3))
  M3
}
