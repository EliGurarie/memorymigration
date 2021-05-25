#' Run Many Years 
#' 
#' Based on a migratory population's set up (the World) and the values \code{alpha},
#'\code{beta0} and \code{beta1}, this function determines the population 
#'distribution after several years. 
#'
#' @param World World List of 5: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau,
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 6.
#'  Set up by the getSinePop function.  
#' @param Parameters named vector of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - resource 
#' following coefficient; \code{beta0} - social cohesion coefficient; \code{beta1} - 
#' memory coefficient
#' @param n.years number of years the population migrates 
#' @param threshold the threshold for the social cohesion between two population 
#' distributions from two consecutive years. This is a number between 0 and 1. 
#' @return a list of n.years containing T x X matrices describing the population 
#' distribution for each year after initial population
#' @seealso \link{getSinePop}, \link{runManyYears}
#' @export
#' 
runManyYears <- function(world, parameters, n.years = 20, 
                         threshold= 0.9999, verbose = FALSE){
  cat("\n")
  cat(paste(names(parameters), parameters, collapse = "; "))
  
  pop.list <- list(Year1 = world$pop)
  i <- 1
  pop.list[[i+1]] <- runNextYear(world, Parameters = parameters, 
                                 Pop_lastyear = world$pop, Year = 1)
  similarity <- computeEfficiency(pop.list[[i]], pop.list[[i+1]], world)
  
  while((similarity < threshold) & (i < n.years)){
    if(verbose){cat("\n"); cat(paste("running year ", i))}
    
    i <- i+1
    pop.list[[i+1]] <- runNextYear(world = world, 
                                   Parameters = parameters, 
                                   Pop_lastyear = pop.list[[i]], 
                                   Year = i)
    similarity <- computeEfficiency(pop.list[[i]], pop.list[[i+1]], world)
  }
  names(pop.list) <- paste0("Year",0:(length(pop.list)-1))
  attr(pop.list, "parameters") <- parameters
  return(pop.list)
}

#' Run Many Years for a set of parameters
#' 
#' Based on a migratory population's set up (the World) and the values \code{epsilon}, \code{alpha},
#'\code{beta0} and \code{beta1}, this function determines the population 
#'distribution after several years. 
#'
#'
#'@param parameters.df list of data frames with values of \code{epsilon}, \code{alpha},
#'\code{beta0} and \code{beta1} to test
#'@param world World List of 5: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau,
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 6.
#'  Set up by the getSinePop function.  
#' @param filename string containing base name of the file to be created 
#' @param results.dir string containing directory of where results from the R scripts will be stored
#' @export
#' 
runManyRuns <- function (parameters.df, resource_param, world, resource, filename = NULL, results.dir = NULL, ...) 
{
  newresults <- data.frame()
  
  if(resource == "drifting"){
  for (i in 1:nrow(parameters.df)) {
    for(j in 1:nrow(resource_param)){
      par0 <- getCCpars(mu_x0 = resource_param[j,1], 
                        mu_t0 = resource_param[j,2],
                        beta_x = resource_param[j,3],
                        beta_t = resource_param[j,4],
                        n.years = resource_param[j,5],
                        sigma_x = resource_param[j,6],
                        sigma_t = resource_param[j,7])
      
      Resource.CC <- aaply(par0, 1, function(p) getPulsedResource(world, p))
      world$resource <- Resource.CC
      M <- try(runManyYears(world, parameters <- c(epsilon = parameters.df[i,1], 
                                                   alpha = parameters.df[i,2],
                                                   beta = parameters.df[i,3],
                                                   kappa = parameters.df[i,4],
                                                   lambda = parameters.df[i,5]), 
                            n.years = 20, threshold = 0.9999))
      if(!inherits(M, "try-error")){
        myR <- data.frame(parameters.df[i, ], computeIndices(M[[length(M)]], 
                                                             world$resource[length(M)-1,,], world), 
                          n.runs = length(M) - 1,
                          final_similarity = computeEfficiency(M[[length(M)-1]], 
                                                               M[[length(M)]], world), resource_param,
                          resource = resource)
        newresults <- rbind(newresults, c(myR))
    }
    if(!is.null(results.dir) & (i %% 10 == 0 | i == max(i)))  
      save(newresults, file =paste0("~/Rprojects/memorymigration/",results.dir,"/",filename,".rda"))
  }}
  return(newresults) }
  
  if(resource == "island"){
    for (i in 1:nrow(parameters.df)) {
      for(j in 1:nrow(resource_param)){
        par0 <- getCCpars(mu_x0 = resource_param[j,1], 
                          mu_t0 = resource_param[j,2],
                          beta_x = resource_param[j,3],
                          beta_t = resource_param[j,4],
                          n.years = resource_param[j,5],
                          sigma_x = resource_param[j,6],
                          sigma_t = resource_param[j,7])
        
        Resource.CC <- aaply(par0, 1, function(p) getPulsedResource_v2(world, p))
        world$resource <- Resource.CC
        M <- try(runManyYears(world, parameters <- c(epsilon = parameters.df[i,1], 
                                                     alpha = parameters.df[i,2],
                                                     beta = parameters.df[i,3],
                                                     kappa = parameters.df[i,4],
                                                     lambda = parameters.df[i,5]), 
                              n.years = 20, threshold = 0.9999))
        if(!inherits(M, "try-error")){
          myR <- data.frame(parameters.df[i, ], computeIndices(M[[length(M)]], 
                                                               world$resource[length(M)-1,,], world), 
                            n.runs = length(M) - 1,
                            final_similarity = computeEfficiency(M[[length(M)-1]], 
                                                                 M[[length(M)]], world), resource_param)
          newresults <- rbind(newresults, c(myR))
        }
        if(!is.null(results.dir) & (i %% 10 == 0 | i == max(i)))  
          save(newresults, file =paste0("~/Rprojects/memorymigration/",results.dir,"/",filename,".rda"))
      }}
    return(newresults) }
  
  
}

