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
runNextYear <- function(World, Parameters, Pop_lastyear, Year){
  pop0 <- World$pop
  pop2 <- pop0*0
  
  Time <- World$time
  Resource <- World$resource
  
  w0 <- Parameters["gamma"]^(Year-1)
  memory <- w0 * pop0 + (1-w0) * Pop_lastyear
  
  for(t in Time){
    
    if(t == 1) pop_now <- Pop_lastyear[nrow(Pop_lastyear),] else 
      pop_now <- pop2[t-1,]
    
    pop2[t,] <- ode(y = pop_now, 
                    times = 0:1, 
                    parms = Parameters,
                    func = ForagingMemoryModel, 
                    resource = c(0,Resource[t,],0),
                    memory = c(0, memory[t,], 0),
                    dx = World$dx)[2,1:ncol(pop2)+1]
  }
  pop2
}

runNextYear_v0 <- function(World, Parameters, Pop_lastyear, Year){
  pop0 <- World$pop
  pop2 <- pop0*0
  
  Time <- World$time
  Resource <- World$resource
  
  w0 <- Parameters["gamma"]^(Year-1)
  memory <- w0 * pop0 + (1-w0) * Pop_lastyear
  
  for(t in Time){
    
    if(t == 1) pop_now <- Pop_lastyear[nrow(Pop_lastyear),] else 
      pop_now <- pop2[t-1,]
    
    pop2[t,] <- ode(y = pop_now, 
                    times = 0:1, 
                    parms = Parameters,
                    func = ForagingMemoryModel, 
                    resource = c(0,Resource[t,],0),
                    memory = c(0, memory[t,], 0),
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
runManyYears <- function(World, parameters, n.years = 60, 
                         threshold= 0.995, verbose = FALSE){
  
  cat("\n")
  cat(paste(names(parameters), parameters, collapse = "; "))
  
  
  pop.list <- list(Year1 = World$pop)
  i <- 1
  
  pop.list[[i+1]] <- runNextYear(World, Parameters = parameters, 
                                 Pop_lastyear = World$pop, Year = 1)
  similarity <- computeEfficiency(pop.list[[i]], pop.list[[i+1]], World)
  
  
  while((similarity < threshold) & (i < n.years)){
    if(verbose){cat("\n"); cat(paste("running year ", i))}
    
    i <- i+1
    pop.list[[i+1]] <- runNextYear(World, 
                                   Parameters = parameters, 
                                   Pop_lastyear = pop.list[[i]], 
                                   Year = i)
    similarity <- computeEfficiency(pop.list[[i]], pop.list[[i+1]], World)
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
runManyRuns <- function (parameters.df, world, filename = NULL, results.dir = NULL, ...) 
{
  newresults <- data.frame()
  for (i in 1:nrow(parameters.df)) {
    M <- try(runManyYears(world, parameters = parameters.df[i,], n.years = 60, threshold = 0.995))
    if(!inherits(M, "try-error")){
      myR <- data.frame(parameters.df[i, ], computeIndices(M[[length(M)]], 
                                                           world$resource, world), 
                        n.runs = length(M) - 1,
                        final_similarity = computeEfficiency(M[[length(M)-1]], 
                                                             M[[length(M)]], world))
      newresults <- rbind(newresults, c(myR))
    }
    if(!is.null(results.dir) & (i %% 10 == 0 | i == max(i)))  
      save(newresults, file =paste0("~/Rprojects/memorymigration/",results.dir,"/",filename, "_",
                                    parameters.df[1,2], ".rda"))
  }
  return(newresults)
}

