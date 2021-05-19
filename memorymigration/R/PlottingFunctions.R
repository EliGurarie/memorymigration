#' Plot Year List
#' 
#' This function plots the migratoriness population distribution over a time period. 
#' 
#' @param yearlist a list of T x X matrices describing the population 
#' distribution for each year after initial population; produced by the runManyYear function
#' @param World List of 5: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau,
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 6.
#'  Set up by the getSinePop function. 
#' @param X.max maximum value of X-axis
#' @param tau maximum value of time T
#' @param log logical;
#' @param persp whether to plot an image / contour or perspective plot
#' @seealso \link{runManyYears}; \link{printParameters}
#' @example examples/plotting_examples.R
#' @export
#' 
plotYearList <- function(yearlist, World, X.max = 100, tau = 360, log = FALSE, 
                         persp = FALSE, ...){
  
  n.years <- length(yearlist)
  allyears <- do.call(rbind, yearlist)
  tau <- World$tau
  
  if(log) allyears <- log(allyears)
  
  time <- 1:nrow(allyears)
  x <- seq(0,X.max, length = ncol(allyears))
  
  if(!persp){
    image(time, x, allyears)
    contour(time, x, allyears, add = TRUE)  
    mtext(side = 3, at = (1:n.years)*tau - tau/2, text = paste("Year", 1:n.years-1))
    abline(v = tau*1:(n.years-1), col = "grey") 
  } else 
    mypersp(time, x, allyears, ...) 
}

#' Print Parameters 
#' 
#' This function prints the parameters of the diffusion-advection equation used 
#' on the model on the plot of the population distribution set up by the plotYearList function.
#' 
#'  @param p List of 5 a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau,
#'  the dx value and the tau value. Set up by the getSinePop function. 
#'  @return displays the values of the parameters on the plot which are:
#'   \code{epsilon} - diffusion coefficient; \code{alpha} - resource 
#' following coefficient; \code{beta0} - social cohesion coefficient; \code{beta1} - 
#' memory coefficient
#'  @seealso \link{PlotYearList}; \link{getSinePop}
#'  
printParameters <- function(p) {
  parse(text = paste0("list(",paste(names(p), p, sep = "==", collapse = ", "),")"))
}

#' My Persp 
#' 
#' This function...  
mypersp <- function(x,y,z,...)
  persp(x,y,z, theta=45, phi=45, bor=NA, shade=TRUE, ylab = "x", xlab="time", zlab="population", ...)

#' Plotting simulation results
#' @export

plotManyRuns <- function(sim, world, years = NULL, nrow = 1, outer = TRUE, 
                         labelyears = FALSE, 
                         par = NULL, ylab = "", ...){
  
  require(gplots)
  
  if(is.null(years)){
    years <- 1:length(sim)
    n <- length(years)
    zmax <- max(sapply(sim, max))
  } else{
    zmax <- max(sapply(sim, max)[paste0("Year",c(0,years[-length(years)]))])
    n <- length(years)
  }
 
  parameters <- attributes(sim)$parameter
  if(is.null(par))
    par(mfrow = c(nrow,ceiling(n/nrow)), mar = c(1,0,1,0), oma = c(2,2,4,2), tck = 0.01)
  
  for(i in years){
    image(1:world$tau, world$X, sim[[i]], 
          breaks = seq(0, zmax, length = 100), col = rich.colors(99),
          yaxt = "n", xaxt = "n", ylab = "", xlab = "")
    if(labelyears) title(paste("year", i-1), line = 0.3)
    if(i == 1) mtext(side = 2, ylab, ...)
  }
  if(outer) 
    title(outer = TRUE, paste(names(parameters),"=",parameters, collapse = "; "))
}
