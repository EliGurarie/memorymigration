#' Plot Year List
#' 
#' This function plots the migratoriness population distribution over a time period. 
#' 
#' @param yearlist a list of T x X matrices describing the population 
#' distribution for each year after initial population; produced by the runManyYear function
#' @param World List of 5: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau,
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make 
#'  a list of 6.  Set up by the getSinePop function. 
#' @param X.max maximum value of X-axis
#' @param tau maximum value of time T
#' @param log logical;
#' @param persp whether to plot an image / contour or perspective plot
#' @seealso \link{runManyYears}; \link{printParameters}
#' @example examples/plotting_examples.R
#' @export
#' 
plotYearList <- function(yearlist, world, X.max = 100, tau = 360, log = FALSE, 
                         persp = FALSE, ...){
  
  n.years <- length(yearlist)
  allyears <- do.call(rbind, yearlist)
  tau <- world$tau
  
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
#'  @param p world object; list of 7: a population distribution across the time period in a 
#'  T x X matrix, a vector with midpoint X-values, the time points for the population as integers 
#'  1:tau, the minimum value of population distribution (X.min), the maximum value of population 
#'  distribution (X.max), the dx value and the tau value. Can incorporate resource attribute into 
#'  the world to make a list of 8.  Set up by the getSinePop/getOptimal function. 
#'  @return displays the values of the parameters on the plot which are:
#'   \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - resource following coefficient; \code{beta} - spatial scale of sociality; \code{kappa} - 
#' memory following coefficient; \code{lambda} - maximum speed
#'  @seealso \link{plotMemories}; \link{getSinePop}; \link{getOptimalPop}
#'  @example examples/plotting_examples.R
#'  @export
#'  
printParameters <- function(p) {
  parse(text = paste0("list(",paste(names(p), p, sep = "==", collapse = ", "),")"))
}

#' My Persp 
#' 
#' This function...  
mypersp <- function(x,y,z,...)
  persp(x,y,z, theta=45, phi=45, bor=NA, shade=TRUE, ylab = "x", xlab="time", zlab="population", ...)

#' Plotting efficiency
#' @export
plotEfficiency <- function(M, world, ...){
  FE1 <- ldply(M, computeEfficiency, 
               resource = world$resource, 
               world = world,
               .id = "year") %>% mutate(year = 1:length(year))
  plot(FE1, type = "o", ...)
}


#' Plot migration patterns over resource
#' 
#' @param 
#' @export
plotMigration <- function(M, world, plotresource = TRUE, add = FALSE){
  if(plotresource) 
    with(world, image(time, X, resource, col = grey.colors(100))) else 
      with(world, image(time, X, resource, col = NA))
  memory.df <- ldply(M, function(l)
    data.frame(time = 1:nrow(l), 
               memory = getMem(l, world = world)),.id = "year")
  
  if(!add) with(memory.df, plot(time, memory, type= "n"))
  n.years <- length(unique(memory.df$year))
  palette(rich.colors(n.years))
  ddply(memory.df, "year", function(df)
    with(df, lines(time, memory, col = as.integer(year))))
}



#' Plot estimated migration (with or without memory)
#' 
#' @param mhat data frame of migration estimates
#' @param {x.peak,t.peak} 
#' @export

plotMigrationHat <- function(mhat, x.peak = NULL, t.peak  = NULL, 
                             cols = c("darkorange", "darkblue"), legend = TRUE){
  par(mfrow = c(1,2), mar = c(3,3,2,2), xpd = FALSE); with(mhat,{
    plot(year, t1, ylim = c(0,100), ylab = "migration timing (day of year)", col = cols[1])
    segments(year, t1, year, t1+dt1, col = cols[1])
    points(year, t1 + dt1, col = cols[1])
    
    points(year, t2, col = cols[2])
    points(year, t2 + dt2, col = cols[2])
    segments(year, t2, year, t2+dt2, col = cols[2])
    if(!is.null(t.peak))
      abline(h = c(t.peak,100-t.peak), col =alpha("black",.3), lwd = 3, lty =3)
    
    plot(year, x1, type = "o", ylim = c(-100,100), ylab = "seasonal centroids", col = cols[1])
    lines(year, x2, type = "o", col = cols[2])
    
    if(!is.null(x.peak))
    abline(h = c(-x.peak,x.peak), col =alpha("black",.3), lwd = 3, lty =3)
    
    if(legend)
    legend("topright", pch = c(1,1,NA), lty = c(1,1,3), 
           lwd = c(1,1,3), 
           legend = c( "summer", "winter", "true value"), col = c(cols, "darkgrey"), bty = "n")
  })
}


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


#' Plot memories against years
#' 
#' This function plots the memory of the population over a time period for each year of the population.
#' 
#' @param sim A simulation run from runManyYears
#' @param world world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function.
#'  @seealso \link{PlotYearList}; \link{getSinePop}; \link{getOptimalPop}; \link{runManyYears}
#' @example examples/plotting_examples.R
#' @export
plotMemories <- function(sim, world){
  memory.df <- ldply(sim, function(l)
    data.frame(time = 1:nrow(l), 
               memory = getMem(l, world = world)))
  
  ggplot(memory.df, aes(time, memory, col = .id)) + geom_path() + 
    theme_few()
}

#' Double Plot for Shiny
#' @export


doublePlotForShiny <- function(M, world){
  par(mfrow = c(1,2), mar = c(2,2,1,1), 
      tck = 0.01, mgp = c(1.5,.25,0), 
      bty = "l", cex.lab = 1.25, las = 1, xpd = NA)
      plotMigrationForShiny(M, world, add = TRUE)
      FE1 <- ldply(M, computeEfficiency, 
                 resource = world$resource[1,,], world = world,
                 .id = "year") %>% mutate(year = 1:length(year))
      plot(FE1, type = "o")
  
}

#' @export
plotMigrationForShiny <- function(M, world, plotresource = TRUE, add = FALSE){
  if(plotresource) 
    with(world, image(time, X, resource[1,,], col = grey.colors(100))) else 
      with(world, image(time, X, resource[1,,], col = NA))
  memory.df <- ldply(M, function(l)
    data.frame(time = 1:nrow(l), 
               memory = getMem(l, world = world)),.id = "year")
  
  if(!add) with(memory.df, plot(time, memory, type= "n"))
  n.years <- length(unique(memory.df$year))
  palette(rich.colors(n.years))
  ddply(memory.df, "year", function(df)
    with(df, lines(time, memory, col = as.integer(year))))
}
