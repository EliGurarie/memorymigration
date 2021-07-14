#' Run Foraging Memory Model
#' 
#' Based on a migratory population's set up (the World) and the values \code{epsilon}, 
#' \code{alpha}, \code{beta}, \code{kappa} and \code{lambda}, this function determines the population 
#'distribution for the following year. 
#' 
#' @param World world object; list of 7: a population distribution across the time period in a T x X matrix,
#'  a vector with midpoint X-values, the time points for the population as integers 1:tau, the minimum value of population distribution
#'  (X.min), the maximum value of population distribution (X.max),
#'  the dx value and the tau value. Can incorporate resource attribute into the world to make a list of 8.
#'  Set up by the getSinePop/getOptimal function. 
#' @param Parameters named vector of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - resource following coefficient; \code{beta} - spatial scale of sociality; \code{kappa} - 
#' memory following coefficient; \code{lambda} - maximum speed
#' @return a T x X matrix describing the population distribution for the next year
#' @seealso \link{getSinePop}, \link{getOptimalPop}, \link{runManyYears}
#' @export

runFWM <- function(world, parameters, pop.lastyear, Year, m.hat = world$m0){
  pop0 <- world$pop
  pop1 <- pop.lastyear
  pop2 <- pop0*0
  Time <- world$time
  
  migration.hat <- with(as.list(m.hat), stepMigration(Time, t1, dt1, t2, dt2, x1, x2))
  
  if(length(dim(world$resource)) == 3)
    resource <- world$resource[Year,,] else
      resource <- world$resource
  
  dresource <- apply(resource, 1, ddx.edge) %>% t
  v.migration <- extend(diff(migration.hat))
  
  for(t in Time){
    if(t == 1) 
      pop_now <- pop.lastyear[nrow(pop.lastyear),] else 
        pop_now <- pop2[t-1,]
      dp <- convolvePop(seq(world$X.min, world$X.max, world$dx), 
                        pop = extend(pop_now), lambda = parameters["lambda"])
      
      pop2[t,] <- ode(y = pop_now, 
                      times = 0:1, 
                      parms = parameters,
                      func = ForagingWithMemory, 
                      dh = dresource[t,],
                      dp = dp, 
                      v.memory = v.migration[t],
                      dx = world$dx)[2,1:ncol(pop2)+1]
  }
  if(any(pop2 < 0)){
    warning("Some negative population values will be coerced to 0.")
  }
  pop2[pop2 < 0] <- 0
  pop2 <- apply(pop2, 1, function(x) x/sum(x)) %>% t
  pop2
}


#' Foraging with memory model
#' 
#' This function sets up the diffusion advection model 
#' 
#' @param t vector of times
#' @param pop a T x X matrix describing the population distribution 
#' across the time period.
#' @param parms named vector of parameters. These are \code{epsilon} - diffusion coefficient; 
#' \code{alpha} - forage following coefficient; \code{beta} - strength of sociality; \code{lambda} - 
#' spatial sclae of sociality; \code{kappa} - propotion of reference versus working memory
#' @param memory the memory score
#' @param dh a row of a resource matrix (a T x X distribution of the (potentially) dynamic resource.)
#' @param dp memory contribution
#' @param dx time step
#' @return Model setup; output of \code{\link{tran.1D}} function. 
#' @export

ForagingWithMemory <- function(t, pop, parms, v.memory, dh, dp, dx = dx){
  tran.1D(C = pop, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * dh/dx + 
            parms["beta"] * dp/dx + 
            v.memory,
          dx = dx)
}

#' @export
getMem <- function(pop, world){
  apply(t(pop)*world$X, 2, sum) * world$dx
}

#' @export
extend <- function(x){
  x1 <- c(x[length(x)],x,x[1])
  (x1[-1] + x1[-length(x1)])/2
}

#' @export
ddx.edge <- function(x)
  diff(c(x[1], x, x[length(x)])) 


#' @export
convolvePop <- function(X, pop, lambda){
  K <- function(x, l) -x*exp(-x^2/(2*l^2)) / (2*l^2)
  ck <- function(x, pop, X, l) sum(K(x-X, l)*pop)
  sapply(X, FUN = ck, pop = pop, X = X, l = lambda)
}


