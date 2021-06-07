
#' Foraging with memory model
#' 
#' This function sets up the diffusion advection model 
#' 
#' @param t vector of times
#' @param pop a T x X matrix describing the population distribution 
#' across the time period.
#' @param parms named vector of parameters. These are \code{epsilon} - 
#' diffusion coefficient; \code{alpha} - resource following coefficient; 
#' \code{beta} - spatial scale of sociality; \code{kappa} - memory 
#' following coefficient; \code{lambda} - maximum speed
#' @param memory the memory score
#' @param dh a row of a resource matrix (a T x X distribution of the 
#' (potentially) dynamic resource.)
#' @param dx time step
#' @return Model setup; output of \code{\link{tran.1D}} function. 
#' @export



buildOnRuns <- function(M, world, ...){
  world$pop <- M$pop[[length(M$pop)]]
  M2 <- runManyYears(world, threshold = 1, ...)
  M2$pop[[1]] <- NULL
  M3 <- list()
  M3$pop <- c(M$pop, M2$pop)
  M3$m.hat <- c(M$m.hat, M2$m.hat)
  names(M3$pop) <- paste0("Year",1:length(M3$pop))
  M3
}


runManyYears <- function(world, parameters, n.years = 20, 
                         threshold= 0.9999, verbose = FALSE, 
                         FUN = runNextYear){
  cat("\n")
  cat(paste(names(parameters), parameters, collapse = "; "))
  
  migration.list <- list(Year1 = world$m0)
  pop.list <- list(Year1 = world$pop)
  
  similarity <- 0
  i <- 1 
  while((similarity < threshold) & (i < n.years)){
    if(verbose){cat("\n"); cat(paste("running year ", i))}
   
    i <- i+1
    
    pop.new <- FUN(world = world, 
                   parameters = parameters, 
                   pop.lastyear = pop.list[[length(pop.list)]], 
                   Year = i)
    pop.list[[i]] <-  pop.new
    migration.list[[i]] <- getMigrationParameters(world$m0, pop.new, 
                                    parameters["kappa"], 
                                    year = i, 
                                    world)
    similarity <- computeEfficiency(pop.list[[i-1]], pop.list[[i]], world)
  }
  names(pop.list) <- paste0("Year",0:(length(pop.list)-1))
  attr(pop.list, "parameters") <- parameters
  
  m.hat <- ldply(migration.list, .id = "year") %>% mutate(year = 1:length(pop.list) - 1)
  return(list(pop = pop.list, m.hat = migration.list))
}

eval <- FALSE
if(eval){
  pop1 <- M0$pop[[4]]
  image(world$time, world$X, pop1)
  m.hat <- getMigrationParameters(world$m0, pop1, parameters["kappa"], Year, world)
  with(as.list(m.hat), abline(v = c(t1 + c(0, dt1), 
                                  t2 + c(0, dt2))))
  with(as.list(m.hat), abline(h = c(x1, x2)))
  contour(world$time, world$X, world$resource, add = TRUE)
}


runNextYear <- function(world, parameters, pop.lastyear, Year){
  pop0 <- world$pop
  pop1 <- pop.lastyear
  pop2 <- pop0*0
  Time <- world$time

  m.hat <- getMigrationParameters(world$m0, pop1, parameters["kappa"], Year, world)
  
  migration.hat <- with(as.list(m.hat), getMigration(Time, t1, dt1, t2, dt2, x1, x2))
  
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
                        pop = extend(pop_now), 
                        lambda = parameters["lambda"])
      
      pop2[t,] <- ode(y = pop_now, 
                      times = 0:1, 
                      parms = parameters,
                      func = ForagingMemoryModel, 
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

ForagingMemoryModel <- function(t, pop, parms, v.memory, dh, dp, dx = dx){
  tran.1D(C = pop, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * dh/dx + 
            parms["beta"] * dp/dx + 
            v.memory,
          dx = dx)
}

#' @export
convolvePop <- function(X, pop, lambda){
  K <- function(x, l) -x*exp(-x^2/(2*l^2)) / (2*l^2)
  ck <- function(x, pop, X, l) sum(K(x-X, l)*pop)
  sapply(X, FUN = ck, pop = pop, X = X, l = lambda)
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
