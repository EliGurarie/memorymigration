
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


runNextYear <- function(world, Parameters, Pop_lastyear, Year){
  pop0 <- world$pop
  pop1 <- Pop_lastyear
  pop2 <- pop0*0
  Time <- world$time
  
  memory <- Parameters["kappa"]^Year * getMem(pop0, world) + 
    (1 - Parameters["kappa"]^Year) * getMem(pop1, world)
  # dmemory <- diff(extend(memory))
  dm1 <- diff(memory)
  dmemory <- c(dm1[length(dm1)], dm1)
  
  if(length(dim(world$resource)) == 3)
    resource <- world$resource[Year,,] else
      resource <- world$resource
  
  #dp.matrix <- dresource * 0
  
  dresource <- apply(resource, 1, ddx.edge) %>% t
  for(t in Time){
    if(t == 1) 
      pop_now <- Pop_lastyear[nrow(Pop_lastyear),] else 
        pop_now <- pop2[t-1,]
      dp <- convolvePop(seq(world$X.min, world$X.max, world$dx), 
                        pop = extend(pop_now), 
                        lambda = Parameters["lambda"])
      
      # dp.matrix[t,] <- extend(dp)
      pop2[t,] <- ode(y = pop_now, 
                      times = 0:1, 
                      parms = Parameters,
                      func = ForagingMemoryModel, 
                      dh = dresource[t,],
                      dp = dp, 
                      memory = dmemory[t],
                      dx = world$dx)[2,1:ncol(pop2)+1]
  }
  if(any(pop2 < 0)){
    warning("Some negative population values will be coerced to 0.")
  }
  pop2[pop2 < 0] <- 0
  pop2 <- apply(pop2, 1, function(x) x/sum(x)) %>% t
  pop2 
}

ForagingMemoryModel <- function(t, pop, parms, memory, dh, dp, dx = dx){
  tran.1D(C = pop, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * dh/dx - 
            parms["beta"] * dp/dx + 
            parms["gamma"] * memory,
          dx = dx)
}

#' @export
convolvePop <- function(X, pop, lambda){
  K <- function(x, l) -x*exp(-x^2/(2*l^2)) / (2*l^2)
  ck <- function(x, pop, X, l) sum(K(x-X, l)*pop)
  sapply(X, FUN = ck, pop = pop, X = X, l = lambda)
}


#' @export
getMemoryVelocity <- function(x, m.hat, dm, beta, lambda){
  if(dm > 0){
    x0 <- m.hat - log(lambda/dm - 1)*beta  
    v <- lambda / (1 + exp((x - x0)/beta))
  } else
    if(dm < 0){
      x0 <- m.hat + log(lambda/(-dm) - 1)*beta 
      v <- -lambda / (1 + exp(-(x - x0)/beta))
    } else 
      v <- rep(0, length(x))
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