require(deSolve)
require(ReacTran)
require(magrittr)
#' Foraging model

ForagingMemoryModel <- function(t, y, parms, resource, pop_lag){
  tran.1D(C = y, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * diff(resource)/parms["dx"] +  
            parms["gamma"] * diff(y)/parms["dx"] +
            parms["beta"] * diff(pop_lag)/parms["dx"], 
          dx = parms["dx"])
}

#' Run One Year forward

runNextYear <- function(pop.Year1, Time, Resource, Parameters){
  
  pop.Year2 <- pop.Year1*0
  pop.Year2[1,] <- pop.Year1[nrow(pop.Year1),]
  
  t.max <- max(Time)
  
  fl <-  Parameters["lambda"]
  
  for(i in 2:t.max){
    if(i < t.max-fl)
      pop_lag <- c(0,pop.Year1[i+fl,],0) else
        pop_lag <- c(0,pop.Year2[max(i - (t.max - fl)),],0)
      
      pop.Year2[i,] <- ode(y = pop.Year2[i-1,], 
                           times = c(0,Time[i]-Time[i-1]), 
                           parms = Parameters,
                           func = ForagingMemoryModel, 
                           resource = c(0,Resource[i,],0),
                           pop_lag = pop_lag)[2,1:ncol(pop.Year2)+1]
  }
  
  pop.Year2
}

#' Run several years forward

runManyYears <- function(pop.Year1, Time, Resource, Parameters, n.years){
  pop.list <- list(Year1 = pop.Year1)
  
  for(i in 1:n.years){
    cat(paste("running year ", i, "\n"))
    pop.list[[i+1]] <- runNextYear(pop.list[[i]], Time, Resource = Resource, 
                                   Parameters = parameters)
  }
  names(pop.list) <- paste0("Year",0:n.years)
  pop.list <- lapply(pop.list, function(l) l[-1,])
  pop.list
}


#' Plotting Functions

mypersp <- function(x,y,z,...)
  persp(x,y,z, theta=45, phi=45, bor=NA, shade=TRUE, ylab = "x", xlab="time", zlab="population", ...)

plotYearList <- function(yearlist, X.max = 100, tau = 360, log = FALSE, 
                         persp = FALSE, ...){
  
  n.years <- length(yearlist)
  allyears <- do.call(rbind, yearlist)
  
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

printParameters <- function(p) 
  parse(text = paste0("list(",paste(names(p), p, sep = "==", collapse = ", "),")"))


#' getPulsedResource

getPulsedResource <- function(t, x, par){
  
  t.peak <- par["t.peak"] 
  t.sd <- par["t.sd"] 
  x.peak <- par["x.peak"]
  x.sd <- par["x.sd"]
  
  x.scaled <- x/max(x)
  x.peak <- x.peak/max(x)
  x.sd <- x.sd/max(x)
  
  t1 <- t[1:(length(t)/2)]
  
  m1 <- (dnorm(t1, mean = t.peak, sd = t.sd) %>% {./max(.)}) * (x.peak - .5) + .5
  m2 <- 1-m1[length(m1):1]
  m <- c(m1, m2)
  
  # spline the sigma
  k <- 1/sqrt(12)
  s1 <- k - ((dnorm(t1, t.peak, t.sd) %>% {./max(.)}) * (k - x.sd))
  s <- c(s1, s1[length(s1):1])
  
  a = -(m*s^2+m^3-m^2)/s^2
  b = ((m-1)*s^2+m^3-2*m^2+m)/s^2
  
  R <- sapply(1:length(t), function(i) dbeta(x.scaled, a[i], b[i])) %>% t
  R[,1] <- 1
  R[,length(x)] <- 1
  colnames(R) <- x
  rownames(R) <- t
  R <- apply(R, 1, function(x) x/sum(x)) %>% t
  return(R)
}