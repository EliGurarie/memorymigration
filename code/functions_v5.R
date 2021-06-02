runManyYears <- function(world, parameters, n.years = 20, 
                         threshold= 0.9999, verbose = FALSE, 
                         FUN = runNextYear){
  cat("\n")
  cat(paste(names(parameters), parameters, collapse = "; "))
  
  pop.list <- list(Year1 = world$pop)
  i <- 1
  pop.list[[2]] <- FUN(world, Parameters = parameters, 
                         pop.lastyear = world$pop, Year = 1)
  similarity <- computeEfficiency(pop.list[[i]], pop.list[[i+1]], world)
  
  while((similarity < threshold) & (i < n.years)){
    if(verbose){cat("\n"); cat(paste("running year ", i))}
    
    i <- i+1
    pop.list[[i+1]] <- FUN(world = world, 
                           Parameters = parameters, 
                           pop.lastyear = pop.list[[i]], 
                           Year = i)
    similarity <- computeEfficiency(pop.list[[i]], pop.list[[i+1]], world)
  }
  names(pop.list) <- paste0("Year",0:(length(pop.list)-1))
  attr(pop.list, "parameters") <- parameters
  return(pop.list)
}


runNextYear_v2 <- function(world, Parameters, pop.lastyear, Year){
  pop.ref <- world$pop
  Time <- world$time
  
  memory <- Parameters["kappa"]^Year * getMem(pop.ref, world) + 
    (1 - Parameters["kappa"]^Year) * getMem(pop.lastyear, world)
  
  if(length(dim(world$resource)) == 3)
    resource <- world$resource[Year,,] else
      resource <- world$resource
  
  # get new X frame of reference
  maxmem <- max(memory)
  minmem <- min(memory)
  X <- world$X
  X.t <- seq(min(X) + minmem, max(X) + maxmem, world$dx)
  
  
  resource.t <- transformToMemoryFrame(resource, X.t, memory, world)
  pop.lastyear.t <- transformToMemoryFrame(pop.lastyear, X.t, memory, world)
  pop.thisyear.t <- resource.t*0
  dresource.t <- apply(resource.t, 1, ddx.edge) %>% t
  
  for(t in Time){
    if(t == 1) 
      pop_now.t <- pop.lastyear.t[nrow(pop.lastyear.t),] else 
        pop_now.t <- pop.thisyear.t[t-1,]
      
      dp.t <- convolvePop(X.t, 
                          pop = pop_now.t, 
                          lambda = Parameters["lambda"])
      
      pop.thisyear.t[t,] <- ode(y = pop_now.t, 
                                times = 0:1, 
                                parms = Parameters,
                                func = ForagingMemoryModel, 
                                dh = dresource.t[t,],
                                dp = extend(dp.t),
                                dx = world$dx)[2,1:ncol(pop.thisyear.t)+1]
  }
  
  pop.thisyear <- transformToRealFrame(pop.thisyear.t, X.t, memory, world)
  pop.thisyear[pop.thisyear < 0] <- 0
  pop.thisyear <- apply(pop.thisyear, 1, function(x) x/sum(x)) %>% t
  #print(range(pop.thisyear))
  return(pop.thisyear)
}



transformToMemoryFrame <- function(m, X.t, memory, world){
  m.t <- matrix(0, ncol = length(X.t), nrow = length(world$time))
  for(t in world$time){
    f <- splinefun(X, m[t,])
    m.t[t, ] <- f(X.t + memory[t])   
  }
  m.t
}

transformToRealFrame <- function(m.t, X.t, memory, world){
  X <- world$X
  m <- world$pop*0
  for(t in world$time){
    f <- splinefun(X.t, m.t[t,])
    m[t, ] <- f(X - memory[t])   
  }
  m
}

ForagingMemoryModel <- function(t, pop, parms, 
                                memory, dh, dp, dx = dx){
  tran.1D(C = pop, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * dh/dx + 
            parms["beta"] * dp/dx,
          dx = dx)
}

convolvePop <- function(X, pop, lambda){
  ck <- function(x, pop, X, l) sum(-((x-X) / (2*l^2)) * exp(-(x-X)^2/(2*l^2)) * pop)
  sapply(X, FUN = ck, pop = pop, X = X, l = lambda)
}



getMem <- function(pop, world){
  pop[pop < 0 | is.na(pop)] <- 0
  pop <- pop/sum(pop)
  mem <- apply(pop, 1, function(p) weighted.mean(world$X, w = p)) 
  # handssplice end and start
  m1 <- mem[length(mem)-1]
  m2 <- mem[2]
  mem[length(mem)] <- m1 + (m2-m1)/3
  mem[1] <- m1 + 2*(m2-m1)/3
  #apply(pop, 1, function(p) weighted.median(world$X, w = p)) 
  #apply(pop, 1, function(p) world$X[which.max(p)]) 
  mem
}


plotMemory <- function(M, add = FALSE){
  require(plyr)
  memory.df <- ldply(M, function(l)
    data.frame(time = 1:nrow(l), 
               memory = getMem(l, world = world)),.id = "year")
  
  if(!add) with(memory.df, plot(time, memory, type= "n"))
  n.years <- length(unique(memory.df$year))
  palette(rich.colors(n.years))
  ddply(memory.df, "year", function(df)
    with(df, lines(time, memory, col = as.integer(year))))
  #legend("topright", legend = 1:n.years, col = 1:n.years, 
  #       lty = 1, ncol = 2, title = "year", bty = "n")
}

doublePlot <- function(M, world){
  par(mfrow = c(1,2), mar = c(2,2,1,1), 
      tck = 0.01, mgp = c(1.5,.25,0), 
      bty = "l", cex.lab = 1.25, las = 1, xpd = NA)
  with(world, image(time, X, resource, col = grey.colors(100)))
  plotMemory(M, add = TRUE)
  FE1 <- ldply(M, computeEfficiency, 
               resource = world$resource, world = world,
               .id = "year") %>% mutate(year = 1:length(year))
  plot(FE1, type = "o")
}


buildOnRuns <- function(M, world, ...){
  world$pop <- M[[length(M)]]
  M2 <- runManyYears(world, threshold = 1, ...)
  M2[[1]] <- NULL
  M3 <- c(M, M2)
  names(M3) <- paste0("Year",1:length(M3))
  M3
}