
getMem <- function(pop, world){
  apply(t(pop)*world$X, 2, sum) * world$dx
}

extend <- function(x){
  x1 <- c(x[length(x)],x,x[1])
  (x1[-1] + x1[-length(x1)])/2
}

ddx.edge <- function(x)
  diff(c(x[1], x, x[length(x)])) 


runManyYears <- function(world, parameters, n.years = 60, 
                         threshold= 0.995, verbose = FALSE){
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
    pop.list[[i+1]] <- runNextYear(World = world, 
                                   Parameters = parameters, 
                                   Pop_lastyear = pop.list[[i]], 
                                   Year = i)
    similarity <- computeEfficiency(pop.list[[i]], pop.list[[i+1]], world)
  }
  names(pop.list) <- paste0("Year",0:(length(pop.list)-1))
  attr(pop.list, "parameters") <- parameters
  return(pop.list)
}


Parameters <- c(epsilon = 1, alpha = 100, beta=.01, kappa = 1, lambda = 10)


par(mfrow = c(2,2))
image.plot(X.matrix)
image.plot(memory.matrix)
image.plot(dmemory.matrix)
image.plot(dm.matrix)

runNextYear <- function(World, Parameters, Pop_lastyear, Year){
  pop0 <- World$pop
  pop1 <- Pop_lastyear
  pop2 <- pop0*0
  Time <- World$time
  memory <- Parameters["kappa"] * getMem(pop0, World) + 
    (1 - Parameters["kappa"]) * getMem(pop1, World)
  dmemory <- diff(extend(memory))
  
  X.edge <- seq(0,World$X.max, World$dx)
  X.matrix <- matrix(rep(X.edge, times = World$tau), byrow = TRUE, nrow = World$tau)
  memory.matrix <- matrix(rep(memory, length(X.edge)), byrow = FALSE, nrow = World$tau)
  dmemory.matrix <- matrix(rep(dmemory, length(X.edge)), byrow = FALSE, nrow = World$tau)
  
  dm.matrix <- dm.adjust(X.matrix, memory.matrix, dmemory.matrix,
                         Parameters["beta"], Parameters["lambda"])

  if(length(dim(World$resource)) == 3)
    resource <- World$resource[Year,,] else
      resource <- World$resource
  
  dresource <- apply(resource, 1, ddx.edge) %>% t
  for(t in Time){
    if(t == 1) 
      pop_now <- Pop_lastyear[nrow(Pop_lastyear),] else 
        pop_now <- pop2[t-1,]
    pop2[t,] <- ode(y = pop_now, 
                    times = 0:1, 
                    parms = Parameters,
                    func = ForagingMemoryModel, 
                    dh = dresource[t,],
                    memory = dm.matrix[t,],
                    dx = World$dx)[2,1:ncol(pop2)+1]
  }
  if(any(pop2 < 0)){
    warning("Some negative population values will be coerced to 0.")
  }
  pop2[pop2 < 0] <- 0
  pop2 <- apply(pop2, 1, function(x) x/sum(x)) %>% t
  pop2 
}

ForagingMemoryModel <- function(t, pop, parms, memory, m_minus_X, dh, dx = dx){
  tran.1D(C = pop, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * dh/dx + memory,
          dx = dx)
}


dm.adjust <- function(x, m.hat, dm, beta, lambda){
  x0 <- ifelse(dm > 0, 
               m.hat - log(lambda/dm - 1)*beta,  ifelse(
                 dm < 0, 
                 m.hat + log(lambda/(-dm) - 1)*beta,
                 0)) 
  
  ifelse(dm > 0, 
         lambda / (1 + exp((x - x0)/beta)), ifelse(
           dm < 0,
           -lambda / (1 + exp(-(x - x0)/beta)),
           0))
}

plotMemories <- function(sim){
  memory.df <- ldply(sim, function(l)
    data.frame(time = 1:nrow(l), 
               memory = getMem(l, world = world)))
  
  ggplot(memory.df, aes(time, memory, col = .id)) + geom_path() + 
    theme_few()
}

rm(list=ls())

require(memorymigration)
world <- getSinePop(tau = 100, X.min = 0, X.max = 200, dx=1, 
                    peak.max = 140, peak.min = 60, sd = 10)

world$resource <- getPulsedResource_v2(world, 
                                       par = c(t.peak = 25, t.sd = 12, 
                                               x.peak = 120, x.sd = 6))

M1 <- runManyYears(world, 
                  c(epsilon = 1, alpha = 100, 
                    beta= 100, kappa = 0, lambda = 20), 
                  n.years = 10, threshold = 1)
plotManyRuns(M1, world)
plotMemories(M1) + ggtitle("all reference memory")


M2 <- runManyYears(world, 
                   c(epsilon = 1, alpha = 100, 
                     beta = 100, kappa = 1, lambda = 20), 
                   n.years = 10, threshold = 1)
plotManyRuns(M2, world)
plotMemories(M2) + ggtitle("all reference memory")
