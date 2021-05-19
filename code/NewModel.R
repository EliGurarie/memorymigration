
getDMem <- function(pop, world){
  memory <- apply(t(pop)*world$X, 2, sum) * world$dx
  dmem.raw <-  diff(c(memory[length(memory)], memory, memory[1])) 
  (dmem.raw[-1] + dmem.raw[-length(dmem.raw)])/2
}

ForagingMemoryModel_v2 <- function(t, pop, parms, 
                                   dmem.ref, dmem.new,
                                   resource, 
                                   dx = dx){
  tran.1D(C = pop, D = parms["epsilon"], 
          flux.up = 0, flux.down = 0, 
          v = parms["alpha"] * diff(resource)/dx + 
            parms["beta"] * (parms["kappa"] * dmem.ref + (1-parms["kappa"]) * dmem.new), 
          dx = dx)
}

runNextYear <- function(World, Parameters, Pop_lastyear, Year){
  pop0 <- Pop_lastyear
  pop2 <- pop0*0
  Time <- World$time
  dmem.ref <- World$dmem.ref
  dmem.new <- getDMem(pop0, World)
  
  if(length(dim(World$resource)) == 3)
    myresource <- World$resource[Year,,] else
      myresource <- World$resource
  
  for(t in Time){
    if(t == 1) pop_now <- Pop_lastyear[nrow(Pop_lastyear),] else 
      pop_now <- pop2[t-1,]
    pop2[t,] <- ode(y = pop_now, 
                    times = 0:1, 
                    parms = Parameters,
                    func = ForagingMemoryModel_v2, 
                    resource = c(0,myresource[t,],0),
                    dmem.ref = dmem.ref[t],
                    dmem.new = dmem.new[t],
                    dx = World$dx)[2,1:ncol(pop2)+1]
  }
  pop2 
}

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




world.R1 <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=1, 
                       peak.max = 70, peak.min = 30, sd = 10)
world.R1$resource <- getPulsedResource(world.R1, 
                                       par = c(t.peak = 25, t.sd = 9, 
                                               x.peak = 80, x.sd = 9))
world.R1$dmem.ref <- getDMem(world.R1$pop, world.R1)


M1 <- runManyYears(world.R1, 
                  c(epsilon = .5, alpha = 0, beta=.5, kappa = 1), 
                  n.years = 10, 1)
plotManyRuns(M1)

M2 <- runManyYears(world.R1, 
                  c(epsilon = 1, alpha = 300, beta=1, kappa = 1), 
                  n.years = 10, 1) 
plotManyRuns(M2)


M3 <- runManyYears(world.R1, 
                   c(epsilon = .5, alpha = 100, beta=1, kappa = 0), 
                   n.years = 10, 1) 
plotManyRuns(M3)

dm0 <- getDMem(M1[[1]], world.R1)
dm1 <- getDMem(M1[[length(M1)]], world.R1)
dm2 <- getDMem(M2[[11]], world.R1)
dm3 <- getDMem(M3[[11]], world.R1)

par(mfrow = c(1,1))
plot(dm0, type = "o", ylim = c(-2,2))
lines(dm1, type = "o", col = 2)
lines(dm2, type = "o", col = 3)
lines(dm3, type = "o", col = 4)
