require("memorymigration")
# Initialize world
  
  world <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=1, 
                      peak.max = 80, peak.min = 20, sd = 10)
  world$resource <- getPulsedResource(world$time, world$X, c(t.peak = 25, t.sd = 9, 
                                                             x.peak = 80, x.sd = 9))
  
  
  parameters <- c(epsilon = 5, alpha = 300, beta0=100, beta1 = 300)
  world.R1 <- world %>% 
    list_modify(resource = getPulsedResource(1:world$tau, world$X, 
                                             c(t.peak = 25, t.sd = 9, 
                                               x.peak = 80, x.sd = 9)))
  
# run simulation 12 years
  
  M <- runManyYears(world.R1, Parameters = parameters, n.years = 6)
  plotYearList(M, tau = tau)
  
  pop <- world$pop
  resource <- world$resource
  
  world.R1 <- world %>% list_modify(pop = M$Year6)
  M2 <- runManyYears(world.R1, Parameters = parameters, n.years = 6)
  plotYearList(M2, tau = tau)
  
  source("code/functions_indices.R")
  
  computeCohesiveness(world$pop, world)["SC.mean"]
  computeMigratoriness(world$pop, world)$overlap
  computeEfficiency(world$pop, world$resource, world)
  computeIndices(world$pop, world$resource, world)
  computeIndices(M[[2]], world$resource, world)
  
# Migratoriness Index


  indices1 <- sapply(M, computeIndices, resource = world$resource, world = world) %>% t
  indices2 <- sapply(M2, computeIndices, resource = world$resource, world = world) %>% t
  I <- rbind(indices1, indices2)
  
  matplot(I, type="o", pch = 19, lty = 1)
  legend("topright", pch = 19, col = 1:3, legend = colnames(I))

  I.v2 <- sapply(Hybrid_PulsedResource_Sim, computeIndices, 
         world = world.R1,
         resource = world.R1$resource) %>% t
  matplot(I.v2, type="o", pch = 19, lty = 1)
  legend("topright", pch = 19, col = 1:3, legend = colnames(I))
  

  