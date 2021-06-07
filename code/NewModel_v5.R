  require(memorymigration)
  source("code/functions_v5.R")
  
  Parameters <- c(epsilon = 1, alpha = 100, kappa = 0, beta = 100, lambda = 5)
  
  world <- getSinePop(tau = 100, peak.max = 50, peak.min = -50, sd = 10)
  
  
  world$resource <- getResource_island(world, 
                                       c(t.peak = 25, t.sd = 7, 
                                         x.peak = 30, x.sd = 5))
  
  image(world$resource)
  
  p0 <- c(epsilon = 1, alpha = 100, kappa = 0, beta = 20, lambda = 30)
  M2 <- runManyYears(world, parameters = p0, n.years = 20, 1, FUN = runNextYear_v2, verbose = TRUE)
  #M2 <- buildOnRuns(M2, world, parameters = p0, n.years = 40, verbose = TRUE, FUN = runNextYear_v2)
  
  plotManyRuns(M2, world, nrow = 3)
  doublePlot(M2, world)
  
  m <- getMem(M2[[length(M2)]], world)
  d <- sapply(M2, function(M) max(abs(diff(getMem(M, world)))))
  plot(d)
  
  plot(diff(m))
  lines(abs(diff(getMem(M2[[length(M2)]], world))), col = "red")
  lines(abs(diff(getMem(M2[[15]], world))), col = "darkgreen")
  lines(abs(diff(getMem(M2[[10]], world))), col = "orange")


#. Example 2: learning migration

  world <- getSinePop(tau = 100, peak.max = 0, 
                      peak.min = 0, sd = 10)
  
  world$resource <- getResource_drifting(world, 
                                         c(t.peak = 25, t.sd = 12, 
                                           x.peak = 40, x.sd = 5))
  image(world$resource, col = rich.colors(101))
  
  p0 <- c(epsilon = 2, alpha = 300, kappa = 0, beta = 100, lambda = 30)
  
  M <- runManyYears(world, parameters = p0, FUN = runNextYear_v2, 
                    n.years = 31, 1, verbose = TRUE)
  
  plotManyRuns(M, world, nrow = 2, outer = FALSE)
  doublePlot(M, world)

#plotMemory(M)
#save(M, file = "results/msexamples/learningtomigrate.rda")

#plot(FE, type = "o")
