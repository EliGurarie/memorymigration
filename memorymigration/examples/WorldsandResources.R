## Worlds
  world_nonmigratory <- getSinePop(tau = 100, peak.max = 0, peak.min = 0, sd = 10)
  world_sinusoidal <- getSinePop(tau = 100, peak.max = 40, peak.min = -40, sd = 5)
  world_optimal <- getOptimalPop(tau = 100, t.peak = 25, 
                                 x1 = 40, x2 = -40, x.sd = 5, t.sd = 12)
  
  
  require(fields)
  with(world_nonmigratory, image.plot(time, X, pop))
  with(world_sinusoidal, image.plot(time, X, pop))
  with(world_optimal, image.plot(time, X, pop))
  
  
  
## Fixed (bimodal) resource example

  resource_drifting <- getResource_drifting(world_sinusoidal, 
                                         par = c(t.peak = 25, t.sd = 12, 
                                                 x.peak = 40, x.sd = 5))
  
  with(world_sinusoidal, image.plot(time, X, resource_drifting))
  
  
## Independent (Island) resource
  
  resource_island <- getResource_island(world_sinusoidal, 
                                            par = c(t.peak = 25, t.sd = 12, 
                                                    x.peak = 80, x.sd = 5))
  
  with(world_sinusoidal, image.plot(time, X, resource_island))
  
  