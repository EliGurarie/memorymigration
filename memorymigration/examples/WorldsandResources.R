## Worlds
  world_nonmigratory <- getSinePop(tau = 100, peak.max = 0, peak.min = 0, sd = 10)
  world_sinusoidal <- getSinePop(tau = 100, peak.max = 40, peak.min = -40, sd = 10)
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
  
  
## Climate Change Island Resource
  par0 <- getCCpars(mu_x0 = 80, 
                    mu_t0 = 25,
                    beta_x = -1,
                    beta_t = 0,
                    n.years = 5,
                    sigma_x = 5,
                    sigma_t = 12,
                    psi_x = 0, 
                    psi_t = 0)
  world$resource <- aaply(par0, 1, function(p) getResource_island(world, p))

  
  