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

runNextYear(world.R1, Parameters=parameters)
M <- runManyYears(world.R1, Parameters = parameters, n.years = 6)
plotYearList(M, tau = tau)

pop <- world$pop
resource <- world$resource

world.R1 <- world %>% list_modify(pop = M$Year6)
M2 <- runManyYears(world.R1, Parameters = parameters, n.years = 6)
plotYearList(M2, tau = tau)

computeCohesiveness(world$pop, world)["SC.mean"]
computeMigratoriness(world$pop, world)$overlap
computeEfficiency(world$pop, world$resource, world)
computeIndices(world$pop, world$resource, world)
computeIndices(M[[2]], world$resource, world)
