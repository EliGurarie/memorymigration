# Initialize world

dx <- 1

parameters <- c(epsilon = 5, alpha = 300, beta0=100, beta1 = 300)
world.R1 <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=2, 
                    peak.max = 80, peak.min = 20, sd = 10)
world.R1$resource <- getPulsedResource(world.R1$time, world.R1$X, c(t.peak = 25, t.sd = 9, 
                                                           x.peak = 80, x.sd = 9))

#run simulation for 12 years 
M <- runManyYears(world.R1, Parameters = parameters, n.years = 6, 1)

world.R2 <- world.R1 %>% list_modify(pop = M$Year6)
M2 <- runManyYears(world.R2, Parameters = parameters, n.years = 6, 1)
plotYearList(M2, world.R2, tau = tau)

#simulation runs for 3 years since that is when threshold is hit

M <- runManyYears(world.R1, Parameters = parameters, n.years = 12, 0.998)
plotYearList(M, world.R1, tau = tau)

computeCohesiveness(M[[6]], world.R1)["SC.mean"]
computeMigratoriness(M[[6]], world.R1)$overlap
computeEfficiency(M[[6]], world.R1$resource, world.R1)
computeIndices(M[[6]], world.R1$resource, world.R1)
