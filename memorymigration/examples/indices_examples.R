# Initialize world
require(memorymigration)
dx <- 1


world.R1 <- getOptimalPop(tau = 100, t.peak = 25, 
                               x1 = 40, x2 = -40, x.sd = 5, t.sd = 12)
world.R1$resource <- getResource_island(world.R1, 
                                       c(t.peak = 25, t.sd = 12, x.peak = 35, x.sd = 5))

#run simulation for 12 years 

parameters <- c(epsilon = 5, alpha = 100, beta=50, kappa = 1, lambda = 20)

M <- runManyYears(world.R1, parameters = parameters, n.years = 12, threshold = 0.9999)

#simulation runs for 3 years since that is when threshold is hit so only 3 years are plotted
plotManyRuns(M, world.R1)
plotYearList(M, world.R1, tau = tau)

# computing indices
computeCohesiveness(M[[3]], world.R1)["SC.mean"]
computeMigratoriness(M[[3]], world.R1)$overlap
computeEfficiency(M[[3]], world.R1$resource, world.R1)
computeIndices(M[[3]], world.R1$resource, world.R1)
