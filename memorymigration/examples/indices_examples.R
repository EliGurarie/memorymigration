# Initialize world
require(memorymigration)



world <- getOptimalPop(tau=100, X.min = -100, X.max = 100, dx=1, 
                       x1 = 30, x2 = -30, t.peak=25, 
                       x.sd=12, t.sd=6)
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))

par0 <- getCCpars(mu_x0 = 30, 
                  mu_t0 = 25,
                  beta_x = 0,
                  beta_t = 0,
                  n.years = 30,
                  sigma_x = 12,
                  sigma_t = 6,
                  psi_x = 0, 
                  psi_t = 0,
                  n.years.null = 0) 

world$resource <- aaply(par0, 1, function(p) getResource_island(world, p))
attr(world$resource, "par") <- par0[nrow(par0),]

#run simulation for 12 years 

parameters <- c(epsilon = 4, alpha = 100, beta=400, kappa = 0, lambda = 80)

sim <- runManyYears(world, parameters = parameters, n.years = 12, threshold = 0.9999)


plotManyRuns(sim$pop, world)
plotYearList(sim$pop, world, tau = tau)

# computing indices
computeCohesiveness(sim$pop[[12]], world)["SC.mean"]
computeMigratoriness(sim$pop[[12]], world)$overlap
computeEfficiency(sim$pop[[12]], world$resource[11,,], world)
computeAvgEfficiency(sim$pop, world$resource, world)
computeIndices(sim$pop[[12]], world$resource[11,,], world)
computeMigrationIndices(sim, world)
computeTotalError(sim, world)
computeAvgTotalError(sim, world, par0)
