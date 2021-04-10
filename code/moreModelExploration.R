require("memorymigration")

data(world)
data(resources_island)
world_gaussian$resource <- resources_island[["R_t12_x3"]]
image(world_gaussian$resource)

world_sinusoidal <- getSinePop(100, peak.max = 75, peak.min = 25, sd = 2, dx = 1)
world_sinusoidal$resource <- resources_island[["R_t12_x3"]]


eps <- 1

sim.ResourceOnly <- runManyYears(world_sinusoidal, 
                                 threshold = 1, n.years = 5,
                                 parameters = c(epsilon = eps, alpha = 100, 
                                                beta = 0.01, gamma = 0), verbose = TRUE) %T>%
plotManyRuns(nrow = 1)


world_sinusoidal$resource <- resources_island[["R_t12_x3"]]
sim.MemOnly <- runManyYears(world_sinusoidal, 
                     threshold = 1, n.years = 5,
                     parameters = c(epsilon = eps, alpha = 0, 
                                    beta = 40, gamma = 1), verbose = TRUE)
plotManyRuns(sim.MemOnly, nrow = 1)


sim.Combined <- runManyYears(world_sinusoidal, 
                                 threshold = 1, n.years = 5,
                                 parameters = c(epsilon = eps, alpha = 100, 
                                                beta = 100, gamma = 1), verbose = TRUE)
plotManyRuns(sim.Combined, nrow = 1)




sim1 <- runManyYears(world_sinusoidal, 
                     threshold = 1, 
                     n.years = 8,
                     parameters = c(epsilon = .5, alpha = 0, 
                                    beta0 = 0, beta1 = 35), verbose = TRUE)
plotSim(sim1, nrow = 1)



sim2 <- runManyYears(world_sinusoidal, 
                     threshold = 1, 
                     n.years = 11,
                     parameters = c(epsilon = 1, alpha = 10, 
                                    beta0 = 5, beta1 = 40), verbose = TRUE)
plotSim(sim2, nrow = 2)

# s1 <- runNextYear(world_sinusoidal, c(epsilon = .2, alpha = 0, beta0 = 0, beta1 = 1000))
# image(s1)

