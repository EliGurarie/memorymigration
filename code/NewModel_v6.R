
#rm(list=ls())
require(memorymigration)
source("code/functions_v6.R")
source("code/functions_discretemigration.R")
source("code/functions_plottingresults.R")

world <- getSinePop(tau = 100, peak.max = 70, peak.min = -70, sd = 10)
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
world$resource <- getResource_island(world, 
                                     c(t.peak = 25, t.sd = 5, 
                                       x.peak = 30, x.sd = 5))

image(world$resource)

p0 <- c(epsilon = 1, alpha = 100, kappa = 0, beta = 100, lambda = 30)
M0 <- runManyYears(world, parameters = p0, n.years = 40, 1, FUN = runNextYear, verbose = TRUE)
#M0 <- buildOnRuns(M0$pop, world, parameters = p0, n.years = 20, verbose = TRUE, FUN = runNextYear)
doublePlot(M0$pop, world)
plotManyRuns(M0$pop, world, nrow = 3)


mhat <- ldply(M0$m.hat) %>% mutate(year = 1:length(M0$pop) - 1)
plotMigrationHat(mhat)




world <- getSinePop(tau = 100, peak.max = 80, peak.min = -80, sd = 10)
world$resource <- getResource_drifting(world, 
                                     c(t.peak = 25, t.sd = 5, 
                                       x.peak = 50, x.sd = 12))
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
with(world, image.plot(time, X, resource))

p1 <- c(epsilon = 1, alpha = 1000, kappa = 0, beta = 100, lambda = 30)
M1 <- runManyYears(world, parameters = p1, n.years = 14, 1, FUN = runNextYear, verbose = TRUE)
M1 <- buildOnRuns(M1, world, parameters = p1, n.years = 30, verbose = TRUE, FUN = runNextYear)
plotManyRuns(M1$pop, world, nrow = 2)
doublePlot(M1$pop, world)

mhat <- ldply(M1$m.hat) %>% mutate(year = 1:length(M1$pop) - 1)

plotMigrationHat(mhat)
