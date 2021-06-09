
rm(list=ls())
require(memorymigration)
#source("code/functions_v6.R")
#source("code/functions_discretemigration.R")
#source("code/functions_plottingresults.R")
require(minpack.lm); require(fields); require(scales)


world <- getSinePop(tau = 100, peak.max = 70, peak.min = -70, sd = 10)
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
world$resource <- getResource_island(world, 
                                     c(t.peak = 25, t.sd = 5, 
                                       x.peak = 30, x.sd = 5))

image(world$resource)

p0 <- c(epsilon = 1, alpha = 200, kappa = 0, beta = 100, lambda = 30)
M0 <- runManyYears(world, parameters = p0, n.years = 40, 1, FUN = runNextYear, verbose = TRUE)
#M0 <- buildOnRuns(M0$pop, world, parameters = p0, n.years = 20, verbose = TRUE, FUN = runNextYear)
doublePlot(M0$pop, world)
plotManyRuns(M0$pop, world, nrow = 3)
plotMigrationHat(M0$m.hat, 30, 25)




world <- getOptimalPop(tau = 100, x1 = 50, x2 = -50, 
                       t.peak = 25, t.sd = 12, x.sd = 12)
world$resource <- getResource_drifting(world, 
                                     c(t.peak = 25, t.sd = 12, 
                                       x.peak = 50, x.sd = 12))
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))

par(mfrow = c(1,2))
with(world, {
 image.plot(time, X, pop)
 abline(v = c(m0["t1"] + c(0, m0["dt1"]), m0["t2"] + c(0, m0["dt2"])), lwd = 2)
 abline(h = c(m0["x1"], m0["x2"]), lwd = 2)
 image.plot(time, X, resource)
}) 



with(world, {
  par(mfrow = c(1,2))
  image.plot(time, X, pop)
  abline(v = c(m0["t1"] + c(0, m0["dt1"]), m0["t2"] + c(0, m0["dt2"])), lwd = 2)
  abline(h = c(m0["x1"], m0["x2"]), lwd = 2)
  
  image.plot(time, X, y1)
  m1 <- getMigrationParameters(m0, y1, p1["kappa"], 2, world)
  abline(v = c(m1["t1"] + c(0, m1["dt1"]), m1["t2"] + c(0, m1["dt2"])), lwd = 2)
  abline(h = c(m1["x1"], m1["x2"]), lwd = 2)
}) 


p1 <- c(epsilon = 5, alpha = 1000, kappa = 0, beta = 100, lambda = 30)
M1 <- runManyYears(world, parameters = p1, n.years = 5, 1, FUN = runNextYear, verbose = TRUE)

plotManyRuns(M1$pop, world, nrow = 2)
doublePlot(M1$pop, world)
plotMigrationHat(M1$m.hat, 50, 25)



p2 <- c(epsilon = 5, alpha = 1000, kappa = 0, beta = 100, lambda = 100)
M2 <- runManyYears(world, parameters = p2, n.years = 2, 1, FUN = runNextYear, verbose = TRUE)
M2 <- buildOnRuns(M2, world, parameters = p2, n.years = 20, verbose = TRUE, FUN = runNextYear)

plotManyRuns(M2$pop, world, nrow = 2)
doublePlot(M2$pop, world)

m.hat <- ldply(M2$m.hat, .id = "year") %>% mutate(year = 1:length(M2$m.hat) - 1)
plotMigrationHat(M3$m.hat, 50, 25)


world <- getSinePop(tau = 100, peak.max = 1, peak.min = -1, sd = 10)
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
world$resource <- getResource_drifting(world, 
                                     c(t.peak = 25, t.sd = 12, 
                                       x.peak = 30, x.sd = 12))

image(world$resource)

p0 <- c(epsilon = 1, alpha = 100, kappa = 1, beta = 100, lambda = 20)
M0 <- runManyYears(world, parameters = p0, n.years = 10, 1, FUN = runNextYear, verbose = TRUE)
#M0 <- buildOnRuns(M0$pop, world, parameters = p0, n.years = 20, verbose = TRUE, FUN = runNextYear)
doublePlot(M0$pop, world)
plotManyRuns(M0$pop, world, nrow = 3)


plotMigrationHat(M0$m.hat, 50, 25)

