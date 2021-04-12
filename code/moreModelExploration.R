require("memorymigration")

data(world)
data(resources_island)
world_gaussian$resource <- resources_island[["R_t12_x3"]]
image(world_gaussian$resource)

world_sinusoidal <- getSinePop(100, peak.max = 80, peak.min = 20, sd = 3, dx = 1)
world_sinusoidal$resource <- resources_island[["R_t12_x3"]]

eps <- 2
n.years <- 11
magnitude <- 50
nrows <- 2

runSetOfSims <- function(world, epsilon, alphabeta, threshold = 1, 
                    gamma_mixed = 0.9, n.years = 5, ...){
  list(
  sim.Resource = runManyYears(world, 
                               threshold = threshold, n.years = n.years,
                               parameters = c(epsilon = epsilon, alpha = alphabeta, 
                                              beta = 0, gamma = 0), ...),
  sim.Memory = runManyYears(world, 
                             threshold = threshold, n.years = n.years,
                             parameters = c(epsilon = epsilon, alpha = 0, 
                                            beta = alphabeta, gamma = 1), ...),
  sim.CombinedI = runManyYears(world, 
                                threshold = threshold, n.years = n.years,
                                parameters = c(epsilon = epsilon, alpha = alphabeta/2, 
                                               beta = alphabeta/2, gamma = 1), ...),
  sim.CombinedII = runManyYears(world, 
                                 threshold = threshold, n.years = n.years,
                                 parameters = c(epsilon = epsilon, alpha = alphabeta/2, 
                                                beta = alphabeta/2, gamma = gamma_mixed), ...))
}

getFE.ts <- function(run, world){
  FE <- c()
  for(i in 2:length(run))
    FE <- c(FE, computeEfficiency(run[[i]], world$resource, world = world))
  return(FE)
}



getMedianSim <- function(sim, plotme = TRUE, ...){
  
  get_median <- function(x){
    f <- function(p)
      (splinefun((1:length(x))-.5, cumsum(x))(p) - 0.5)^2
    optimize(f, c(0, length(x)))$minimum
  }
  
  median <- apply(sim, 1, get_median)
  if(plotme) plot(1:nrow(sim), median, type = "l", ..., xlab = "time", ylim = c(0,nrow(sim)))
  median
}

SimSet <- runSetOfSims(world_sinusoidal, epsilon = 1, 
                       alphabeta = 100, 
                       threshold = 1, 
                       n.years = 10, 
                       verbose = TRUE,
                       gamma_mixed = 0.95)

ns <- sapply(SimSet, length)
par(mfrow = c(4, max(ns)), mar = c(0,0,3,0), oma = c(2,2,4,2), tck = 0.01)
lapply(SimSet, plotManyRuns, nrow = 1, par = FALSE, outer = FALSE)

par(mfrow = c(1,2), mar = c(4,4,3,2), bty = "l", oma = c(0,0,0,0))
sim.FEs <- ldply(SimSet, getFE.ts, world = world_sinusoidal)
matplot(t(sim.FEs[,-1]), type = "o", col = 1:4, lty = 1, pch = 19 ,
        ylab = "foraging efficiency", xlab = "years")
legend("bottomright", col = 1:4, pch = 19, sim.FEs$.id)

sim.Medians <- ldply(SimSet, function(l) getMedianSim(l[[length(l)]], plotme = FALSE))
matplot(t(sim.Medians[,-1]), type = "l", col = 1:4, lty = 1, lwd = 2, 
        ylab = "median location", xlab = "days")

# Scan best ratio

require(memorymigration)
require(plyr)

scanResourceMemoryRatios <- function(world, epsilon, alphabeta, threshold = 1, 
                                     n.years = n.years, ratios = seq(0,1,.2), ...){
  llply(ratios, 
        function(r)
          sim.Resource = runManyYears(world, 
                                threshold = threshold, n.years = n.years,
                                parameters = c(epsilon = epsilon, 
                                               alpha = r*alphabeta, 
                                               beta = (1-r)*alphabeta, 
                                               gamma = 1), ...))
}


ratios <- seq(0,1,.2)
SimSet2 <- scanResourceMemoryRatios(world_sinusoidal, epsilon = 1, 
                                    alphabeta = 200, threshold = 1, 
                                    ratios = ratios, n.years = 6, verbose = TRUE)

ns <- sapply(SimSet2, length)

png("text/figures/ScanningResourceMemoryRatios_part1.png", width = 2000, height = 2000, res = 300)
par(mfrow = c(length(ratios), max(ns)), mar = c(0,0,1,0), oma = c(4,2,4,2), tck = 0.01, xpd = NA)
for(i in 1:length(ratios)){
  plotManyRuns(SimSet2[[i]], par = FALSE, outer = FALSE, 
               ylab = paste(ratios[i]*100, "%"), line = 0.5, labelyears = i == 1)
}
mtext(side = 3, "all reference memory", outer = TRUE, line = 1.5)
mtext(side = 1, "all resource following", outer = TRUE, line = 1)
dev.off()



require(gplots)
palette(rich.colors(length(ratios)))

png("text/figures/ScanningResourceMemoryRatios_part2.png", width = 2400, height = 1200, res = 300)
  par(mfrow = c(1,2), mar = c(3,3,3,2), bty = "l", oma = c(0,0,0,0), 
      tck = 0.01, mgp = c(1.25,.25,0), cex.lab  = 1.25, bty = "l")
  sim.FEs <- ldply(SimSet2, getFE.ts, world = world_sinusoidal)
  matplot(t(sim.FEs), type = "o", col = 1:length(ratios), lty = 1, pch = 19, 
          ylab = "foraging efficiency", xlab = "years")
  legend("bottomright", col = 1:length(ratios), pch = 19, legend = paste(ratios*100, "%"), 
         cex = 0.8, title = "percent resource \nfollowing", bty = "n")
  
  sim.Medians <- ldply(SimSet2, function(l) getMedianSim(l[[length(l)]], plotme = FALSE))
  matplot(t(sim.Medians[,-1]), type = "l", col = 1:length(ratios), lty = 1, lwd = 2, 
          ylab = "median location", xlab = "days")
dev.off()
  
  