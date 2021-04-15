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



png("text/figures/ScanningResourceMemoryRatios_part1.png", width = 2000, height = 2000, res = 300)
par(mfrow = c(length(ratios), max(ns)), mar = c(0,0,1,0), oma = c(4,2,4,2), tck = 0.01, xpd = NA)
for(i in seq(11,1,-2)){
  plotManyRuns(SimSet2[[i]], par = FALSE, outer = FALSE, 
               ylab = paste((1-ratios[i])*100, "%"), line = 0.5, labelyears = i == 6)
}
mtext(side = 3, "all resource following", outer = TRUE, line = 1.5)
mtext(side = 1, "all reference memory", outer = TRUE, line = 1)
dev.off()



require(gplots)


getMedian.daily <- function(sim, plotme = TRUE, ...){
  
  get_median <- function(x){
    f <- function(p)
      (splinefun((1:length(x))-.5, cumsum(x))(p) - 0.5)^2
    optimize(f, c(0, length(x)))$minimum
  }
  
  median <- apply(sim, 1, get_median)
  
  if(plotme) plot(1:nrow(sim), median, type = "l", ..., xlab = "time", ylim = c(0,nrow(sim)))
  data.frame(median)
}

getFE.year <- function(run, world){
  FE <- c()
  for(i in 2:length(run))
    FE <- c(FE, computeEfficiency(run[[i]], world$resource, world = world))
  return(data.frame(year = (2:length(run))-1, FE = FE))
}

summarizeSweep <- function(SimSet){
  sim.FEs <- ldply(SimSet, getFE.year, world = world_sinusoidal, .id = "ratio")
  sim.Medians <- ldply(SimSet, function(l) getMedian.daily(l[[length(l)]], plotme = FALSE),
                       .id = "ratio")
  MI.final <- apply(sim.Medians[,-1], 1, function(x) diff(range(x)))
  FE.final <- ddply(sim.FEs, "ratio", function(df) tail(df, 1))
  data.frame(FE.final, MI = MI.final, n = sapply(SimSet, length)) %>% 
    mutate(ratio = as.numeric(as.character(ratio))) %>% arrange(ratio)
}

eval <- FALSE
if(eval){
  ratios <- seq(0,1,length = 21)
  SimSet.threshold <- scanResourceMemoryRatios(world_sinusoidal, epsilon = 1, 
                                               alphabeta = 200, threshold = .99999, 
                                               n.years = 20,
                                               ratios = ratios, verbose = TRUE)
  names(SimSet.threshold) <- ratios
  ns <- sapply(SimSet.threshold, length)
  save(SimSet.threshold, ns, ratios, file = "results/memoryforagingsweep/SimSet_ratios_beta200.rda")
} else load("results/memoryforagingsweep/SimSet_ratios_beta200.rda")
Sim200 <- SimSet.threshold

if(eval){
  ratios <- seq(0,1,length = 21)
  SimSet.threshold <- scanResourceMemoryRatios(world_sinusoidal, epsilon = 1, 
                                               alphabeta = 150, threshold = .99999, 
                                               n.years = 20,
                                               ratios = ratios, verbose = TRUE)
  names(SimSet.threshold) <- ratios
  ns <- sapply(SimSet.threshold, length)
  save(ratios, ns, SimSet.threshold, file = "results/memoryforagingsweep/SimSet_ratios_beta150.rda")
} else load("results/memoryforagingsweep/SimSet_ratios_beta150.rda")
Sim150 <- SimSet.threshold

if(eval){
  ratios <- seq(0,1,length = 21)
  SimSet.threshold <- scanResourceMemoryRatios(world_sinusoidal, epsilon = 1, 
                                               alphabeta = 100, threshold = .99999, 
                                               n.years = 20,
                                               ratios = ratios, verbose = TRUE)
  names(SimSet.threshold) <- ratios
  ns <- sapply(SimSet.threshold, length)
  save(ratios, ns, SimSet.threshold, file = "results/memoryforagingsweep/SimSet_ratios_beta100.rda")
} else load("results/memoryforagingsweep/SimSet_ratios_beta100.rda")
Sim100 <- SimSet.threshold

if(eval){
   ratios.fillin <- ratios[-length(ratios)] + .05
   SimSet.threshold.fillin <- scanResourceMemoryRatios(world_sinusoidal, epsilon = 1, 
                                               alphabeta = 100, threshold = .99999, 
                                               n.years = 20,
                                               ratios = ratios.fillin, verbose = TRUE)
   names(SimSet.threshold.fillin) <- ratios.fillin
   ratios <- names(SimSet.threshold)
   for(r in ratios.fillin) SimSet.threshold[as.character(r)] <- SimSet.threshold.fillin[as.character(r)]
} 


sims.df <- list(beta200 = Sim200, beta150 = Sim150, beta100 = Sim100) %>% 
  ldply(summarizeSweep) %>% mutate(alphabeta = factor(.id))

require(ggplot2)
p1 <- ggplot(sims.df, aes(ratio*100, MI, col = .id)) + geom_path() + geom_point()
p2 <- ggplot(sims.df, aes(ratio*100, FE, col = .id)) + geom_path() + geom_point()
require(gridExtra)
grid.arrange(p1, p2)

{
  png("text/figures/FinalMigratoryFollowing.png", height=2000, width=1400, res = 300)
  palette(rich.colors(4)[c(1,2,4)])

  par(mfrow = c(2,1), mar = c(2,3,0,2), bty = "l", oma = c(2,1,3,0), 
    tck = 0.01, mgp = c(2,.25,0), cex.lab  = 1.25, bty = "l", las = 1, xpd = NA)
  with(sims.df, 
    plot(ratio, MI, type = "n", pch = 19, cex = 2, 
         xlab = "", ylab = "migratoriness", xaxt = "n"))
  d_ply(sims.df, "alphabeta", 
        function(df) with(df, lines(ratio, MI, col = as.integer(alphabeta), pch = 19, type = "o"))
        )
  text(-.05,67,"100% reference memory", xpd = NA, pos = 4, font = 3)
  text(0.90,10,"100% \nresource\nfollowing", xpd = NA, pos = 4, font = 3)
  
  legend("topright", title = "total taxis", legend = c(100,150,200), col = 1:3, pch = 19, lty = 1)
  
  with(sims.df, plot(ratio, FE, type = "n", pch = 19, cex = 2, 
            xlab = expression(alpha/(beta + alpha)), ylab = "foraging efficiency"))
  d_ply(sims.df, "alphabeta", 
        function(df) with(df, lines(ratio, FE, col = as.integer(alphabeta), pch = 19, type = "o"))
  )
  dev.off()
}

plotSomeSims <- function(SimSet){
  n.sims <- length(SimSet)
  ns <- sapply(SimSet, length)
  par(mfrow = c(5,max(ns)), mar = c(0,0,1,0), oma = c(4,2,4,2), tck = 0.01, xpd = NA)
  for(i in seq(1,n.sims,length = 5) %>% round){
    s <- SimSet[[i]]
    empty <- max(ns)-length(s)
    plotManyRuns(s, par = FALSE, outer = FALSE, 
                 ylab = paste((1-ratios[i])*100, "%"), 
                 line = 0.5, labelyears = FALSE)
    if(empty > 0)
      for(i in 1:empty) plot.new()
  }
}

plotSomeSims(Sim100)
plotSomeSims(Sim150)
plotSomeSims(Sim200)


png("text/figures/ScanningResourceMemoryRatios_part2.png", width = 2400, height = 1200, res = 300)
  matplot(t(sim.FEs), type = "o", col = 1:length(ratios), lty = 1, pch = 19, 
          ylab = "foraging efficiency", xlab = "years")
  legend("bottomright", col = 1:length(ratios), pch = 19, legend = paste(ratios*100, "%"), 
         cex = 0.8, title = "percent resource \nfollowing", bty = "n")
  
  matplot(t(sim.Medians[,-1]), type = "l", col = 1:length(ratios), lty = 1, lwd = 2, 
          ylab = "median location", xlab = "days")
dev.off()
  
  

data(world)
data(resources_island)
world_gaussian$resource <- resources_island[["R_t12_x3"]]
image(world_gaussian$resource)

world_sinusoidal <- getSinePop(100, peak.max = 80, peak.min = 20, sd = 3, dx = 1)
world_sinusoidal$resource <- resources_island[["R_t12_x3"]]

s <- runManyYears(world_sinusoidal, 
             threshold = 1, n.years = 5,
             parameters = c(epsilon = 1, 
                            alpha = 200, 
                            beta = 0, 
                            gamma = 1))

plotManyRuns(s)
