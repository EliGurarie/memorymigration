rm(list=ls())
require(memorymigration)
#source("code/functions_v6.R")
#source("code/functions_discretemigration.R")
#source("code/functions_plottingresults.R")
require(minpack.lm); require(fields); require(scales)

#############################
# Example 1: Adaptation
#############################


eval <- FALSE
if(eval)
{
  world <- getOptimalPop(tau = 100, x1 = 50, x2 = -50, 
                         t.peak = 25, t.sd = 12, x.sd = 5)
  world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
  world$resource <- getResource_island(world, 
                                       c(t.peak = 25, t.sd = 5, 
                                         x.peak = 30, x.sd = 10))
  
  par(mfrow = c(1,2))
  with(world, {
    image.plot(time, X, pop)
    image.plot(time, X, resource)
  })
  
  p0 <- c(epsilon = 1, alpha = 100, kappa = 0, beta = 100, lambda = 40)
  M0 <- runManyYears(world, parameters = p0, n.years = 40, 1, 
                     verbose = TRUE)
  save(world, M0, file = "results/msexamples/adaptation.rda")
}

load("results/msexamples/adaptation.rda")
source("text/figures/code/functions_plottingForMS.R")

n <- 14

png("text/figures/example1_adaptation.png", 
    width = 3000, height = 3000, res = 300)

{
  M <- rbind(1:n, rep(n+1:2, each = n/2), rep(n+3:4, each = n/2))
  layout(M)
  
  par(oma = c(2,4,2,2), mgp = c(2,.25,0), las = 1, mar = c(4,0,0,0), 
      xpd = NA, tck = 0, bty = "l")
  M0.example <- M0$pop[paste0("Year",1:n - 1)] %T>% 
    plotSomeYears(world, labelyears = TRUE, par = TRUE)
  
  par(mar = c(3,4,2,2), xpd = 0, cex.lab = 1.75, cex.axis = 1.25)
  doublePlot(M0$pop, world, par = TRUE)
  plotMigrationHat(M0$migration.hat, 30, 25, par = TRUE)
}

dev.off()


#####################################
# Example 2: Learning to migrate
######################################


eval <- FALSE
if(eval)
{
  world <- getSinePop(tau = 100, peak.max = 1, peak.min = -1, sd = 10)
  world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
  world$resource <- getResource_drifting(world, 
                                       c(t.peak = 25, t.sd = 12, 
                                         x.peak = 50, x.sd = 10))
  par(mfrow = c(1,2))
  with(world, {
    image.plot(time, X, pop)
    image.plot(time, X, resource)
  })
  p0 <- c(epsilon = 4, alpha = 500, kappa = 0, beta = 50, lambda = 40)
  M0 <- runManyYears(world, parameters = p0, n.years = 100, 1, verbose = TRUE)
  
  #m0 <- M0$migration.hat[nrow(M0$migration.hat),] %>% mutate(year = NULL) %>% 
  #  as.list %>% unlist
  #M0b <- M0 %>% buildOnRuns(world, parameters = p0, n.years = 50, verbose = TRUE, m0 = m0)
  #M0 <- M0b
  save(world, M0, file = "results/msexamples/learningtomigrate.rda")
}
load("results/msexamples/learningtomigrate.rda")

M0.example <- M0$pop[paste0("Year",seq(0,90,10))] %T>% 
  plotSomeYears(world, labelyears = TRUE, nrow = 2)
doublePlot(M0$pop, world, par = FALSE)
plotMigrationHat(M0$memory.hat, x.peak = 50, t.peak = 25, par = FALSE, ylim1 = c(0,120))



world <- getSinePop(tau = 100, peak.max = 1, peak.min = -1, sd = 10)
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
world$resource <- getResource_drifting(world, 
                                       c(t.peak = 25, t.sd = 12, 
                                         x.peak = 30, x.sd = 9))
par(mfrow = c(1,2))
with(world, {
  image.plot(time, X, pop)
  image.plot(time, X, resource)
})
p0 <- c(epsilon = 1, alpha = 400, kappa = 0, beta = 800, lambda = 80)
M0 <- runManyYears(world, parameters = p0, n.years = 100, .9999, verbose = TRUE)

M0$pop[paste0("Year",seq(0,90,10))] %>% plotSomeYears(world, labelyears = TRUE, nrow = 2)
doublePlot(M0$pop, world, par = FALSE)
plotMigrationHat(M0$memory.hat, x.peak = 30, t.peak = 25, par = FALSE, ylim1 = c(0,120))




fitMigration <- function(t, x, m.start = NULL, tau = 100){
  if(is.null(m.start)) m.start <- c(t1 = 15, dt1 = 30, t2 = 55, dt2 = 30, x1 = min(x), x2 = max(x))
  migration.fit <- nlsLM(x ~ stepMigration(t, t1, dt1, t2, dt2, x1, x2, tau = 100), 
                         start = as.list(m.start),
                         lower = c(-100,0,0,0,-100,-100), 
                         upper = c(100,100,200,100,100,100)) 
  summary(migration.fit)$coef[,1]
}

t <- world$time
x <- getMem(M0$pop[[64]], world)
m.start <- M0$migration.hat[64,1:6]

plot(t,x)
