rm(list = ls())
require(memorymigration)

world <- getOptimalPop(tau=100, X.min = -100, X.max = 100, dx=1, 
                       x1 = 30, x2 = -30, t.peak=25, 
                       x.sd=12, t.sd=6)

world$resource <- getResource_island(world,
                                     c(t.peak = 25, t.sd = 3,
                                       x.peak = 30, x.sd = 3))
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))

cc.params <- list(mu_x0 = 30, mu_t0 = 25,
               beta_x = -.5, beta_t = 0,
               n.years = 50, sigma_x = 12 ,
               sigma_t = 6, psi_x = 0, psi_t = 0) 

# lambda = 20-200
# beta = 100, 400

# alpha = 400


p0 <- c(epsilon = 4, alpha = 400, beta = 400, kappa = 0, lambda = 20)
#p0 <- c(epsilon = 8, alpha = 300, beta = 100, kappa = 0, lambda = 100)
p.cc <- with(cc.params, getCCpars(mu_x0, mu_t0, sigma_x, sigma_t, n.years, beta_x, beta_t, psi_x, psi_t))

world$resource <- aaply(p.cc, 1, function(p) getResource_island(world, p)) 
M0 <- runManyYears(world, parameters = p0, n.years = 20, threshold = 1, verbose = TRUE)
# computeSpatialAdaptationIndex(M0, cc.params, trim = 10)



plotManyRuns(M0$pop, world = world, nrow = 3)
source("text/figures/code/functions_plottingForMS.R")

length(M0$pop)
dim(world$resource)

n.years <- length(M0$pop)
FE <- computeAnnualEfficiency(M0$pop, world$resource, world)

par(mfrow = c(2,2), mar = c(2,2,2,2), bty = "l")
plotMigrationHatCC(M0$migration.hat, cc.params)
plot(FE)
error.df <- computeMigrationIndicesCC(M0, world, cc.params)
matplot(error.df, col = rich.colors(4), type = "o", pch = 19)


computeMigrationIndicesCC <- function(sim, world, cc.params){
  n.years <- length(sim$pop) - 1
  m.hat <- sim$migration.hat[1:(n.years+1),]
  m.hat$t1.hat <- cc.params$mu_t0 + cc.params$beta_t * 0:n.years
  m.hat$t2.hat <- 100-cc.params$mu_t0 + cc.params$beta_t * 0:n.years
  m.hat$x1.hat <- cc.params$mu_x0 - cc.params$beta_x * 0:n.years
  m.hat$x2.hat <- -m.hat$x1.hat
  t1.error <- with(m.hat, {
      ifelse(t1.hat > t1 & t1.hat < t1 + dt1, 0, 
             ifelse(t1.hat < t1, abs(t1.hat - t1), 
                    abs(t1.hat - (t1 + dt1))))    
  })
  t2.error <- with(m.hat, {
    ifelse(t2.hat > t2 & t2.hat < t2 + dt2, 0, 
           ifelse(t2.hat < t2, abs(t2.hat - t2), 
                  abs(t2.hat - (t2 + dt2))))    
  })
  x1.error <- m.hat$x1 - m.hat$x1.hat
  x2.error <- m.hat$x2 - m.hat$x2.hat
  data.frame(t1.error, t2.error, x1.error, x2.error)
}

