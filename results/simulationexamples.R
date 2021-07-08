world <- getOptimalPop(tau=100, X.min = -100, X.max = 100, dx=1, 
                       x1 = 30, x2 = -30, t.peak=25, 
                       x.sd=12, t.sd=6)
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))

par0 <- getCCpars(mu_x0 = 30, 
                  mu_t0 = 25,
                  beta_x = -0.5,
                  beta_t = 0,
                  n.years = 30,
                  sigma_x = 12,
                  sigma_t = 6,
                  psi_x = 3, 
                  psi_t = 0,
                  n.years.null = 20) 
world$resource <- aaply(par0, 1, function(p) getResource_island(world, p))


parameters <- c(epsilon = 4, alpha = 100, beta=400, kappa = 1, lambda = 80)

eval <- FALSE
if(eval){  
  sim <- runManyYears(world, parameters = parameters, n.years = 50, threshold = 1, verbose=TRUE)
  sim1 <- runManyYears(world, parameters = parameters, n.years = 50, threshold = 1, verbose=TRUE)
  sim2 <- runManyYears(world, parameters = parameters, n.years = 50, threshold = 1, verbose=TRUE)
  sim3 <- runManyYears(world, parameters = parameters, n.years = 50, threshold = 1, verbose=TRUE)
}

load("results/kappa06.rda")  # sim2
load("results/kappa1.rda")  # sim3



plotManyRuns(sim1$pop, world, nrow = 3)
plotManyRuns(sim3$pop, world, nrow = 3)

FE <- data.frame()
for(i in 2:length(sim2$pop)){
compFE <- computeEfficiency(sim2$pop[[i]], world$resource[i-1,,],world)
FE <- rbind(FE, compFE)
}

FE2 <- data.frame()
for(i in 2:length(sim2$pop)){
  compFE <- computeEfficiency(sim3$pop[[i]], world$resource[i-1,,],world)
  FE2 <- rbind(FE2, compFE)
}

plot(FE[,1], type = "o")
lines(FE2[,1], type = "o", col = 2)


image(sim3$pop[[50]])
contour(world$resource[47,,], add = TRUE)


image(sim2$pop[[50]])
contour(world$resource[50,,], add = TRUE)
