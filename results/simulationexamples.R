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

resource <- aaply(par0, 1, function(p) getResource_island(world, p))


rm(list=ls())

model.pars80 <- list(
  c(epsilon = 4, alpha = 100, beta=400, kappa = 0, lambda = 80),
  c(epsilon = 4, alpha = 100, beta=400, kappa = .8, lambda = 80),
  c(epsilon = 4, alpha = 100, beta=400, kappa = 1, lambda = 80))
  
  
model.pars120 <- list(
  c(epsilon = 4, alpha = 100, beta=400, kappa = 0, lambda = 120),
  c(epsilon = 4, alpha = 100, beta=400, kappa = .8, lambda = 120),
  c(epsilon = 4, alpha = 100, beta=400, kappa = 1, lambda = 120))


data(world_cc)

sims.80 <- lapply(model.pars80, function(p)
    runManyYears(world_cclambda80, parameters = p, n.years = 10,  
               threshold = 1, verbose=TRUE)
  )

sims.120 <- lapply(model.pars120, function(p)
  runManyYears(world_cclambda120, parameters = p, n.years = 10,  
               threshold = 1, verbose=TRUE)
)


ldply(sims.80, function(sim)
  computeEfficiency(sim$pop[[length(sim$pop)]], world_cclambda80$resource, world_cclambda80)
)

ldply(sims.120, function(sim)
  computeEfficiency(sim$pop[[length(sim$pop)]], world_cclambda120$resource, world_cclambda120)
)

l_ply(sims.80, function(sim) plotMigrationHat(sim$migration.hat))
l_ply(sims.120, function(sim) plotMigrationHat(sim$migration.hat))

# load("results/kappa06.rda")  # sim2
# load("results/kappa1.rda")  # sim3


world_cc <- getOptimalPop(tau=100, X.min = -100, X.max = 100, dx=1, 
                       x1 = 30, x2 = -30, t.peak=25, 
                       x.sd=12, t.sd=6)

makeMhatvector <- function(df){
  ((df %>% tail(1))[1:6] %>% as.matrix)[1,]
}

world_cc$m0 <- makeMhatvector(sim3$migration.hat)
world_cc$resource <- getResource_island(world_cc, c(x.peak = 30, t.peak = 25, x.sd = 12, t.sd = 6))

params_cclambda80 <- c(epsilon = 4, alpha = 100, beta=400, kappa = 0, lambda = 80)
params_cclambda120 <- c(epsilon = 4, alpha = 100, beta=400, kappa = 0, lambda = 120)

world_cc$pop <- sim3$pop[[length(sim3$pop)]]
sim0_cclambda80 <- runManyYears(world_cc, 
                                parameters = params_cclambda80, 
                                n.years = 100, 
                                threshold= 1-1e-6, 
                                verbose = TRUE)

sim0_cclambda120 <- runManyYears(world_cc, 
                                parameters = params_cclambda120, 
                                n.years = 100, 
                                threshold= 1-1e-6, 
                                verbose = TRUE)

world_cclambda120 <- world_cclambda80 <- world_cc

world_cclambda80$pop <- sim0_cclambda80$pop[[length(sim0_cclambda80$pop)]]
world_cclambda80$m0 <- makeMhatvector(sim0_cclambda80$migration.hat)

world_cclambda120$pop <- sim0_cclambda120$pop[[length(sim0_cclambda120$pop)]]
world_cclambda120$m0 <- makeMhatvector(sim0_cclambda120$migration.hat)

save(world_cclambda80, world_cclambda120, file = "memorymigration/data/world_cc.Rda")


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
