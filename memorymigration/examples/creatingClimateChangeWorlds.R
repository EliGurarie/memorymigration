world <- getOptimalPop(tau=100, X.min = -100, X.max = 100, dx=1, 
                       x1 = 30, x2 = -30, t.peak=25, 
                       x.sd=12, t.sd=6)
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
resource.null <- getResource_island(world, c(x.peak = 30, t.peak = 25, x.sd = 12, t.sd = 6))
world$resource <- resource.null

# run baseline no climate change for 100 years
sim3 <- runManyYearsWithStabilization(world, parameters = model.pars[[3]], 
                                        n.years = 30,  
                                        n.years.null = 100, 
                                        resource.null = resource.null, 
                                        threshold = 0.999999, 
                                        verbose=TRUE)
plotMigrationHat(sim3$migration.hat)


# create 2 worlds for climate change runs

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