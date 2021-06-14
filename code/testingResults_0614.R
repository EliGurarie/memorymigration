 i <- 40

computeMigratoriness(M0$pop[[i]], world)
image(M0$pop[[i]])


MI <- sapply(M0$pop, function(p) computeMigratoriness(p, world)$overlap)
plot(MI)


load("results/climate_space/climate_space_betax_0.rda")
r.betax0 <- newresults
load("results/climate_space/climate_space_betax_1.rda")
r.betax1 <- newresults
load("results/climate_space/climate_space_betax_3.rda")
r.betax3 <- newresults


with(r.betax0, table(beta, epsilon, alpha))
require(ggthemes)
r.betax0 %>% 
  ggplot(aes(alpha, beta, fill = FE)) + 
  facet_grid(lambda~epsilon) + 
  geom_tile() + theme_few() + ggtitle("no climate change")

r.betax1 %>% 
  ggplot(aes(alpha, beta, fill = FE)) + 
  facet_grid(lambda~epsilon) + 
  geom_tile() + theme_few()+ ggtitle("medium climate change")


r.betax3 %>% 
  ggplot(aes(alpha, beta, fill = FE)) + 
  facet_grid(lambda~epsilon) + 
  geom_tile() + theme_few()+ ggtitle("high climate change")


world <- getOptimalPop(tau=100, X.min = -100, X.max = 100, dx=1, 
                       x1 = 60, x2 = -60, t.peak=25, 
                       x.sd=5, t.sd=12)
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))


resource_param <- expand.grid(mu_x0 = 60, mu_t0 = 25,
                              beta_x = 0, beta_t = 0,
                              n.years = 50, sigma_x = 10,
                              sigma_t = 5,
                              psi_x = 0, psi_t = 0)

j <- 1
par0 <- with(resource_param, 
             getCCpars(mu_x0 = mu_x0[j],
                  mu_t0 = mu_t0[j],
                  beta_x = beta_x[j],
                  beta_t = beta_t[j],
                  n.years = n.years[j],
                  sigma_x = sigma_x[j],
                  sigma_t = sigma_t[j],
                  psi_x = psi_x[j], 
                  psi_t = psi_t[j]))

world$resource <- aaply(par0, 1, function(p) getResource_island(world, p)) 
attr(world$resource, "par") <- par0[nrow(par0),]


p0 <- c(epsilon = 1, alpha = 100, kappa = 0, beta = 100, lambda = 40)
M0 <- runManyYears(world, parameters = p0, n.years = 50, .9999, verbose = TRUE)

par(mfrow = c(1,2))
plotMigrationHat(M0$migration.hat, 60, 25, par = TRUE)
plotManyRuns(M0$pop, world, nrow = 4)

M0b <- buildOnRuns(M0, world, parameters = p0, n.years = 10, verbose =TRUE)
par(mfrow = c(1,2))
plotMigrationHat(M0b$migration.hat, 60, 25, par = TRUE)
plotManyRuns(M0b$pop, world, nrow = 4)


#M0 <- M0b

fitMigration(t = world$time, x = getMem(M0$pop[[33]], world), 
             m.start = M0$migration.hat[32,1:6])

p <- runFWM(world = world, parameters = p0, 
      pop.lastyear = M0$pop[[33]], 
      Year = 33, m.hat = M0$migration.hat[33,])


image(p)
fitMigration(t = world$time, x = getMem(p, world), 
             m.start = M0$migration.hat[33,1:6])



x <- getMem(M0$pop[[33]], world)
t <- world$time  
  
nlsLM(x ~ stepMigration(t, t1, dt1, t2, dt2, x1, x2, tau = 100), 
      start = as.list(m.start),
      lower = c(-100,0,0,0,-100,-100), 
      upper = c(100,100,100,100,100,100)) 
