rm(list=ls())
require(memorymigration)

world <- world_sinusoidal

# set up climate change scenario

par0 <- getCCpars(mu_x0 = 80, 
                  mu_t0 = 25,
                  beta_x = 1,
                  beta_t = 0,
                  n.years = 10,
                  sigma_x = 3,
                  sigma_t = 12)

Resource.CC <- aaply(par0, 1, function(p) getPulsedResource_v2(world, p))
world$resource <- Resource.CC


sim <- runManyYears(world, parameters = c(epsilon = 1, 
                                    alpha = 60, 
                                    beta = 120, 
                                    gamma = .9),
             threshold = 1, n.years = 10, verbose = TRUE)

plotManyRuns(sim)
