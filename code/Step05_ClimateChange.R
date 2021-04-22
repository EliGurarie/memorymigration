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

#total taxis = 200
#population breaks down when resource following proportion is below 0.6 
#epsilon 1; alpha 140; beta 60; gamma 0.8 = 200
#epsilon 1; alpha 120; beta 80; gamma 0.8
#epsilon 1; alpha 140; beta 60; gamma 0.8
#epsilon 1; alpha 160; beta 40; gamma 0.8
#epsilon 1; alpha 120; beta 80; gamma 0.9
#epsilon 1; alpha 140; beta 60; gamma 0.7


