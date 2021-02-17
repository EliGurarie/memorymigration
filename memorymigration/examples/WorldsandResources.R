## Worlds
world <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=1, 
                       peak.max = 80, peak.min = 20, sd = 10)


##Resources
fr1 <- dnorm(world$X, mean = 80, sd = 10) %>% {./sum(.)/world$dx}
fr2 <- dnorm(world$X, mean = 20, sd = 10) %>% {./sum(.)/world$dx}
fixed_Resource <- matrix((fr1 + fr2)/2, byrow = TRUE, 
                         nrow = nrow(world$pop), 
                         ncol = ncol(world$pop))

resource_R1 <- getPulsedResource(1:world$tau, world$X, 
                                 c(t.peak = 25, t.sd = 10, x.peak = 80, x.sd = 10))
resource_R2 <- getPulsedResource(1:world$tau, world$X, 
                                 c(t.peak = 25, t.sd = 10, x.peak = 80, x.sd = 4))
resource_R3 <- getPulsedResource(1:world$tau, world$X, 
                                 c(t.peak = 25, t.sd = 4, x.peak = 80, x.sd = 10))
resource_R4 <- getPulsedResource(1:world$tau, world$X, 
                                 c(t.peak = 25, t.sd = 4, x.peak = 80, x.sd = 4))


save(resource_R1, resource_R2, resource_R3, resource_R4, fixed_Resource, 
     file = "memorymigration/data/resources.rda")

save(world, file = "memorymigration/data/world.rda")
par(mfrow = c(2,2))
image(resource_R1) 
image(resource_R2) 
image(resource_R3) 
image(resource_R4) 