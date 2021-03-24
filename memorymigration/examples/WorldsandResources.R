## Worlds
world <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=1, 
                       peak.max = 20, peak.min = 20, sd = 10)


##Resources
fr1 <- dnorm(world$X, mean = 80, sd = 10) %>% {./sum(.)/world$dx}
fr2 <- dnorm(world$X, mean = 20, sd = 10) %>% {./sum(.)/world$dx}
fixed_Resource <- matrix((fr1 + fr2)/2, byrow = TRUE, 
                         nrow = nrow(world$pop), 
                         ncol = ncol(world$pop))

resource_R1 <- getPulsedResource(world, c(t.peak = 25, t.sd = 10, x.peak = 75, x.sd = 10))
resource_R2 <- getPulsedResource(world, c(t.peak = 25, t.sd = 10, x.peak = 75, x.sd = 4))
resource_R3 <- getPulsedResource(world, c(t.peak = 25, t.sd = 4, x.peak = 75, x.sd = 10))
resource_R4 <- getPulsedResource(world, c(t.peak = 25, t.sd = 4, x.peak = 75, x.sd = 4))

resource_R33 <- getPulsedResource(world, c(t.peak = 25, t.sd = 3, x.peak = 75, x.sd = 3))
resource_R36 <- getPulsedResource(world, c(t.peak = 25, t.sd = 3, x.peak = 75, x.sd = 6))
resource_R39 <- getPulsedResource(world, c(t.peak = 25, t.sd = 3, x.peak = 75, x.sd = 9))
resource_R312 <- getPulsedResource(world, c(t.peak = 25, t.sd = 3, x.peak = 75, x.sd = 12))
resource_R315 <- getPulsedResource(world, c(t.peak = 25, t.sd = 3, x.peak = 75, x.sd = 15))

resource_R63 <- getPulsedResource(world, c(t.peak = 25, t.sd = 6, x.peak = 75, x.sd = 3))
resource_R66 <- getPulsedResource(world, c(t.peak = 25, t.sd = 6, x.peak = 75, x.sd = 6))
resource_R69 <- getPulsedResource(world, c(t.peak = 25, t.sd = 6, x.peak = 75, x.sd = 9))
resource_R612 <- getPulsedResource(world, c(t.peak = 25, t.sd = 6, x.peak = 75, x.sd = 12))
resource_R615 <- getPulsedResource(world, c(t.peak = 25, t.sd = 6, x.peak = 75, x.sd = 15))

resource_R93 <- getPulsedResource(world, c(t.peak = 25, t.sd = 9, x.peak = 75, x.sd = 3))
resource_R96 <- getPulsedResource(world, c(t.peak = 25, t.sd = 9, x.peak = 75, x.sd = 6))
resource_R99 <- getPulsedResource(world, c(t.peak = 25, t.sd = 9, x.peak = 75, x.sd = 9))
resource_R912 <- getPulsedResource(world, c(t.peak = 25, t.sd = 9, x.peak = 75, x.sd = 12))
resource_R915 <- getPulsedResource(world, c(t.peak = 25, t.sd = 9, x.peak = 75, x.sd = 15))

resource_R123 <- getPulsedResource(world, c(t.peak = 25, t.sd = 12, x.peak = 75, x.sd = 3))
resource_R126 <- getPulsedResource(world, c(t.peak = 25, t.sd = 12, x.peak = 75, x.sd = 6))
resource_R129 <- getPulsedResource(world, c(t.peak = 25, t.sd = 12, x.peak = 75, x.sd = 9))
resource_R1212 <- getPulsedResource(world, c(t.peak = 25, t.sd = 12, x.peak = 75, x.sd = 12))
resource_R1215 <- getPulsedResource(world, c(t.peak = 25, t.sd = 12, x.peak = 75, x.sd = 15))

resource_R153 <- getPulsedResource(world, c(t.peak = 25, t.sd = 15, x.peak = 75, x.sd = 3))
resource_R156 <- getPulsedResource(world, c(t.peak = 25, t.sd = 15, x.peak = 75, x.sd = 6))
resource_R159 <- getPulsedResource(world, c(t.peak = 25, t.sd = 15, x.peak = 75, x.sd = 9))
resource_R1512 <- getPulsedResource(world, c(t.peak = 25, t.sd = 15, x.peak = 75, x.sd = 12))
resource_R1515 <- getPulsedResource(world, c(t.peak = 25, t.sd = 15, x.peak = 75, x.sd = 15))


save(resource_R1, resource_R2, resource_R3, resource_R4, fixed_Resource, 
     resource_R33, resource_R36, resource_R39, resource_R312, resource_R315,
     resource_R63, resource_R66, resource_R69, resource_R612, resource_R615,
     resource_R93, resource_R96, resource_R99, resource_R912, resource_R915,
     resource_R123, resource_R126, resource_R129, resource_R1212, resource_R1215,
     resource_R153, resource_R156, resource_R159, resource_R1512, resource_R1515,
     file = "memorymigration/data/resources.rda")

save(world, file = "memorymigration/data/world.rda")
par(mfrow = c(2,2))
image(resource_R1) 
image(resource_R2) 
image(resource_R3) 
image(resource_R4) 