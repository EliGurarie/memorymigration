## Worlds
world <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=2, 
                       peak.max = 80, peak.min = 20, sd = 10)


##Resources
fr1 <- dnorm(world$X, mean = 80, sd = 10) %>% {./sum(.)/world$dx}
fr2 <- dnorm(world$X, mean = 20, sd = 10) %>% {./sum(.)/world$dx}
fixed.Resource <- matrix((fr1 + fr2)/2, byrow = TRUE, 
                         nrow = nrow(world$pop), 
                         ncol = ncol(world$pop))

resource.R1 <- getPulsedResource(1:world$tau, world$X, c(t.peak = 25, t.sd = 9, x.peak = 80, x.sd = 9))

resource.R2 <- getPulsedResource(1:world$tau, world$X, c(t.peak = 25, t.sd = 9, x.peak = 80, x.sd = 5))

resource.R3 <- getPulsedResource(1:world$tau, world$X, c(t.peak = 25, t.sd = 5, x.peak = 80, x.sd = 9))

resource.R4 <- getPulsedResource(1:world$tau, world$X, c(t.peak = 25, t.sd = 5, x.peak = 80, x.sd = 5))