## Worlds
world_gaussian <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=1, 
                       peak.max = 20, peak.min = 20, sd = 10)
world_sinusoidal <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=1, 
                    peak.max = 80, peak.min = 20, sd = 10)
save(world_gaussian, world_sinusoidal, file = "memorymigration/data/world.rda")


## Fixed (bimodal) resource example

        fr1 <- dnorm(world$X, mean = 80, sd = 10) %>% {./sum(.)/world$dx}
        fr2 <- dnorm(world$X, mean = 20, sd = 10) %>% {./sum(.)/world$dx}
        fixed_Resource <- matrix((fr1 + fr2)/2, byrow = TRUE, 
                         nrow = nrow(world$pop), 
                         ncol = ncol(world$pop))
        image(fixed_Resource)

## Drifting resource
        
        t.sds <- seq(3,15,3)
        x.sds <- seq(3,15,3)
        
        require(plyr)
        tx.sds<- expand.grid(t.sds, x.sds) %>% plyr::rename(c(Var1 = "t.sd", Var2 = "x.sd")) %>% 
                mutate(combination = paste0("R_t",t.sd,"_x",x.sd))
        resources_drifting <- dlply(tx.sds, "combination",
              function(df) getPulsedResource(world, par = c(t.peak = 25, t.sd = df$t.sd, 
                                                            x.peak = 75, x.sd = df$x.sd)))
        save(resources_drifting, file = "memorymigration/data/resources_drifting.rda")
        
        par(mfrow = c(2,2))
        with(resources_drifting, {
                image(R_t3_x3) 
                image(R_t3_x12) 
                image(R_t12_x3) 
                image(R_t12_x12) 
        })

## Independent (Island) resource
        
        t.sds <- seq(3,15,3)
        x.sds <- seq(3,15,3)
        
        require(plyr)
        tx.sds<- expand.grid(t.sds, x.sds) %>% plyr::rename(c(Var1 = "t.sd", Var2 = "x.sd")) %>% 
                mutate(combination = paste0("R_t",t.sd,"_x",x.sd))
        resources_island <- dlply(tx.sds, "combination",
                            function(df) getPulsedResource_v2(world, par = c(t.peak = 25, t.sd = df$t.sd, 
                                                                          x.peak = 75, x.sd = df$x.sd)))
       save(resources_island, file = "memorymigration/data/resources_island.rda")
        
        par(mfrow = c(2,2))
        with(resources_island, {
                image(R_t6_x6) 
                image(R_t6_x12) 
                image(R_t12_x6) 
                image(R_t12_x12) 
        })
        