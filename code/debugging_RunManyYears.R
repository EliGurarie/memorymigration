rm(list=ls())
require(memorymigration)

world <- getSinePop(tau = 100, peak.max = 1, peak.min = -1, sd = 10)
world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))

world$resource <- getResource_drifting(world, 
                                       c(t.peak = 25, t.sd = 9, 
                                         x.peak = 30, x.sd = 3), 
                                       x.null = 50)

par(mfrow = c(1,2))
with(world, {
  image.plot(time, X, pop)
  image.plot(time, X, resource)
})

p0 <- c(epsilon = 1, alpha = 400, kappa = 0, beta = 600, lambda = 80)
M0 <- runManyYears(world, parameters = p0, n.years = 40, 0.9999, 
                   verbose = TRUE)

plot

parameters <- p0
m0 <- world$m0

migration.list <- list(m0)
memory.list <- list(m0)

pop.list <- list(world$pop)
similarity <- 0
i <- 1 
{
  if(verbose){cat("\n"); cat(paste("running year ", i))}
  i <- i+1
  pop.list[[i]] <- runFWM(world = world, parameters = parameters, 
                          pop.lastyear = pop.list[[length(pop.list)]], 
                          Year = i, m.hat = memory.list[[i-1]])
  # m.hat.lastyear  <- migration.list[[i-1]]
  if(abs(migration.list[[i-1]]["x2"] - migration.list[[i-1]]["x1"]) < 1){
    migration.list[[i]] <- migration.list[[i-1]]
  } else{
    migration.list[[i]] <- fitMigration(t = world$time, x = getMem(pop.list[[i]], world), 
                                        m.start = migration.list[[i-1]])
    if(migration.list[[i]]["dt1"] < 0){
      migration.list[[i]]["dt1"] <- 0
      warning("\nnegative dt1 estimated!!!  coerced to 0")
    }
    if(migration.list[[i]]["dt2"] < 0){
      migration.list[[i]]["dt2"] <- 0
      warning("\nnegative dt2 estimated!!!  coerced to 0")
    }
  }
  memory.list[[i]] <- parameters["kappa"]^(i-1) * world$m0 + 
    (1 - parameters["kappa"]^(i-1)) * migration.list[[i]]
  
  similarity <- computeEfficiency(pop.list[[i-1]], pop.list[[i]], world)
  
  image(pop.list[[length(pop.list)]])
  plot(world$time, getMem(pop.list[[i]], world))
}




#m <-migration.list[[i-1]] %>% as.list
m.start <- migration.list[[i-1]]
t <- world$time
x <- getMem(pop.list[[i]], world)
nlsLM(x ~ stepMigration(t, t1, dt1, t2, dt2, x1, x2, tau = 100), 
      start = as.list(m.start),
      lower = c(-100,-50,0,0,-100,-100), 
      upper = c(200,100,200,100,100,100))


plot(world$time, getMem(pop.list[[i]], world), ylim = c(-2,12))
with(migration.list[[i-1]] %>% as.list,{
  abline(v = c(t1, t1+dt1, t2, t2 + dt2))
  abline(h = c(x1, x2))
})

with(migration.list[[i-1]] %>% as.list, 
     stepMigration(t = world$time, t1, dt1, t2, dt2, x1, x2, tau = 100)
) %>% lines

memory.list[[i]] <- parameters["kappa"]^(i-1) * world$m0 + 
  (1 - parameters["kappa"]^(i-1)) * migration.list[[i]]

similarity <- computeEfficiency(pop.list[[i-1]], pop.list[[i]], world)
}
