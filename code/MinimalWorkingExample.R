source("functions2.R")

world <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=2, 
                    peak.max = 80, peak.min = 20, sd = 10)
world$resource <- getPulsedResource(world$time, world$X.mid, c(t.peak = 25, t.sd = 9, x.peak = 80, x.sd = 9))


parameters <- c(epsilon = 5, alpha = 300, beta0=100, beta1 = 300)
M <- runManyYears(world.R1, Parameters = parameters, n.years = 6)

plotYearList(M, tau = tau)
world.R1 <- world %>% list_modify(pop = M$Year6)

M2 <- runManyYears(world.R1, Parameters = parameters, n.years = 6)
plotYearList(M2, tau = tau)

sqrt(mean((M[[2]]-M[[1]])^2))
sqrt(mean((M2[[6]]-M2[[5]])^2))



# Migratoriness Index

  findMinOverlap <- function(P, world = world){
    
    getOverlap <- function(i1, i2, P){
      sum(sqrt(P[i1,] * P[i2,])) * world$dx
    }
    
    Overlap <- matrix(NA, world$tau, world$tau)
    for(i in 1:(world$tau-1))
      for(j in (i+1):world$tau)
        Overlap[i,j] <- sum(sqrt(P[i,] * P[j,])) * world$dx
    
    minOverlap <-  min(Overlap, na.rm = TRUE)
    list(times = which(Overlap == minOverlap, arr.ind = TRUE),
         overlap = minOverlap)
  }
  
  sapply(M, function(m) findMinOverlap(m)$overlap)
  sapply(M2, function(m) findMinOverlap(m)$overlap)

# Social cohesion index
  
  getSD <- function(f, world){
    EX <- sum( world$X.mid * f) * world$dx
    EX2 <- sum( world$X.mid^2 * f) * world$dx
    sd  = sqrt(EX2 - EX^2)
  }

  computeCohesiveness <- function(P){
    SDs <- apply(P, 1, getSD, world = world)
    SD.max <- (1/sqrt(12))*max(world$X.mid)
    SC <- 1 - SDs/SD.max
    c(SC.mean = mean(SC), SC.sd = sd(SC))
  }

  sapply(M, computeCohesiveness)
  sapply(M2, computeCohesiveness)
  
  
  
  
  