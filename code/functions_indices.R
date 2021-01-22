getSD <- function(pop, world){
  EX <- sum( world$X * pop) * world$dx
  EX2 <- sum( world$X^2 * pop) * world$dx
  sd  = sqrt(EX2 - EX^2)
}

computeCohesiveness <- function(pop){
  SDs <- apply(pop, 1, getSD, world = world)
  SD.max <- (1/sqrt(12))*max(world$X)
  SC <- 1 - SDs/SD.max
  c(SC.mean = mean(SC), SC.sd = sd(SC))
}

computeEfficiency <- function(pop, resource, world){
  
  dx <- world$dx
  tau <- world$tau
  
  Similarity <- matrix(NA, tau, tau)
  for(i in 1:(tau-1))
    for(j in (i+1):tau)
      Similarity[i,j] <- sum(sqrt(pop[i,] * resource[j,])) * dx
  
  FE <- mean(Similarity, na.rm=TRUE)/tau
}

computeMigratoriness <- function(pop, world){

  dx <- world$dx
  tau <- world$tau
  
  Overlap <- matrix(NA, tau, tau)
  for(i in 1:(tau-1))
    for(j in (i+1):tau)
      Overlap[i,j] <- sum(sqrt(pop[i,] * pop[j,])) * dx
  
  minOverlap <-  1-min(Overlap, na.rm = TRUE)
  list(times = which(Overlap == minOverlap, arr.ind = TRUE),
       overlap = minOverlap)
}

computeIndices <- function(pop, resource, world){
  SC = computeCohesiveness(pop, world)["SC.mean"]
  MI = computeMigratoriness(pop, world)$overlap
  FE = computeEfficiency(pop, resource, world)
  data.frame(SC, MI, FE)
}
