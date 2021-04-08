require("memorymigration")

data(world)
data(resources_island)
world_gaussian$resource <- resources_island[["R_t12_x3"]]
image(world_gaussian$resource)

plotSim <- function(sim, years = NULL, nrow = 1){
  
 require(gplots)
  
  if(is.null(years)){
    years <- 1:length(sim)
    n <- length(years)
    zmax <- max(sapply(sim, max))
  } else{
    zmax <- max(sapply(sim, max)[paste0("Year",c(0,years[-length(years)]))])
    n <- length(years)
  }
  
  par(mfrow = c(nrow,ceiling(n/nrow)), mar = c(0,0,3,0), oma = c(2,2,4,2), tck = 0.01)
  for(i in years) 
    image(1:100, 1:100, sim[[i]], 
          breaks = seq(0, zmax, length = 100), col = rich.colors(99),
          main = paste("year", i-1), yaxt = "n", xaxt = "n")
}


world_sinusoidal$resource <- resources_island[["R_t12_x3"]]
sim0 <- runManyYears(world_sinusoidal, 
                     threshold = 1, n.years = 30,
                     parameters = c(epsilon = .3, alpha = 5, 
                                    beta0 = 0, beta1 = 0), verbose = TRUE)

plotSim(sim0, nrow = 3)


sim1 <- runManyYears(world_sinusoidal, 
                     threshold = 1, 
                     n.years = 10,
                     parameters = c(epsilon = .3, alpha = 5, 
                                    beta0 = 5, beta1 = 0), verbose = TRUE)
plotSim(sim1, nrow = 2)


world_sinusoidal <- getSinePop(100, peak.max = 75, peak.min = 25, sd = 3, dx = 1)

image(world_sinusoidal$pop)

world_sinusoidal$resource <- resources_island[["R_t12_x3"]]
sim2 <- runManyYears(world_sinusoidal, 
                     threshold = 1, 
                     n.years = 11,
                     parameters = c(epsilon = 1, alpha = 10, 
                                    beta0 = 5, beta1 = 40), verbose = TRUE)
plotSim(sim2, nrow = 2)

# s1 <- runNextYear(world_sinusoidal, c(epsilon = .2, alpha = 0, beta0 = 0, beta1 = 1000))
# image(s1)

