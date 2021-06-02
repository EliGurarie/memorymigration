require(memorymigration)

#. Example 1: adaptation

eval <- FALSE
if(eval){
  world <- getSinePop(tau = 100, peak.max = 50, peak.min = -50, sd = 10)
  world$resource <- getResource_island(world, 
                                       c(t.peak = 25, t.sd = 12, 
                                         x.peak = 30, x.sd = 5))
  parameters <- c(epsilon = 5, alpha = 100, 
                  beta=50, kappa = 0, lambda = 20)
  M <- runManyYears(world, parameters = parameters, n.years = 22, 1)
  save(world, M, file = "results/msexamples/adaptation.rda")
} else load("results/msexamples/adaptation.rda")



world <- getSinePop(tau = 100, peak.max = 50, peak.min = -50, sd = 10)
world$resource <- getResource_island(world, 
                                     c(t.peak = 25, t.sd = 12, 
                                       x.peak = 30, x.sd = 5))
parameters <- c(epsilon = 5, alpha = 100, 
                beta=50, kappa = 0, lambda = 20, gamma = 1)
M <- runManyYears(world, parameters = parameters, n.years = 10, 1, verbose = TRUE)
parameters["gamma"] <- 0.95
M1 <- runManyYears(world, parameters = parameters, n.years = 10, 1, verbose = TRUE)

plotManyRuns(M1, world, nrow = 3, 
             outer = FALSE, labelyears = TRUE)
{
  par(mfrow = c(1,2), mar = c(2,2,1,1), 
      tck = 0.01, mgp = c(1.5,.25,0), 
      bty = "l", cex.lab = 1.25, las = 1, xpd = NA)
  with(world, image(time, X, resource, col = grey.colors(100)))
  plotMemory(M1, add = TRUE)
  FE1 <- ldply(M, computeEfficiency, 
               resource = world$resource, world = world,
               .id = "year") %>% mutate(year = 1:length(year))
  plot(FE1, type = "o")
}

#world$pop <- M[[length(M)]]
#M2[[1]] <- NULL
#M <- c(M, M2)
#names(M) <- paste0("Year", 1:length(M)-1)
#M2 <- runManyYears(world, parameters = parameters, 
#                   n.years = 11, 1)



par(mfrow = c(2,1))
contour(world$time, -100:100, memory_velocity)
lines(world$time, memory, col = "red", lwd = 2)
plot(world$time, dmemory, col = "green", lwd = 2)
abline(h = 0)
abline(v = 50.5, xpd = NA, col = "grey")

require(gplots)
plotManyRuns(M, world, nrow = 3, 
             outer = FALSE, labelyears = TRUE)
# plotYearList(M, world, nrow = 2)

require(plyr); require(ggplot2); require(ggthemes)


plotMemory <- function(M, add = FALSE){
  memory.df <- ldply(M, function(l)
    data.frame(time = 1:nrow(l), 
               memory = getMem(l, world = world)),.id = "year")
  
  if(!add) with(memory.df, plot(time, memory, type= "n"))
  n.years <- length(unique(memory.df$year))
  palette(rich.colors(n.years))
  ddply(memory.df, "year", function(df)
    with(df, lines(time, memory, col = as.integer(year))))
  #legend("topright", legend = 1:n.years, col = 1:n.years, 
  #       lty = 1, ncol = 2, title = "year", bty = "n")
}

with(world, image(time, X, resource, col = grey.colors(100)))
plotMemory(M, add = TRUE)

FE <- ldply(M, computeEfficiency, 
      resource = world$resource, world = world,
       .id = "year") %>% mutate(year = 1:length(year))
plot(FE)

#. Example 2: learning migration


world <- getSinePop(tau = 100, peak.max = 0, 
                    peak.min = 0, sd = 10)

world$resource <- getResource_drifting(world, 
                                     c(t.peak = 25, t.sd = 15, 
                                       x.peak = 40, x.sd = 5))
image(world$resource, col = rich.colors(101))

parameters <- c(epsilon = 5, alpha = 200, beta = 50, 
                kappa = 0, lambda = 20)

M <- runManyYears(world, parameters = parameters, 
                  n.years = 31, 1, verbose = TRUE)

plotManyRuns(M, world, nrow = 2, outer = FALSE)

plotMemory(M)
save(M, file = "results/msexamples/learningtomigrate.rda")

plot(FE, type = "o")
