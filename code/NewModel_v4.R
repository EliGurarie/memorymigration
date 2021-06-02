  require(memorymigration)
  
  buildOnRuns <- function(M, world, ...){
    world$pop <- M[[length(M)]]
    M2 <- runManyYears(world, threshold = 1, ...)
    M2[[1]] <- NULL
    M3 <- c(M, M2)
    names(M3) <- paste0("Year",1:length(M3))
    M3
  }
  
  source("code/functions_v4.R")
  source("memorymigration/R/RunningModel.R")
  source("memorymigration/R/PlottingFunctions.R")
  source("memorymigration/R/functions_indices.R")
  
  convolvePop <- function(X, pop, lambda){
    K <- function(x, l) -(x / (2*l^2)) * exp(-x^2/(2*l^2)) 
    ck <- function(x, pop, X, l) sum(K(x-X, l) * pop)
    sapply(X, FUN = ck, pop = pop, X = X, l = lambda)
  }
  
  
  pop <- extend(world$pop[25,]) %>% {./sum(.)}
  X.edge <- with(world, seq(X.min, X.max, dx))
  dp1 <- convolvePop(X.edge, pop = pop, lambda = 10)
  dp2 <- convolvePop(X.edge, pop = pop, lambda = 5)
  
  plot(pop, ylim = c(-.02, .02))
  lines(dp1, col = "red")
  lines(dp2, col = "green")
  
  #. Example 0: null
  
  world <- getSinePop(tau = 100, X.min = -50, X.max = 50, 
                      peak.max = 20, peak.min = -20, sd = 10)
  world$resource <- getResource_island(world, 
                                       c(t.peak = 25, t.sd = 9, 
                                         x.peak = 20, x.sd = 5))
  p0 <- c(epsilon = 5, alpha = 100, beta = 10,
          kappa = 0, lambda = 5, gamma = 0.5)
  M0 <- runManyYears(world, parameters = p0, n.years = 10, 1, verbose = TRUE)
  #M0 <- buildOnRuns(M0, world, parameters = p0, n.years = 10)
  plotManyRuns(M0, world, nrow = 2, outer = FALSE, labelyears = TRUE)
  
  {
    par(mfrow = c(1,2), mar = c(2,2,1,1), 
        tck = 0.01, mgp = c(1.5,.25,0), 
        bty = "l", cex.lab = 1.25, las = 1, xpd = NA)
    with(world, image(time, X, resource, col = grey.colors(100)))
    plotMemory(M0, add = TRUE)
     FE1 <- ldply(M0, computeEfficiency, 
                 resource = world$resource, world = world,
                 .id = "year") %>% mutate(year = 1:length(year))
     plot(FE1, type = "o")
  }

#. Example 1: adaptation
  world <- getSinePop(tau = 100, X.min = -50, X.max = 50, 
                      peak.max = 20, peak.min = -20, sd = 10)
  world$resource <- getResource_island(world, 
                                       c(t.peak = 25, t.sd = 9, 
                                         x.peak = 30, x.sd = 5))
  p1 <- c(epsilon = 5, alpha = 200, beta = 50, kappa = 0, lambda = 50)
  M1 <- runManyYears(world, parameters = p1, n.years = 30, 1, 
                     verbose = TRUE)

# M1 <- buildOnRuns(M1, world, parameters = p1, n.years = 20)
  plotManyRuns(M1, world, nrow = 3, outer = FALSE, labelyears = TRUE)
  
  p2 <- c(epsilon = 5, alpha = 200, beta = 10, kappa = 0, lambda = 50)
  M2 <- runManyYears(world, parameters = p2, n.years = 20, 1, 
                     verbose = TRUE)
  
# M2 <- buildOnRuns(M2, world, parameters = p2, n.years = 10, verbose = TRUE)
  plotManyRuns(M2, world, nrow = 3, outer = FALSE, labelyears = TRUE)
  {
    par(mfrow = c(2,1), mar = c(2,2,1,1), 
        tck = 0.01, mgp = c(1.5,.25,0), 
        bty = "l", cex.lab = 1.25, las = 1, xpd = NA)
    with(world, image(time, X, resource, col = grey.colors(100)))
    plotMemory(M1, add = TRUE)
    # with(world, image(time, X, resource, col = grey.colors(100)))
    # plotMemory(M2, add = TRUE)
    FE1 <- ldply(M1, computeEfficiency, 
                resource = world$resource, world = world,
                .id = "year") %>% mutate(year = 1:length(year))
    #FE2 <- ldply(M2, computeEfficiency, 
    #            resource = world$resource, world = world,
    #            .id = "year") %>% mutate(year = 1:length(year))
    plot(FE1, type = "o")
    #plot(FE2, type = "o")
  }
