require(memorymigration)

getXT <- function(f){
  strsplit(f, "_") %>% ldply %>% mutate(t.sd = substring(V2, 2) %>% as.numeric, 
                                        x.sd = substring(V3, 2) %>% as.numeric) %>% 
    extract(c("x.sd", "t.sd"))
}


getCompiledResults <- function(folder){
  files <- list.files(folder)
  results.compiled <- data.frame()
  for(f in files){
    print(f)
    load(paste0(folder, "/", f))
    results.compiled <- 
          rbind(results.compiled, data.frame(getXT(f), newresults))
  }
  results.compiled
  #data.frame(rid = 1:nrow(results.compiled), results.compiled)
}


eval <- FALSE{
  require(plyr)
  
  results.dir <- "~/research/memorymigration/results/islandresources/gaussian/"
  folder.list <- list.files(results.dir)
  setwd(results.dir)
  results.df <- ldply(folder.list, getCompiledResults)
  setwd("~/research/memorymigration")
  results <- data.frame(type = "gaussian", results.df)
  save(results, file = "results/Results_Island_Gaussian.rda")
  
  results.dir <- "~/research/memorymigration/results/islandresources/sinusoidal/"
  folder.list <- list.files(results.dir)
  setwd(results.dir)
  results.df <- ldply(folder.list, getCompiledResults)
  setwd("~/research/memorymigration")
  results <- data.frame(type = "sinusoidal", results.df)
  save(results, file = "results/Results_Island_Sinusoidal.rda")
}



require(ggplot2)
require(magrittr)
require(plyr)
require(ggthemes)


source("code/functions_plottingresults.R")

load("results/Results_Island_Gaussian.rda")
r.gaussian <- results
load("results/Results_Island_Sinusoidal.rda")
r.sinusoidal <- results


r.pooled <- rbind(r.gaussian, r.sinusoidal)


# Results: Island - Gaussian (naive)
require(ggplot2)

r.pooled %>% subset(t.sd == 12 & x.sd == 3 & type == "gaussian") %>% plotbasicstats()
r.pooled %>% subset(t.sd == 12 & x.sd == 3  & type == "sinusoidal") %>% plotbasicstats()


r.pooled %>% subset(epsilon == 5 & alpha == 2000 & type == "sinusoidal") %>% 
  compareResourceTypes(200,200)  + ggtitle("high resource following, low diffusion")


r.pooled %>% subset(epsilon == 20 & alpha == 500 & type == "gaussian") %>% 
  compareResourceTypes(500,500)

r.pooled %>% subset(epsilon == 5 & alpha == 500 & type == "gaussian") %>% 
  compareResourceTypes(200,200)

r.pooled %>% subset(epsilon == 10 & alpha == 500 & type == "gaussian") %>% 
  compareResourceTypes(200,200)

r.pooled %>% subset(epsilon == 20 & alpha == 500 & type == "gaussian") %>% 
  compareResourceTypes(200,200)




######################################################################################
# Explore the actual process for the island resource
# at intermediate alpha - epsilon values, see breakdowns occur - what they look like

# SAMPLE CODE:

require(memorymigration)

eval <- FALSE
if(eval){
  data(world)
  world$resource <- resource_R153
  image(world$resource)
  
  sim0 <- runManyYears(world, parameters = c(alpha = 1000, epsilon = 10, beta1 = 0, beta0 = 0), verbose = TRUE)
  sim1 <- runManyYears(world, parameters = c(alpha = 1000, epsilon = 10, beta1 = 500, beta0 = 0), verbose = TRUE)
  sim2 <- runManyYears(world, parameters = c(alpha = 1000, epsilon = 10, beta1 = 0, beta0 = 500), verbose = TRUE)
  sim3 <- runManyYears(world, parameters = c(alpha = 1000, epsilon = 10, beta1 = 250, beta0 = 250), verbose = TRUE)
  sim4 <- runManyYears(world, parameters = c(alpha = 1000, epsilon = 10, beta1 = 500, beta0 = 200), verbose = TRUE)
  
  save(sim0, sim1, sim2, sim3, sim4, file = "../results/SimsAlpha1000Epsilon10.rda")
} else load("../results/SimsAlpha1000Epsilon10.rda")


length(sim1)

require(fields)

png("images/resource.png", width = 1500, height = 1500, res = 300)
par(mgp = c(1,0,0), cex.main = 2)
image.plot(1:100, 1:100, resource_R153, main = "resource", xaxt = "n", yaxt = "n", xlab = "time", ylab = "space")
dev.off()


