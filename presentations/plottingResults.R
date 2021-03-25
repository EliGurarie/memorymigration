
compareResourceTypes <- function(df, beta0.high = 200, beta1.high = 300){
  p <- ggplot(df %>% 
                mutate(extent = (space.sd*2) %>% factor,
                       duration = (time.sd*2) %>% factor,
                       model = ifelse(beta0 == 0 & beta1 == 500, "c) Memory: 500",
                                      ifelse(beta0 == 500 & beta1 == 0, "b) Sociality: 500",
                                             ifelse(beta0 == beta0.high & beta1 == beta1.high, 
                                                    paste("d) Memory:", beta1.high,  "Sociality: ", beta0.high),
                                                    ifelse(beta0 == 0 & beta1 == 0, "a) Tactics only", NA))))) %>% 
                subset(!is.na(model)), 
              aes(FE, MI, col = extent, pch = duration)) + 
    geom_line(aes(FE, MI, group = duration), col = "grey", lty = 3) + 
    geom_line(aes(FE, MI, group = extent), col = "grey", lty = 3) + 
    geom_point(size = 3) + facet_wrap(.~model, ncol = 2) + 
    theme_few() + ylab("Migratoriness") + xlab("Foraging efficiency") 
  return(p)
}



compareResourceTypes_v2 <- function(df, beta0.high = 200, beta1.high = 300){
  p <- ggplot(df %>% 
                mutate(extent = (space.sd*2) %>% factor,
                       duration = (time.sd*2) %>% factor,
                       model = ifelse(beta0 == 0 & beta1 == 500, "c) Memory: 500",
                                      ifelse(beta0 == 500 & beta1 == 0, "b) Sociality: 500",
                                             ifelse(beta0 == beta0.high & beta1 == beta1.high, 
                                                    paste("d) Memory:", beta1.high,  "Sociality: ", beta0.high),
                                                    ifelse(beta0 == 0 & beta1 == 0, "a) Tactics only", NA))))) %>% 
                subset(!is.na(model)), 
              aes(MI, FE, col = extent, pch = duration)) + 
    geom_line(aes(MI, FE, group = duration), col = "grey", lty = 3) + 
    geom_line(aes(MI, FE, group = extent), col = "grey", lty = 3) + 
    geom_point(size = 3) + facet_wrap(.~model, ncol = 2) + 
    theme_few() + xlab("Migratoriness") + ylab("Foraging efficiency") 
  return(p)
}
setwd("~/research/memorymigration/presentations")
load("../results/Results_0325.rda")


require(ggplot2)
require(magrittr)
require(plyr)
require(ggthemes)

subset(results, epsilon == 10 & alpha == 1000 & n.runs == 60)

with(results %>% subset(epsilon == 10 & alpha == 1000 & n.runs == 60), 
     table(beta0, beta1))

png("images/ComparingResourceDynamics.png", width = 2400, height = 2000, res = 300)
results %>% subset(epsilon == 10 & alpha == 1000) %>% compareResourceTypes_v2(200,200)  + 
  ggtitle("high resource following, low diffusion")
dev.off()

head(results)

p1 <- ggplot(results %>% subset(.id == "R0612") %>% mutate(tactic = factor(alpha), diffusion = factor(epsilon)), 
       aes(tactic, MI, col = diffusion)) + geom_boxplot() + ggtitle("Migratoriness")
p2 <- ggplot(results %>% subset(.id == "R0612") %>% mutate(tactic = factor(alpha), diffusion = factor(epsilon)), 
             aes(tactic, FE, col = diffusion)) + geom_boxplot() + ggtitle("Foraging efficiency")
p3 <- ggplot(results %>% subset(.id == "R0612") %>% mutate(tactic = factor(alpha), diffusion = factor(epsilon)), 
             aes(tactic, SC, col = diffusion)) + geom_boxplot() + ggtitle("Social cohesion")
p4 <- ggplot(results %>% subset(.id == "R0612") %>% mutate(tactic = factor(alpha), diffusion = factor(epsilon)), 
             aes(tactic, n.runs, col = diffusion)) + geom_boxplot() + ggtitle("Number of runs")

require(gridExtra)
png("images/MetricComparisons.png", width = 2000, height = 2400, res = 300)
grid.arrange(p1, p2, p3, p4, ncol = 1)
dev.off()


boxplot(MI ~ alpha*epsilon, data = results) 


with(results, table)

png("images/ComparingResourceDynamics_epsilon100_alpha1000.png", width = 2000, height = 2000, res = 300)
results %>% subset(epsilon == 10 & alpha == 100 & beta1 >= 200 & beta0 >= 100) %>% 
  mutate(extent = (space.sd*2) %>% factor,
                  duration = (time.sd*2) %>% factor,
         beta0 = paste("beta0", beta0),
         beta1 = paste("beta1", beta1),
         n.runs = cut(n.runs, seq(0,60,5))
         ) %>%
  ggplot(aes(FE, MI, col = n.runs, pch = duration)) + 
  geom_point() + facet_grid(beta0~beta1) + 
  theme_few() + ylab("Migratoriness") + xlab("Foraging efficiency") + 
  ggtitle("Moderate resource following")
dev.off()


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


png("images/sim0.png", width = 3000, height = 1500, res = 300)
par(mfrow = c(1,4), mar = c(0,0,3,0), oma = c(2,2,4,2), tck = 0.01)
for(i in 1:length(sim0)) image(1:100, 1:100, sim0[[i]], main = paste("year", i-1), yaxt = "n", xaxt = "n")
title("Only tactic response", outer = TRUE, cex.main = 2)
dev.off()


png("images/sim1.png", width = 3000, height = 1500, res = 300)
par(mfrow = c(2,4), mar = c(0,0,3,0), oma = c(2,2,4,2), tck = 0.01)
for(i in 1:length(sim1)) image(1:100, 1:100, sim1[[i]], main = paste("year", i-1), yaxt = "n", xaxt = "n")
title("With memory", outer = TRUE, cex.main = 2)
dev.off()

png("images/sim2.png", width = 3000, height = 1500, res = 300)
par(mfrow = c(1,4), mar = c(0,0,3,0), oma = c(2,2,4,2), tck = 0.01)
for(i in 1:length(sim2)) image(1:100, 1:100, sim2[[i]], main = paste("year", i-1), yaxt = "n", xaxt = "n")
title("With sociality", outer = TRUE, cex.main = 2)
dev.off()

length(sim3)
png("images/sim3.png", width = 3000, height = 1500, res = 300)
par(mfrow = c(3,4), mar = c(0,0,3,0), oma = c(2,2,4,2), tck = 0.01)
for(i in 1:12) 
  image(1:100, 1:100, sim3[[i]], main = paste("year", i-1), yaxt = "n", xaxt = "n")
title("With memory and sociality", outer = TRUE, cex.main = 2)
dev.off()



length(sim4)
png("images/sim4.png", width = 3000, height = 1500, res = 300)
par(mfrow = c(2,7), mar = c(0,0,3,0), oma = c(2,2,4,2), tck = 0.01)
for(i in 1:14) 
  image(1:100, 1:100, sim4[[i]], main = paste("year", i-1), yaxt = "n", xaxt = "n")
title("With memory and sociality", outer = TRUE, cex.main = 2)
dev.off()


par(mfrow = c(2,4), mar = c(0,0,0,0), oma = c(2,2,4,2))
for(i in 1:length(sim1)) image(sim1[[i]])
title("Only Sociality")





with(subset(results, n.runs > 25), table(alpha, beta0, beta1))


png("plots/ComparingResourceDynamics_alpha1000.png", width = 2000, height = 2000, res = 300)
ggplot(results %>% subset(epsilon == 10 & alpha == 1000 & n.runs < 30) %>% 
         mutate(extent = (space.sd*2) %>% factor,
                duration = (time.sd*2) %>% factor,
                model = ifelse(beta0 == 0 & beta1 == 500, "c) Memory",
                               ifelse(beta0 == 500 & beta1 == 0, "b) Sociality",
                                      ifelse(beta0 == 300 & beta1 == 300, "d) Memory and Sociality",
                                             ifelse(beta0 == 0 & beta1 == 0, "a) Tactics only", NA))))) %>% 
         subset(!is.na(model)), 
       aes(FE, MI, col = extent, pch = duration)) + 
  geom_line(aes(FE, MI, group = duration), col = "grey", lty = 3) + 
  geom_line(aes(FE, MI, group = extent), col = "grey", lty = 3) + 
  geom_point(aes(size = cut(n.runs, c(0, 4, 10, 30)))) + facet_wrap(.~model, ncol = 2) + 
  theme_few() + ylab("Migratoriness") + xlab("Foraging efficiency")
dev.off()



ggplot(results %>% subset(epsilon == 10 & alpha == 1000) %>% 
         mutate(extent = (space.sd*2) %>% factor,
                duration = (time.sd*2) %>% factor,
                model = ifelse(beta0 == 0 & beta1 == 500, "c) Memory only",
                               ifelse(beta0 == 500 & beta1 == 0, "b) Sociality only",
                                      ifelse(beta0 == 300 & beta1 == 300, "d) Memory and Sociality",
                                             ifelse(beta0 == 0 & beta1 == 0, "a) Neither", NA))))) %>% 
         subset(!is.na(model)), 
       aes(FE, n.runs)) + 
  geom_line(aes(group = duration), col = "grey", lty = 3) + 
  geom_line(aes(group = extent), col = "grey", lty = 3) + 
  geom_point(size = 3, aes(col = extent, pch = duration)) + 
  facet_wrap(.~model, ncol = 2) + 
  theme_few() # + ylab("Migratoriness") + xlab("Foraging efficiency")


require(ggthemes)
source("code/plotCompiled.R")
pdf("plots/Q1Q2Results.pdf", height = 5, width = 15)
d_ply(results, ".id", plotCompiled)
dev.off()


