rm(list=ls())
try(setwd("C:/Users/Elie/box sync/UWiscLLab"))
try(setwd("/home/elzizi/Dropbox/UWiscLLab/"))
pcks <- c("shiny","sf","ggplot2","magrittr","plyr", "gplots")
lapply(pcks, require, character = TRUE)

source("functions.R")
source("SIP_ShinyScripts.R")


load("data/WI_data.rda")
WI_data$Z <- WI_data$X.km + 1i*WI_data$Y.km
M.distance <- with(WI_data, outer(Z,Z, function(z1,z2) Mod(z2-z1)))

eval <- FALSE
if(eval){
  parameters <- list(rho = 0.5, gamma = .3, mu_S = 0.05, mu_I = 0.1, 
                     alpha = 1, W_max = 40, lambda = 20, beta = 1)
  a <- run_SIP(WI_data, parameters, Tmax = 500, M.distance) %T>% plotTimeSeries
  x11(); mapResults(a)
  round(a$I[,2]/max(a$I[,2])*255+1)
}

source("SIP_ShinyScripts.R"); 
shinyApp(ui_SIP, server_SIP)




runModel <- function(a, parameters, Tmax = 200){
  print(a)
  parameters$alpha <- a 
  myfit <- run_SIP(WI_data, parameters, Tmax = Tmax, M.distance)
  data.frame(alpha = a, S = myfit$S[,Tmax], I = myfit$I[,Tmax], county = WI_data$county)
}

summarizeSim <- function(sim){
  merge(sim, WI_data[,c("county","Wolf_presence","area", "WTD")], keep = "all") %>% 
  mutate(P.CWD = I/(S+I), K = 2*WTD, P.WTD = (S+I)/K) %>% 
    ddply(c("alpha", "Wolf_presence", "M"), summarize, 
          P.CWD.mean = mean(P.CWD), P.CWD.sd = sd(P.CWD)/sqrt(length(P.CWD)),
          P.WTD.mean = mean(P.WTD), P.WTD.sd = sd(P.WTD)/sqrt(length(P.WTD)))
  }


eval <- FALSE
if(eval){
  alphas <- seq(1,10,1)
  Tmax <- 200
  
  p1 <- list(rho = 0.5, gamma = .02, mu_S = 0.06, mu_I = 0.06, 
                 W_max = 60, lambda = 10, beta = "gaussian")
  
  p2 <- list(rho = 0.5, gamma = .02, mu_S = 0.06, mu_I = 0.06, 
             W_max = 60, lambda = 10, beta = 1)

  p3 <- list(rho = 0.5, gamma = .02, mu_S = 0.06, mu_I = 0.06, 
             W_max = 60, lambda = 80, beta = "gaussian")
  
  p4 <- list(rho = 0.5, gamma = .02, mu_S = 0.06, mu_I = 0.06, 
             W_max = 60, lambda = 80, beta = 1)
  
  runSim1 <- list(simRun = ldply(alphas, runModel, parameters = p1) %>% mutate(M = "Slow, Gaussian"), parameters = p1)
  runSim2 <- list(simRun = ldply(alphas, runModel, parameters = p2) %>% mutate(M = "Slow, Exponential"), parameters = p2)
  runSim3 <- list(simRun = ldply(alphas, runModel, parameters = p3) %>% mutate(M = "Fast, Gaussian"), parameters = p3)
  runSim4 <- list(simRun = ldply(alphas, runModel, parameters = p4) %>% mutate(M = "Fast, exponential"), parameters = p4)
  
  allSims_summary <- ldply(list(runSim1, runSim2, runSim3, runSim4), function(l) summarizeSim(l[[1]]))
  
  require(ggthemes)
  
  png("images/SimResults1.png", width = 2400, height = 1400, res = 300)
  ggplot(allSims_summary %>% mutate(low = P.CWD.mean - 2*P.CWD.sd, high = P.CWD.mean + 2*P.CWD.sd), 
         aes(alpha, P.CWD.mean, ymin = low, ymax = high, col = Wolf_presence)) + 
    geom_point() + geom_line() + geom_errorbar() + xlab("Selection coefficient") + ylab("Proportion infected") + 
    facet_wrap(.~M) + theme_few()
  dev.off()
  
  png("images/SimResults_WTD_K.png", width = 2000, height = 1000, res = 00)
  ggplot(allSims_summary %>% mutate(low = P.WTD.mean - 2*P.WTD.sd, high = P.WTD.mean + 2*P.WTD.sd), 
         aes(alpha, P.WTD.mean, ymin = low, ymax = high, col = Wolf_presence)) + 
    geom_point() + geom_line() + geom_errorbar() + xlab("Selection coefficient") + ylab("Proportion of carrying capacity") + 
    facet_wrap(.~M)
  dev.off()
  
  Sim1_summary <- summarizeSim(runSim1$simRun)
  Sim2_summary <- summarizeSim(runSim1$simRun)
  
  
  require(ggplot2)
  p1a <- ggplot(Sim1_summary %>% mutate(low = P.CWD.mean - 2*P.CWD.sd, high = P.CWD.mean + 2*P.CWD.sd), 
          aes(alpha, P.CWD.mean, ymin = low, ymax = high, col = Wolf_presence)) + geom_point() + geom_errorbar() + xlab("Selection coefficient") + ylab("Proportion infected") 
  p1b <- ggplot(Sim1_summary %>% mutate(low = P.WTD.mean - 2*P.WTD.sd, high = P.WTD.mean + 2*P.WTD.sd), 
                 aes(alpha, P.WTD.mean, ymin = low, ymax = high, col = Wolf_presence)) + geom_point() + geom_errorbar() + xlab("Selection coefficient") + ylab("Proportion infected") 
 
  require(gridExtra)
  grid.arrange(p1a, p1b, ncol = 2)
  
}    
