require(memorymigration)
require(plyr)
require(gplots)
require(ggplot2)
require(ggthemes)
require(gridExtra)
eval <- FALSE
if(eval){
  fixTE <- function(df){
    df %>% mutate(TE = t1.error + t2.error + abs(x1.error) + abs(x2.error), 
           Migration = cut(TE, c(0,1,7,1e3), labels = c("perfect", "good", "bad")), 
           Migration = ifelse(is.na(Migration), "bad", Migration))
  }
  
  load("results/stability/stability_1.rda")
  eps1 <- newresults %>% fixTE
  load("results/stability/stability_1b.rda")
  eps1 <- rbind(eps1, newresults %>% fixTE)
  load("results/stability/stability_1c.rda")
  eps1 <- rbind(eps1, newresults %>% fixTE)
  
  load("results/stability/stability_4.rda")
  eps4 <- newresults %>% fixTE
  load("results/stability/stability_4b.rda")
  eps4 <- rbind(eps4, newresults %>% fixTE)
  load("results/stability/stability_4c.rda")
  eps4 <- rbind(eps4, newresults %>% fixTE)
  
  load("results/stability/stability_8.rda")
  eps8 <- newresults %>% fixTE
  load("results/stability/stability_8b.rda")
  eps8 <- rbind(eps8, newresults %>% fixTE)
  load("results/stability/stability_8c.rda")
  eps8 <- smartbind(eps8, newresults %>% fixTE)
  
  stability <- smartbind(eps1, eps4, eps8)
  save(df, file = "results/stability/stability_compiled.rda")
}


ggMigration <- function(df,  cols = c("darkblue", "orange", "darkgrey"), ...){
  df %>% 
    ggplot(aes(sigma_t, sigma_x, fill = factor(Migration))) + 
    facet_grid(beta~alpha) + 
    scale_fill_manual(values = cols) + 
    geom_tile() + theme_few() + 
    xlab("") + theme(legend.position = "none")
}


ggFE <- function(df, ...){
  df %>% 
    ggplot(aes(sigma_t, sigma_x, fill = FE)) + 
    facet_grid(beta~alpha) + 
    scale_fill_gradientn(colours = rich.colors(101), limits=c(0,1)) + 
    geom_tile() + theme_few() + 
    xlab("") + theme(legend.position = "none")
}

plotBlock <- function(block, cols = c("darkblue", "orange", "lightgrey"), 
                      side1 = FALSE, side2 = FALSE){
  require(reshape2)
  b <- block[,c("TE", "alpha", "beta", "sigma_t", "sigma_x")]
  ns <- apply(b[,-1], 2, function(x) length(unique(x)))
  
  M <-   b %>% acast(alpha + sigma_t ~ beta +  sigma_x, 
                     value.var = "TE")
  
  #M[is.na(M)] <- max(M, na.rm = TRUE)
  image(1:ncol(M)-.5, 1:nrow(M)-.5, M, breaks = c(0,1,5,1000), 
        col = cols, xaxt ="n", xlab = "", ylab = "", yaxt = "n")
  abline(v = 1:ncol(M), col = "white")
  abline(h = 1:nrow(M), col = "white")
  abline(v = (0:ns["alpha"]) * ns["sigma_t"], lwd = 2)
  abline(h = (0:ns["beta"]) * ns["sigma_x"], lwd = 2)
  
  if(side1)
    text(1:ns["alpha"] * ns["sigma_t"] - ns["sigma_t"]/2, -1, 
        unique(b$alpha), xpd = NA)
  
  if(side2)
    text(nrow(M)+ 1, 
        1:ns["beta"] * ns["sigma_x"] - ns["sigma_x"]/2,
        unique(b$beta), xpd = NA, srt = 270)
}



PlotMiniblock <- function(){
  miniM <- subset(df, 
                  lambda == 100 & epsilon == 1 & 
                    beta == 400 & alpha == 200)[,c("sigma_t", "sigma_x", "TE")] %>% 
    acast(sigma_t ~ sigma_x)
  
  sigs <- seq(3,15,3)
  image(sigs - 1.5, sigs - 1.5, miniM, breaks = c(0,1,5,1000), 
        col = cols, xaxt ="n", xlab = "", ylab = "", yaxt = "n", asp = 1)
  mtext(side = 1, at = sigs-1.5, sigs, line = .5, cex = 1.15)
  mtext(side = 2, at = sigs-1.5, sigs, line = .5, cex = 1.15)
  abline(v = sigs, col = "white")
  abline(h = sigs, col = "white")
  mtext(side = 1, expression("Resource duration "~(sigma[t])), line = 3, cex = 1.25)
  mtext(side = 2, expression("Resource extent "~(sigma[x])), line = 2, cex = 1.25)
}


plotStability <- function(stability){
  layout(rbind(1:3,4:6) %>% cbind(7:8))
  par( mar = c(1,1,1,1)/2, oma = c(3,3,3,3), xpd = FALSE)
  for(e in c(1,8)) for(l in c(20,50,100)){
    myblock <- subset(stability, epsilon == e & lambda == l) 
    if(e == 1){
      if(l == 20){
        plotBlock(myblock)
        mtext(side = 3, expression("tight swarm "~(lambda == 20)), line = 1, cex = 1.25)
        mtext(side = 2, expression("low diffusion "~(epsilon == 1)), line = 1, cex = 1.25)
      }
      if(l == 50){
        plotBlock(myblock)
        mtext(side = 3, expression("medium swarm"~(lambda == 40)), line = 1, cex = 1.25)
      }
      if(l == 100){
        plotBlock(myblock, side2 = TRUE)
        mtext(side = 3, expression("loose swarm"~(lambda == 100)), line = 1, cex = 1.25)
      }} 
    else{ 
      if(l == 20){  
        plotBlock(myblock, side1 = TRUE)
        mtext(side = 2, expression("high diffusion "~(epsilon == 8)), line = 1, cex = 1.25)
      }
      if(l == 50) plotBlock(myblock, side1 = TRUE)
      if(l == 100){  
        plotBlock(myblock, side1 = TRUE, side2 = TRUE)
      }
    }
  }
  b <- myblock[,c("TE", "alpha", "beta", "sigma_t", "sigma_x")]
  ns <- apply(b[,-1], 2, function(x) length(unique(x)))
  
  mtext(side = 1, outer = TRUE, expression("resource following "~(alpha)), line = 1.5, cex = 1.25)
  text(ns["sigma_t"] * ns["alpha"]  + 3, 
       ns["sigma_x"] * ns["beta"], srt = 270, xpd = NA, 
       cex = 2, 
       expression("social attraction " ~(beta)))

  par(mar = c(4,8,4,0))  
  plot.new()
  legend("center", fill = cols, legend = c("< 1", "1-5", ">1"), cex = 2, title = "total error", bty = "n")
  PlotMiniblock()
}

load("results/stability/stability_compiled.rda")
png("plots/StabilityResults.png", width = 3800, height = 2000, res = 300)
plotStability(stability)
dev.off()







ns <- apply(b[,-1], 2, function(x) length(unique(x)))

M <-   b %>% acast(alpha + sigma_t ~ beta +  sigma_x, 
                   value.var = "TE")



ggFE(subset(eps1, lambda == 50))

require(gtools)


p.migration.list <- dlply(df, c("epsilon","lambda"), 
               function(df) ggMigration(df) + 
                 ggtitle(paste("epsilon =", df$epsilon[1], ";", paste("lambda =", df$lambda[1]))))

p.FE.list <- dlply(df, c("epsilon","lambda"), 
                   function(df) ggFE(df) + 
                     ggtitle(paste("epsilon =", df$epsilon[1], ";", paste("lambda =", df$lambda[1]))))


pdf("plots/StabilityResults.pdf", height = 10, width = 12)
grid.arrange(grobs = p.migration.list, ncol = 3)
grid.arrange(grobs = p.FE.list, ncol = 3)
dev.off()





table(eps4$lambda)
pdf("plots/StabilityResults_eps4_bylambda.pdf", height = 6, width = 11)
ggResults(eps4 %>% subset(lambda == 20), top = "epsilon = 4, lambda = 20")
ggResults(eps4 %>% subset(lambda == 50), top = "epsilon = 4, lambda = 50")
ggResults(eps4 %>% subset(lambda == 100), top = "epsilon = 1, lambda = 100")
dev.off()


ggResults(eps8 %>% subset(lambda == 20), top = "epsilon = 4, lambda = 20")
ggResults(eps8 %>% subset(lambda == 50), top = "epsilon = 4, lambda = 50")
# ggResults(eps8 %>% subset(lambda == 100), top = "epsilon = 1, lambda = 100")



hist(eps4$TE[eps4$TE < 7], breaks = 50)



require(broom)
require("GGally")

coefs.df <- glm(I(TE < 2) ~ (scale(alpha) + 
                              scale(beta) + 
                              scale(sigma_t) + 
                              scale(sigma_x) + 
                              (factor(epsilon) - 1) + 
                              (factor(lambda) - 1))^2, 
                           data = df, family = "binomial") %>% 
  tidy(conf.int = TRUE) 


coefs.df %>% mutate(term = gsub("scale", "", term )) %>% 
  mutate(term = gsub("factor", "", term, fixed = TRUE)) %>%  
  ggcoef()




# 
require(randomForest)
names(df)
features <- c("epsilon","alpha","beta","lambda","sigma_x","sigma_t","TE")

system.time(
migration.rf <- randomForest(factor(I(TE < 2)) ~ ., 
                             data=df[,features] %>% 
                               mutate(epsilon = factor(epsilon),
                                      lambda = factor(lambda)), 
                             ntree=1000,
                             keep.forest=FALSE, 
                             importance=TRUE)

)

varImpPlot(migration.rf)

require(rpart)
migration.dt <- rpart(factor(I(TE < 2)) ~ ., 
                      data=df[,features] %>% mutate(epsilon = factor(epsilon),
                                                    lambda = factor(lambda)))

plot(migration.dt)
text(migration.dt, use.n = TRUE)
