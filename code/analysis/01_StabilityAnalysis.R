require(memorymigration)
require(plyr)

fixTE <- function(df){
  df %>% mutate(TE = t1.error + t2.error + abs(x1.error) + abs(x2.error), 
         Migration = cut(TE, c(0,1,7,1e3), labels = c("perfect", "good", "bad")), 
         Migration = ifelse(is.na(Migration), "bad", Migration))
}

a <- load("results/stability/stability_1.rda")
eps1 <- newresults %>% fixTE
load("results/stability/stability_4.rda")
eps4 <- newresults %>% fixTE
load("results/stability/stability_8.rda")
eps8 <- newresults %>% fixTE



ggResults <- function(df, ...){
  
  require(gplots)
  require(ggplot2)
  require(ggthemes)
  require(gridExtra)
  
  #cuts <- c(0, 2^(seq(-1,10,1)))
  
  df %>% 
    ggplot(aes(sigma_t, sigma_x, fill = factor(Migration))) + 
    facet_grid(beta~alpha) + 
    #scale_fill_gradientn(trans = "log", breaks = cuts, colours = rich.colors(100)[100:1]) + 
    scale_fill_manual(values = c("darkblue", "grey", "white")) + 
    geom_tile() + theme_few() + 
    xlab("") + theme(legend.position = "none")

  #  p2 <- df %>% subset(ss) %>% 
  #   ggplot(aes(sigma_t, sigma_x, fill = n.runs)) + 
  #    facet_grid(beta~alpha) + 
  #    geom_tile() + theme_few() + ggtitle("N. runs")  + xlab("")
    
  #  p3 <- df %>% subset(ss) %>% 
  #    ggplot(aes(sigma_t, sigma_x, fill = FE)) + 
  #    facet_grid(beta~alpha) + scale_fill_gradientn(colours = rich.colors(100)) + 
  #    geom_tile() + theme_few() + ggtitle("Foraging Efficiency")
    
  #  grid.arrange(p1, p3, nrow = 1, ...)
}


df <- smartbind(eps1, eps4, eps8)


plist <- dlply(df, c("epsilon","lambda"), 
               function(df) ggResults(df) + 
                 ggtitle(paste("epsilon =", df$epsilon[1], ";", paste("lambda =", df$lambda[1]))))


pdf("plots/StabilityResults.pdf", height = 10, width = 12)
grid.arrange(plist[[1]], plist[[2]], plist[[3]], 
             plist[[4]], plist[[5]], plist[[6]], 
             plist[[7]], plist[[8]],
             ncol = 3)
dev.off()

pdf("plots/StabilityResults.pdf", height = 11, width = 8)
ggResults(eps1, top = "epsilon = 1")
ggResults(eps4, top = "epsilon = 4")
ggResults(eps8, top = "epsilon = 8")
dev.off()

table(eps1$lambda)

pdf("plots/StabilityResults_eps1_bylambda.pdf", height = 6, width = 11)
ggResults(eps1 %>% subset(lambda == 20), top = "epsilon = 1, lambda = 20")
ggResults(eps1 %>% subset(lambda == 50), top = "epsilon = 1, lambda = 50")
ggResults(eps1 %>% subset(lambda == 100), top = "epsilon = 1, lambda = 100")
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




summary(TE.lm)

require(broom)
require("GGally")

coefs.eps1 <- glm(I(TE < 2) ~ (scale(alpha) + 
                                          scale(beta) + 
                                          scale(sigma_t) + 
                                          scale(sigma_x) + 
                                          (factor(lambda) - 1))^2, 
                           data = eps1 %>% subset(lambda < 100), family = "binomial") %>% 
  tidy(conf.int = TRUE) 



coefs.eps4 <- glm(I(TE < 2) ~ (scale(alpha) + 
                                 scale(beta) + 
                                 scale(sigma_t) + 
                                 scale(sigma_x) + 
                                 (factor(lambda) - 1))^2, 
                  data = eps4, family = "binomial") %>% 
  tidy(conf.int = TRUE) 


coefs.eps1 %>% mutate(term = gsub("scale", "", term ), 
         term = gsub("factor", "", term )) %>%  
  ggcoef()

coefs.eps4 %>% mutate(term = gsub("scale", "", term ), 
                      term = gsub("factor", "", term )) %>%  
  ggcoef()

