require(memorymigration)
require(plyr)

fixTE <- function(df){
  df %>% mutate(TE = abs(t1.error) + abs(t2.error) + abs(x1.error) + abs(x2.error), 
                Migration = cut(TE, c(0,1,5,10,1e3)))
}

load("results/migratory/migratory_1.rda")
eps1 <- newresults %>% fixTE
load("results/migratory/migratory_4.rda")
eps4 <- newresults %>% fixTE



ggResults <- function(df, ...){
  
  require(gplots)
  require(ggplot2)
  require(ggthemes)
  require(gridExtra)

  df %>% 
    ggplot(aes(sigma_t, sigma_x, fill = factor(Migration))) + 
    facet_grid(beta~alpha) + 
    #scale_fill_gradientn(trans = "log", breaks = cuts, colours = rich.colors(100)[100:1]) + 
    scale_fill_manual(values = rich.colors(4)) + 
    geom_tile() + theme_few() + 
    xlab("") #+ theme(legend.position = "none")
}


ggN.runs <- function(df, ...){
  
  df %>% 
    ggplot(aes(sigma_t, sigma_x, fill = n.runs)) + 
    facet_grid(beta~alpha) + 
    #scale_fill_gradientn(trans = "log", breaks = cuts, colours = rich.colors(100)[100:1]) + 
    #scale_fill_manual(values = rich.colors(4)) + 
    geom_tile() + theme_few() + 
    xlab("") + theme(legend.position = "none")
}

df <- smartbind(eps1, eps4)


plist <- dlply(df, c("epsilon","lambda"), 
               function(df) ggResults(df) + 
                 ggtitle(paste("epsilon =", df$epsilon[1], ";", paste("lambda =", df$lambda[1]))))

nruns.plist <- dlply(df, c("epsilon","lambda"), 
                     function(df) ggN.runs(df) + 
                       ggtitle(paste("epsilon =", df$epsilon[1], ";", paste("lambda =", df$lambda[1]))))

pdf("plots/LearningMigration_nruns.pdf", height = 10, width = 12)
grid.arrange(grobs = nruns.plist, ncol = 3)
dev.off()


pdf("plots/LearningMigrationResults.pdf", height = 10, width = 12)
grid.arrange(grobs = plist, ncol = 3)
dev.off()

t



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

