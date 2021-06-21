require(memorymigration)
require(plyr)
require(gplots)
require(ggplot2)
require(ggthemes)
require(gridExtra)

fixTE <- function(df){
  df %>% mutate(TE = t1.error + t2.error + abs(x1.error) + abs(x2.error), 
         Migration = cut(TE, c(0,1,7,1e3), labels = c("perfect", "good", "bad")), 
         Migration = ifelse(is.na(Migration), "bad", Migration))
}

load("results/stability/stability_1.rda")
eps1 <- newresults %>% fixTE
load("results/stability/stability_1b.rda")
eps1 <- rbind(eps1, newresults %>% fixTE)

load("results/stability/stability_4.rda")
eps4 <- newresults %>% fixTE
load("results/stability/stability_4b.rda")
eps4 <- rbind(eps4, newresults %>% fixTE)

load("results/stability/stability_8.rda")
eps8 <- newresults %>% fixTE
load("results/stability/stability_8b.rda")
eps8 <- rbind(eps8, newresults %>% fixTE)



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


ggFE(subset(eps1, lambda == 50))

require(gtools)
df <- smartbind(eps1, eps4, eps8)


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
