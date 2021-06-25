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
  save(stability, file = "results/stability/stability_compiled.rda")
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








hist(eps4$TE[eps4$TE < 7], breaks = 50)



require(broom)
require("GGally")

load("results/stability/stability_compiled.rda")


fit <- glm(I(TE < 2) ~ (scale(alpha) + 
                              scale(beta) + 
                              scale(sigma_t) + 
                              scale(sigma_x) + 
                              scale(epsilon) + 
                              scale(lambda))^2, 
                           data = stability, family = "binomial") 

fit2 <- lm(log(TE) ~ (scale(alpha) + 
                          scale(beta) + 
                          scale(sigma_t) + 
                          scale(sigma_x) + 
                          (factor(epsilon) - 1) + 
                          (factor(lambda) - 1))^2, 
           data = stability) 

AIC(fit, fit2)

fit.coef <- fit2 %>% tidy(conf.int = TRUE) %>% mutate(term = gsub("scale", "", term )) %>% 
  mutate(term = gsub("factor", "", term, fixed = TRUE))  %>% 
  arrange(estimate) %>% 
  mutate(term = factor(term, levels = as.character(term))) 

ggcoef(fit.coef)




# 
require(randomForest)
names(df)
features <- c("epsilon","alpha","beta","lambda","sigma_x","sigma_t","TE")

system.time(
migration.rf <- randomForest(log(TE) ~ ., 
                             data=stability[,features] %>% 
                               mutate(epsilon = factor(epsilon),
                                      lambda = factor(lambda)), 
                             ntree=1000,
                             keep.forest=FALSE, 
                             importance=TRUE)

)

system.time(
  migration.rf2 <- randomForest(I(TE<1) ~ ., 
                               data=stability[,features] %>% 
                                 mutate(epsilon = factor(epsilon),
                                        lambda = factor(lambda)), 
                               ntree=1000,
                               keep.forest=FALSE, 
                               importance=TRUE)
  
)


importance(migration.rf2)
importance(migration.rf)



with(stability, plot(TE, FE))

ggplot(stability %>% mutate(mismatch = cut(TE, c(0,1,5,50,300))), 
       aes(mismatch, FE, col = mismatch)) + 
  #geom_jitter(alpha = 0.5, cex = 0.5) + 
  geom_boxplot(varwidth = TRUE) + 
  facet_grid(.~sigma_t)

df <- stability %>% mutate(mismatch = cut(TE, c(0,1,5,50,300)))


png("text/figures/ForagingEfficiency.png", width = 2400, height = 900, res = 300)
  cols <- rich.colors(6)[2:5]
  par(mfrow=c(1,5), mar = c(0,.5,0,.5), oma = c(2,3,4,2), bty = "l", tck = 0.02, mgp = c(1,.25,0), xpd = NA)
  for(s in seq(3,15,3)){
    boxplot(FE~mismatch, data = subset(df, sigma_t == s), 
            varwidth = TRUE, yaxt = "n", xaxt = "n", xlab = "", ylab = "", ylim = c(0,1),
            col = cols, lty = 1, border = grey(.4))
    mtext(side = 3, bquote(sigma[t] == .(s)), line = 1, cex = 1.1)
    # mtext(side = 1, at = 1:4, c("< 1", "1-5", "5-50", ">50"), line = 0.5)
    if(s == 3){ 
      axis(2, las = 2)
      mtext(side = 2, "Foraging efficiency", line = 1.75)
      legend("top", fill = cols, legend = c("< 1", "1-5", "5-50", ">50"), 
             ncol = 2, cex = 1.3, xpd = NA, 
             title = "Total mismatch", bty = "n")
    }
  }
dev.off()

  
require(rpart)

par(xpd = NA, mfrow = c(1,1), mar = c(0,0,0,0))
migration.dt <- rpart(factor(cut(TE, c(0,1,5,Inf))) ~ ., 
                      data=stability[,features] %>% mutate(epsilon = factor(epsilon),
                                                           lambda = factor(lambda)))
plot(migration.dt)
text(migration.dt, use.n = TRUE, cex = 0.5)

require("randomForestExplainer")

df <- stability[,features] 

migration.rf <- randomForest(cut(TE, c(0,1,5,Inf)) ~ ., 
                             data= df, 
                             ntree=1000,
                             keep.forest=TRUE, 
                             importance=TRUE,
                             localImp = TRUE)


explain_forest(migration.rf, interactions = TRUE, data = stability)
