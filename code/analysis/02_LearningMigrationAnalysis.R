require(memorymigration)
require(plyr)
require(ggthemes)
require(gridExtra)


fixTE <- function(df){
  df %>% mutate(TE = abs(t1.error) + abs(t2.error) + abs(x1.error) + abs(x2.error), 
                Migration = cut(TE, c(1e3,32,8,2,0), label = LETTERS[1:4]) %>% 
                  relevel("B") %>% relevel("C") %>% relevel("D"))
}


eval <- FALSE
if(eval){
  require(gtools)
  
  load("results/migratory/migratory_1.rda")
  eps1 <- newresults %>% fixTE
  load("results/migratory/migratory_1b.rda")
  eps1 <- eps1 %>% smartbind(newresults %>% fixTE)
  
  load("results/migratory/migratory_4.rda")
  eps4 <- newresults %>% fixTE
  load("results/migratory/migratory_4b.rda")
  eps4 <- eps4 %>% smartbind(newresults %>% fixTE)
  
  load("results/migratory/migratory_8.rda")
  eps8 <- newresults %>% fixTE
  load("results/migratory/migratory_8b.rda")
  eps8 <- eps8 %>% smartbind(newresults %>% fixTE)

  learningmigration <- rbind(eps1, eps4, eps8)  
  save(learningmigration, file = "results/migratory/learningmigration.rda")
}

load("results/migratory/learningmigration.rda")

ggMigration <- function(df,  cols = rich.colors(4), ...){
  df %>% 
    ggplot(aes(sigma_t, sigma_x, fill = factor(Migration))) + 
    facet_grid(beta~alpha) + 
    scale_fill_manual(values = cols) + 
    geom_tile() + theme_few() + 
    xlab("") + theme(legend.position = "none")
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


migration.plist <- dlply(learningmigration, c("epsilon","lambda"), 
               function(df) ggMigration(df) + 
                 ggtitle(paste("epsilon =", df$epsilon[1], ";", paste("lambda =", df$lambda[1]))))

nruns.plist <- dlply(learningmigration, c("epsilon","lambda"), 
                     function(df) ggN.runs(df) + 
                       ggtitle(paste("epsilon =", df$epsilon[1], ";", paste("lambda =", df$lambda[1]))))


pdf("plots/LearningMigrationResults.pdf", height = 10, width = 12)
grid.arrange(grobs = migration.plist, ncol = 3, top = "Migration")
#grid.arrange(grobs = nruns.plist, ncol = 3, top = "N. runs")
dev.off()



fit <- glm(log(TE) ~ (scale(alpha) + 
                          scale(beta) + 
                          scale(sigma_t) + 
                          scale(sigma_x) + 
                          factor(epsilon) + 
                          factor(lambda)-1)^2, 
           data = learningmigration) 

fit.coef <- fit %>% tidy(conf.int = TRUE) %>% mutate(term = gsub("scale", "", term )) %>% 
  mutate(term = gsub("factor", "", term, fixed = TRUE)) 

arrange(fit.coef, estimate) %>% 
  mutate(term = factor(term, levels = as.character(term))) %>%  
  ggcoef()

par(mfrow = 1:2); plot(fit, 1:2)



ggplot(df, aes(sigma_t, TE, col = factor(beta), 
               pch = factor(alpha))) + 
  geom_point(alpha = 0.5) + facet_grid(lambda ~ epsilon) + 
  scale_y_continuous(trans='log10')  + theme_few()


summary(TE.lm)

require(broom)
require("GGally")


# 
require(randomForest)
df <- learningmigration

system.time(
  learning.rf <- randomForest(Migration ~ ., 
                               data=df[,features] %>% 
                                 mutate(epsilon = factor(epsilon),
                                        lambda = factor(lambda)), 
                               ntree=1000,
                               keep.forest=FALSE, 
                               importance=TRUE)
  
)

varImpPlot(learning.rf)

require(rpart)

load("results/migratory/learningmigration.rda")
require(magrittr)
require(plyr)
df <- learningmigration %>% mutate(mismatch = factor(TE<20))

features <- c("epsilon","alpha","beta","lambda","sigma_x","sigma_t", "mismatch")

learning.dt <- rpart(mismatch ~ ., 
                      data=df[,features] %>% mutate(epsilon = factor(epsilon),
                                                    lambda = factor(lambda)))
plot(learning.dt)
text(learning.dt, use.n = TRUE)

