require(memorymigration)
require(plyr)

fixTE <- function(df){
  df %>% mutate(TE = abs(t1.error) + abs(t2.error) + abs(x1.error) + abs(x2.error), 
                Migration = cut(TE, c(1e3,32,8,2,0), label = LETTERS[1:4]) %>% 
                  relevel("B") %>% relevel("C") %>% relevel("D"))
}



load("results/migratory/migratory_1.rda")
eps1 <- newresults %>% fixTE
load("results/migratory/migratory_4.rda")
eps4 <- newresults %>% fixTE
load("results/migratory/migratory_8.rda")
eps8 <- newresults %>% fixTE

require(gtools)
df <- smartbind(eps1, eps4, eps8)

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


migration.plist <- dlply(df, c("epsilon","lambda"), 
               function(df) ggMigration(df) + 
                 ggtitle(paste("epsilon =", df$epsilon[1], ";", paste("lambda =", df$lambda[1]))))

nruns.plist <- dlply(df, c("epsilon","lambda"), 
                     function(df) ggN.runs(df) + 
                       ggtitle(paste("epsilon =", df$epsilon[1], ";", paste("lambda =", df$lambda[1]))))


pdf("plots/LearningMigrationResults.pdf", height = 10, width = 12)
grid.arrange(grobs = migration.plist, ncol = 3, top = "Migration")
#grid.arrange(grobs = nruns.plist, ncol = 3, top = "N. runs")
dev.off()



boxplot(MI ~ Migration * lambda, data =df, col = 2:5)

ggplot(df, aes(sigma_x, TE, col = factor(beta), 
               pch = factor(alpha))) + 
  geom_point(alpha = 0.5) + facet_grid(lambda ~ epsilon) + 
  scale_y_continuous(trans='log2')  + theme_few()


summary(TE.lm)

require(broom)
require("GGally")


# 
require(randomForest)
names(df)

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

features <- c("epsilon","alpha","beta","lambda","sigma_x","sigma_t", "Migration")

learning.dt <- rpart(Migration ~ ., 
                      data=df[,features] %>% mutate(epsilon = factor(epsilon),
                                                    lambda = factor(lambda)))

plot(learning.dt)
text(learning.dt, use.n = TRUE)

