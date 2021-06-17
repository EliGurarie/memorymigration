require(memorymigration)

a <- load("results/stability/stability_1.rda")
eps1 <- newresults %>% mutate(TE = t1.error + t2.error + abs(x1.error) + abs(x2.error))
load("results/stability/stability_4.rda")
eps4 <- newresults %>% mutate(TE = t1.error + t2.error + abs(x1.error) + abs(x2.error))
load("results/stability/stability_8.rda")
eps8 <- newresults %>% mutate(TE = t1.error + t2.error + abs(x1.error) + abs(x2.error))

require(ggplot2)
require(ggthemes)
require(gridExtra)

ggResults <- function(df, ss = TRUE, ...){
  
  cuts <- c(0, 2^(seq(-1,10,1)))
  
  p1 <- df %>% subset(ss) %>% mutate(TotalError = cut(TE, cuts)) %>% 
    ggplot(aes(sigma_t, sigma_x, fill = TE)) + 
    facet_grid(beta~alpha) + 
    scale_fill_gradientn(trans = "log", breaks = cuts, colours = rich.colors(100)[100:1]) + 
    #scale_fill_manual(values = rich.colors(length(cuts)-1)) + 
    geom_tile() + theme_few() + ggtitle("Total error") + xlab("")

#  p2 <- df %>% subset(ss) %>% 
#   ggplot(aes(sigma_t, sigma_x, fill = n.runs)) + 
#    facet_grid(beta~alpha) + 
#    geom_tile() + theme_few() + ggtitle("N. runs")  + xlab("")
  
  p3 <- df %>% subset(ss) %>% 
    ggplot(aes(sigma_t, sigma_x, fill = FE)) + 
    facet_grid(beta~alpha) + scale_fill_gradientn(colours = rich.colors(100)) + 
    geom_tile() + theme_few() + ggtitle("Foraging Efficiency")
  
  grid.arrange(p1, p3, ncol = 1, ...)
}

pdf("plots/StabilityResults.pdf", height = 11, width = 8)
ggResults(eps1, top = "epsilon = 1")
ggResults(eps4, top = "epsilon = 4")
ggResults(eps8, top = "epsilon = 8")
dev.off()

table(eps1$lambda)

pdf("plots/StabilityResults_eps1_bylambda.pdf", height = 11, width = 8)
ggResults(eps1 %>% subset(lambda == 20), top = "epsilon = 1, lambda = 20")
ggResults(eps1 %>% subset(lambda == 50), top = "epsilon = 1, lambda = 50")
ggResults(eps1 %>% subset(lambda == 100), top = "epsilon = 1, lambda = 100")
dev.off()



table(eps4$lambda)
pdf("plots/StabilityResults_eps4_bylambda.pdf", height = 11, width = 8)
ggResults(eps4 %>% subset(lambda == 20), top = "epsilon = 4, lambda = 20")
ggResults(eps4 %>% subset(lambda == 50), top = "epsilon = 4, lambda = 50")
#ggResults(eps1 %>% subset(lambda == 100), top = "epsilon = 1, lambda = 100")
dev.off()


hist(eps1$TE[eps1$TE < 7], breaks = 50)


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

