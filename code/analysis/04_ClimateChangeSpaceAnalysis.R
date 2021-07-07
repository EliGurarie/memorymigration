require(memorymigration)
require(plyr)
require(ggthemes)
require(gridExtra)


fixTE <- function(df){
  df %>% mutate(TE = abs(t1.error) + abs(t2.error) + abs(x1.error) + abs(x2.error), 
                Migration = cut(TE, c(1e3,32,8,2,0), label = LETTERS[1:4]) %>% 
                  relevel("B") %>% relevel("C") %>% relevel("D"))
}


#  Climate change trend --------

eval <- FALSE
if(eval){
  require(gtools)
  
  files <- list.files("results/climate_space/raw/")
  
  ccspace <- data.frame()
  for(f in files[grepl("rda", files)]){ 
    print(f)
    load(paste0("results/climate_space/raw/", f))
    ccspace <- ccspace %>% smartbind(newresults %>% fixTE)
  }
  
  save(ccspace, file = "results/climate_space/ccspace.rda")
}

load("results/climate_space/ccspace.rda")


with(ccspace, table(beta_x, lambda, alpha, beta))
head(ccspace)

require(ggplot2)
ccspace %>% 
  ggplot(aes(lambda, SAI_recent, col = factor(alpha))) +
             # beta, fill = cut(SAI_recent, c(-3,-1,0,.5,.7,.8,.9,1,10)))) + geom_tile() 
  facet_grid(beta_x~beta) + geom_path() + geom_point(aes(alpha = FE), size = 3) + theme_few()

ccspace %>% 
  ggplot(aes(lambda,  FE, col = factor(alpha))) +
  facet_grid(beta_x~beta) + geom_path() + geom_point() + theme_few()


#  Climate change stochasticity --------

eval <- FALSE
if(eval){
  require(gtools)
  
  files <- list.files("results/stochasticity/raw/")
  
  ccsigma <- data.frame()
  for(f in files[grepl("rda", files)]){ 
    print(f)
    load(paste0("results/stochasticity/raw/", f))
    ccsigma <- ccsigma %>% smartbind(newresults %>% fixTE)
  }
  
  save(ccsigma, file = "results/stochasticity/stochasticity.rda")
}

load("results/stochasticity/stochasticity.rda")

with(ccsigma, table(alpha, beta, lambda, kappa, psi_x))

require(ggplot2)
ccsigma %>% subset(alpha == 100) %>% 
  ggplot(aes(lambda, avgFE, col = factor(kappa))) +
  facet_grid(beta~psi_x) + geom_path() + theme_few() + ggtitle(expression(alpha == 100)) + 
  ylim(c(0,1))

ccsigma %>% subset(alpha == 400) %>% 
  ggplot(aes(lambda, avgFE, col = factor(kappa))) +
  facet_grid(beta~psi_x) + geom_path() + theme_few() + ggtitle(expression(alpha == 100)) + 
  ylim(c(0,1))

p1 <- list()
for(a in c(100,400)) for(b in c(100,400)){
  p1[[length(p1)+1]] <- ccsigma %>% subset(alpha == a & beta == b & lambda > 40) %>% 
    ggplot(aes(kappa, TE, col = factor(lambda))) +
    facet_grid(.~psi_x) + geom_path() + theme_few() + ggtitle(bquote(alpha == .(a) ~ beta == .(b))) 
  # + ylim(c(0.5,0.75))
}
grid.arrange(grobs = p1)



p <- list()
for(a in c(100,400)) for(b in c(100,400)){
  p[[length(p)+1]] <- ccsigma %>% subset(alpha == a & beta == b & lambda > 40) %>% 
    ggplot(aes(factor(kappa), avgFE)) +
    facet_grid(.~psi_x) + geom_boxplot() + theme_few() + ggtitle(bquote(alpha == .(a) ~ beta == .(b))) + 
    ylim(c(0.5,0.75))
}
grid.arrange(grobs = p)

#  Climate change trend and stochasticity --------
require(memorymigration)
eval <- FALSE
if(eval){
  require(gtools)
  
  files <- list.files("results/trendstochasticity/raw/")
  ccmusigma <- data.frame()
  for(i in 1:length(files)){ 
    print(files[i])
    load(paste0("results/trendstochasticity/raw/", files[i]))
    ccmusigma <- ccmusigma %>% smartbind(newresults %>% fixTE %>% mutate(run = i))
  }
  
  save(ccmusigma, file = "results/trendstochasticity/trendstochasticity.rda")
}

load("results/trendstochasticity/trendstochasticity.rda")

with(ccmusigma %>% subset(alpha == 100 & beta == 400 & lambda > 40), 
     table(lambda, kappa, psi_x, beta_x))

df <- ccmusigma %>% subset(alpha == 100 & beta == 400 & lambda > 40)

require(ggplot2); require(ggthemes)
df %>% subset(beta_x < 0) %>% 
  ggplot(aes(factor(kappa), annualFE)) +
  facet_grid(beta_x~psi_x~lambda) + geom_boxplot() + theme_few() 

ccsigma %>% subset(alpha == 400) %>% 
  ggplot(aes(lambda, avgFE, col = factor(kappa))) +
  facet_grid(beta~psi_x) + geom_path() + theme_few() + ggtitle(expression(alpha == 100)) + 
  ylim(c(0,1))

p1 <- list()
for(a in c(100,400)) for(b in c(100,400)){
  p1[[length(p1)+1]] <- ccmusigma %>% subset(alpha == a & beta == b) %>% 
    ggplot(aes(kappa, SAI_recent, col = factor(lambda))) +
    facet_grid(beta_x~psi_x) + geom_point() + geom_path() + theme_few() + 
    ggtitle(bquote(alpha == .(a) ~ beta == .(b))) # + 
    #coord_cartesian(ylim=c(.5,.75))
}
x11()
grid.arrange(grobs = p1)


p1 <- list()
for(a in c(100,400)) for(b in c(100,400)){
  p1[[length(p1)+1]] <- ccmusigma %>% subset(alpha == a & beta == b) %>% 
    ggplot(aes(kappa, SAI_recent, col = factor(lambda))) +
    facet_grid(beta_x~psi_x) + geom_point() + geom_path() + theme_few() + 
    ggtitle(bquote(alpha == .(a) ~ beta == .(b))) # + 
  #coord_cartesian(ylim=c(.5,.75))
}
x11()
grid.arrange(grobs = p1)



keepingup.plots <- list()
df <- ccmusigma %>% subset(alpha == 100 & beta == 400 & beta_x != 0 & lambda > 40)
df %>% ggplot(aes(kappa, SAI_recent, col = factor(lambda))) + 
  geom_hline(yintercept = 0, col = "grey", lty = 3, lwd = 2) +
  facet_grid(beta_x~psi_x) + geom_point() + geom_path() + theme_few() 



df <- ccmusigma %>% subset(alpha == 100 & beta == 400 & beta_x != 0 & lambda == 80)
df %>% ggplot(aes(avgFE, SAI_recent)) + 
  geom_hline(yintercept = 0, col = "grey") +
  facet_grid(beta_x~psi_x) + geom_path(col = "grey") + theme_few() +
  geom_point(aes( col = factor(kappa)))
