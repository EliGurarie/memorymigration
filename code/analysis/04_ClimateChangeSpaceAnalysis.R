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


with(ccspace, table(epsilon, beta_x, lambda))
head(ccspace)

require(ggplot2)
ccspace %>% subset(beta_x == -1) %>% 
  ggplot(aes(alpha, beta, fill = cut(avgFE, seq(0,1,.2)))) + 
  geom_tile() + facet_grid(epsilon~lambda) + ggtitle("Foraging efficiency")


ccspace %>% subset(beta_x == -1) %>% 
  ggplot(aes(alpha, beta, fill = cut(n.runs, c(0,10,20,40,80,160)))) + 
  geom_tile() + facet_grid(epsilon~lambda) + ggtitle("N.runs")


ccspace %>% subset(beta_x == -1) %>% 
  ggplot(aes(alpha, beta, fill = cut(TE, seq(0,750,150)))) + 
  geom_tile() + facet_grid(epsilon~lambda) + ggtitle("Migration mismatch")


cctime %>% subset(epsilon > 4 & beta_t == -1) %>% 
  ggplot(aes(FE, TE, col = alpha)) + geom_point()  + facet_grid(.~lambda)
