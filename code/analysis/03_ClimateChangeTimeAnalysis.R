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
  
  files <- list.files("results/climate_time/raw/")
  
  cctime <- data.frame()
  for(f in files[grepl("rda", files)]){ 
    print(f)
    load(paste0("results/climate_time/raw/", f))
    cctime <- cctime %>% smartbind(newresults %>% fixTE)
  }
  
  save(cctime, file = "results/climate_time/cctime.rda")
}

load("results/climate_time/cctime.rda")


with(cctime, table(epsilon, beta_t, lambda))
head(cctime)

require(ggplot2)
cctime %>% subset(epsilon > 4 & beta_t == -1) %>% 
  ggplot(aes(alpha, beta, fill = cut(FE, seq(0,1,.2)))) + 
  geom_tile() + facet_grid(epsilon~lambda~beta_t) + ggtitle("Foraging efficiency")


cctime %>% subset(epsilon > 4 & beta_t == -1) %>% 
  ggplot(aes(alpha, beta, fill = cut(TE, seq(0,750,150)))) + 
  geom_tile() + facet_grid(epsilon~lambda~beta_t) + ggtitle("Foraging efficiency")


cctime %>% subset(epsilon > 4 & beta_t == -1) %>% 
  ggplot(aes(FE, TE, col = alpha)) + geom_point()  + facet_grid(.~lambda)
