 require(memorymigration)
 data(world); data(resources)
 
 parameters.df = data.frame( epsilon=seq(1, 100, length = 2), 
                             alpha=1000, beta0=100, beta1=200)
 
 world$resource <-  resource_R1 
 results <- runManyRuns(parameters.df, world)
 save(results, file =paste0('scripttest/results/run1.rda'))
 