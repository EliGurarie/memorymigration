require(memorymigration)
 data(world); data(resources)
 parameters = c( epsilon=4, alpha=5, beta0=2, beta1=2 )
 world$resource <-  resource_R1 
 M <- runManyYears(world, Parameters = parameters, n.years = 30, threshold = 0.99) 
 indices <- computeIndices(M[[length(M)]], world$resource, world) 
 R5 <- data.frame(t(parameters), indices) 
 save( R5, file =paste0('scripttest/results/run5.rda'))