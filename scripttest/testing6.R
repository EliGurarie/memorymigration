require(memorymigration)
 data(world); data(resources)
 parameters = c( epsilon=5, alpha=5, beta0=2, beta1=2 )
 world$resource <-  resource_R1 
 M <- runManyYears(world, Parameters = parameters, n.years = 30, threshold = 0.99) 
 indices <- computeIndices(M[[length(M)]], world$resource, world) 
 R6 <- data.frame(t(parameters), indices) 
 save( R6, file =paste0('scripttest/results/run6.rda'))