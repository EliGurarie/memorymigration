require(memorymigration)
 data(world); data(resources)
 parameters = c( epsilon=3, alpha=5, beta0=2, beta1=2 )
 world$resource <-  resource_R1 
 M <- runManyYears(world, Parameters = parameters, n.years = 30, threshold = 0.99) 
 indices <- computeIndices(M[[length(M)]], world$resource, world) 
 R4 <- data.frame(t(parameters), indices) 
 save( R4, file =paste0('scripttest/results/run4.rda'))