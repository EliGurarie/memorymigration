require(memorymigration)
 load(world)
 load(resource)
 parameters.df = c( epsilon=10, alpha=300, beta0=100, beta1=300 )
 world$resource <- resourcename 
 M <- runManyYears(world, Parameters = parameters, n.years = 30, threshold = 0.99) 
 indices <- computeIndices(M[[length(M)]], world$resource, world) 
 R2 <- data.frame(t(parameters), indices) 
 save(R2, file = paste0( . results/run2.rda