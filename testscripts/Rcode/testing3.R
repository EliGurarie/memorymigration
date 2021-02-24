require(memorymigration)
 data(world); data(resources)
 world$resource <- resource_R1 
 parametersplit <- parameterGrid(5,1,5,1,3,1,2,0)
parameters.df3= parametersplit[[3]]
 results <- runManyRuns( parameters.df3, world)
 save(results, file = (paste0('scripttest/results/run3.rda')))
