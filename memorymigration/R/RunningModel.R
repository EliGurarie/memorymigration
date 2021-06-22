#' Run Missed Runs Resource
#' 
#' @export

runMissedRuns_res <- function (world_param, parameters.df, resource_param, world, resource, 
                               filename = NULL, results.dir = NULL, ...) {
  newresults <- data.frame()
  FE.matrix <- matrix(NA, nrow=nrow(parameters.df)*nrow(resource_param), ncol = resource_param$n.years[1])
  
  for(i in 1:nrow(parameters.df)) {
    par0 <- with(resource_param, 
                 getCCpars(mu_x0 = mu_x0[i],
                           mu_t0 = mu_t0[i],
                           beta_x = beta_x[i],
                           beta_t = beta_t[i],
                           n.years = n.years[i],
                           sigma_x = sigma_x[i],
                           sigma_t = sigma_t[i],
                           psi_x = psi_x[i], 
                           psi_t = psi_t[i]))
    
    
    world <- with(world_param, getOptimalPop(tau = tau, X.min = X.min,
                                             X.max = X.max, dx = dx,
                                             x1 = x1, x2 = x2, t.peak = t.peak,
                                             x.sd = resource_param$sigma_x[i],
                                             t.sd = resource_param$sigma_t[i]))
    
    world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
    
    if(resource == "drifting")
      world$resource <- aaply(par0, 1, function(p) getResource_drifting(world, p, x.null=50)) 
    if(resource == "island")
      world$resource <- aaply(par0, 1, function(p) getResource_island(world, p)) 
    
    
    attr(world$resource, "par") <- par0[nrow(par0),]
    
    myparams <- with(parameters.df[i,], 
                     c(epsilon = epsilon, 
                       alpha = alpha, 
                       beta = beta, 
                       kappa = kappa, 
                       lambda = lambda))
    
    M <- try(runManyYears(world, parameters = myparams, 
                          n.years = 100, threshold = 0.9999))
    
    
    
    if(!inherits(M, "try-error")){
      myFE <- computeAnnualEfficiency(M$pop, world$resource, world)
      FE.matrix[nrow(newresults)+1, 1:length(myFE)] <- myFE
      
      myR <- data.frame(parameters.df[i, ], computeIndices(M$pop[[length(M$pop)]], 
                                                           world$resource[length(M$pop)-1,,], world),
                        computeMigrationIndices(M, world),
                        avgFE = computeAvgEfficiency(M$pop, world$resource, world),
                        n.runs = length(M$pop) - 1,
                        final_similarity = computeEfficiency(M$pop[[length(M$pop)-1]], 
                                                             M$pop[[length(M$pop)]], world), 
                        
                        resource_param[i,],
                        resource = resource)
      
      newresults <- rbind(newresults, c(myR))
    }
    
    if(!is.null(results.dir) & (i %% 10 == 0 | i == max(i)))  
      save(newresults, file =paste0(results.dir,"/",filename,".rda"))
  }
  newresults$annualFE <- FE.matrix[1:nrow(newresults),]
  return(newresults) 
}