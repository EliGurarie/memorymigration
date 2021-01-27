#' Run One Year forward

runNextYear <- function(World, Parameters){
  pop1 <- World$pop
  pop2 <- pop1*0
  #pop2[1,] <- pop1[nrow(pop1),]
  
  Time <- World$time
  Resource <- World$resource
  
  for(t in Time){
    pop_lastyear <- c(0,pop1[t,],0) 
    if(t == 1) pop_now <- pop1[nrow(pop1),] else pop_now <- pop2[t-1,]
    pop2[t,] <- ode(y = pop_now, 
                    times = 0:1, 
                    parms = Parameters,
                    func = ForagingMemoryModel, 
                    resource = c(0,Resource[t,],0),
                    pop_lag = pop_lastyear,
                    dx = World$dx)[2,1:ncol(pop2)+1]
  }
  pop2
}

#' Run several years forward

runManyYears <- function(World, Parameters, n.years){
  pop.list <- list(Year1 = World$pop)
  
  for(i in 1:n.years){
    cat(paste("running year ", i, "\n"))
    World$pop <- pop.list[[i]]
    pop.list[[i+1]] <- runNextYear(World, 
                                   Parameters = parameters)
  }
  names(pop.list) <- paste0("Year",0:n.years)
  pop.list
}
