#' Get Compiled Results
#' 
#' Compiles results from different folders into one data frame 
#' @param results.dir string containing directory of where results are stored
#' @return creates a data frame of all the results 
#' @export

getCompiledResults <- function(results.dir){
  files <- list.files(results.dir)
  results.compiled <- data.frame()
  for(f in files){
    print(f)
    load(paste0(results.dir, f))
    results.compiled <- rbind(results.compiled, newresults)
  }
  data.frame(rid = 1:nrow(results.compiled), results.compiled)
}

#' Plot Compiled
#' 
#' Plots compiled results 
#' @param r data to be plotted 
#' @return three grid plots for parameters of Social Cohesion, Foraging Efficiency, 
#' and Migratoriness
#' @export 

plotCompiled <- function (r) 
{
  require(ggplot2)
  p1 <- ggplot(r, aes(alpha, SC, col = factor(epsilon))) + 
    facet_grid(beta0 ~ beta1, labeller = label_both)  + 
    geom_path() + ggtitle(paste(r$.id[1], "Social Cohesion"))
  p2 <- ggplot(r, aes(alpha, FE, col = factor(epsilon))) + 
    facet_grid(beta0 ~ beta1, labeller = label_both)  + 
    geom_path() + ggtitle("Foraging Efficiency")
  p3 <- ggplot(r, aes(alpha, MI, col = factor(epsilon))) + 
    facet_grid(beta0 ~ beta1, labeller = label_both)  + 
    geom_path() + ggtitle("Migratoriness")
  require(gridExtra)
  grid.arrange(p1 + theme_few(), 
               p2 + theme_few(), 
               p3 + theme_few(), ncol = 3)
}

