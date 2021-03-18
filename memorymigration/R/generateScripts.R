#' Create Sources
#' 
#' Generates R.scripts to run many different parameters of the same model
#' 
#' @param worldname string containing name of the world
#' @param resourcename string containing name of resource
#' @param code.dir string containing directory of where R scripts will be created
#' @param filename string containing base name of the file to be created 
#' @param results.dir string containing directory of where results from the R scripts will be stored
#' @param epsilons values of epsilon parameter
#' @param alphas values value of alpha parameter
#' @param beta0s values value of beta0 parameter
#' @param beta1s values value of beta1 parameter
#' @param existing a data frame with existing values already tested
#' for server to access;  if there is no existing values, enter an empty data frame. 
#' Save this data frame as results. 
#' @param existingfile name of file containing a data frame with existing values already tested
#' for server to access; 
#' if there is no existing values, enter create a file with an empty data frame
#' @return creates .R files with scripts 
#' @export
#' @seealso \link{createShellScript}, \link{parameterGrid}, \link{createFinalShellScript}
#' @examples
#' 


createSource <- function(worldname = "world", resourcename, 
                         code.dir, filename, results.dir,
                         epsilons, alphas, beta0s,  beta1s,
                         existing, existingfile){
  runparametersplit <- parameterGrid(epsilons, alphas, beta0s, beta1s, existing)

  for(i in 1:length(runparametersplit)){
    sink(paste0(code.dir, "/", filename, i, ".R"))
    cat(
      "require(memorymigration)\n",
      "data(world); data(resources)\n",
      "world$resource <-", resourcename,"\n",
      paste0("load('", existingfile, "') \n"),
      paste0("parametersplit <- parameterGrid(", 
             list(epsilons), ",", list(alphas), ",", list(beta0s), ",", list(beta1s), 
             ", results)\n"))
    cat(paste0("parameters.df",i, "= parametersplit[[",i,"]]\n"),
        "newresults <- runManyRuns(",paste0("parameters.df",i,", world)\n"),
        "save(newresults, file =",paste0("paste0('~/Rprojects/memorymigration/",results.dir,"/",filename, "run_", i,".rda'))\n"))
    sink()}
}

#'Create Shell Scripts
#'
#'Generates .sh files to run many different parameters of the same model through
#'the shell. 
#'
#'@param shell.dir string containing directory of where shell scripts will be created
#' @param code.dir string containing directory of where R scripts our scored
#' @param runname string containing run name for server 
#' @param filename string containing base name of the file of the R.script to run 
#' through the shell
#' @return creates a .sh file for each R script in code.dir
#' @export
#' @seealso \link{createSource}, \link{parameterGrid}, \link{createFinalShellScript}

createShellScripts <- function(shell.dir, code.dir, runname, filename){
  files <- list.files(code.dir)
  for(i in 1:length(files)){
  sink(paste0(shell.dir, "/", filename, i, ".sh"))
  cat("#!/bin/bash \n")
  cat("#SBATCH --ntasks=1 \n")
  cat("#SBATCH --time='UNLIMITED' \n")
  cat(paste0("#SBATCH --job-name=", runname, "\n"))
 
    cat(paste0("R CMD BATCH ~/Rprojects/memorymigration/",code.dir,"/", files[i],"\n"))
  sink() }
  
  }
  
#' Create Final Shell Scripts
#' 
#' Generates one .sh file listing all of the .sh files for model to run at once
#' Make sure to delete this file if you need to rewrite it 
#' 
#' @param shell.dir string containing directory of where shell scripts that were created are located;
#' this is also where this .sh file will be located
#' @param runname string containing run name for server 
#' @return creates one .sh file
#' @seealso \link{createSource}, \link{parameterGrid}, \link{createShellScripts}
#' @export

createFinalShellScript <- function(shell.dir, runname){
  files <- list.files(shell.dir)
  sink(paste0(shell.dir, "/", runname, "runthisshell", ".sh"))
  cat("#!/bin/bash \n")
  for(i in 1:length(files)){
    cat(paste0("sbatch --share ", files[i], "\n"))}
  sink()
}


#' Parameter Grid
#' 
#' Generates a data frame splitting up all of the combinations of parameters
#' 
#' @param epsilons values of epsilon parameter
#' @param alphas values value of alpha parameter
#' @param beta0s values value of beta0 parameter
#' @param beta1s values value of beta1 parameter
#' @param existing a data frame with existing values already tested; if there is no 
#' existing values, enter this parameter as an empty data frame
#' @return list of data frames 
#' @seealso \link{createShellScript}, \link{createSource}, \link{createFinalShellScript}
#' @export
#' @examples
#' parameterGrid(c(1,2,3), c(2,3), seq(1,4), 4)

parameterGrid <- function(epsilons, alphas, beta0s, beta1s, existing){
  params.df <- expand.grid(epsilon = epsilons, alpha = alphas, beta0 = beta0s, beta1 = beta1s)
  params.df <- setdiff(params.df, existing)
  
  parametersplit <- split(params.df, params.df$alpha)
  parametersplit
}
