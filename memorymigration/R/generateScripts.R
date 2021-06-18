#' Create Sources
#' 
#' Generates R.scripts to run many different parameters of the same model
#' 
#' @param worldname string containing name of the world
#' @param resourcename string containing name of resource
#' @param resourcefile string containng the name of rda file that contains the resource
#' @param code.dir string containing directory of where R scripts will be created
#' @param filename string containing base name of the file to be created 
#' @param results.dir string containing directory of where results from the R scripts will be stored
#' @param epsilons values of epsilon parameter
#' @param alphas values value of alpha parameter
#' @param beta0s values value of beta0 parameter
#' @param beta1s values value of beta1 parameter
#' @param existing a data frame with existing values already tested
#' for server to access. Save the existing values as results
#' @param existingfile string name of file containing a data frame with existing values already tested
#' for server to access
#' @return creates .R files with scripts 
#' @export
#' @seealso \link{createShellScript}, \link{parameterGrid}, \link{createFinalShellScript}
#' @examples
#' 


createSource <- function(worldname, resourcename, resourcefile, 
                         code.dir, filename, results.dir,
                         epsilons, alphas, beta0s,  beta1s,
                         existing=NULL, existingfile=NULL){
  if(!is.null(existing)){
    runparametersplit <- parameterGrid(epsilons, alphas, beta0s, beta1s, existing)
  }
  if(is.null(existing)){
    runparametersplit <- parameterGrid(epsilons, alphas, beta0s, beta1s)
  }

  for(i in 1:length(runparametersplit)){
    sink(paste0(code.dir, "/", filename, i, ".R"))
    cat(
      "require(memorymigration)\n",
      paste0("data(world); data(",resourcefile,")\n"),
      paste0("world<-", worldname,"\n"),
      paste0("world$resource <-", resourcefile, "[['", resourcename,"']]\n"))
      if(!is.null(existingfile)){
        cat(paste0("load('", existingfile, "') \n"))}
      
    if(!is.null(existing)){
          cat(paste0("parametersplit <- parameterGrid(", 
             list(epsilons), ",", list(alphas), ",", list(beta0s), ",", list(beta1s), 
             ", results)\n"))}
    
    if(is.null(existing)){
      cat(paste0("parametersplit <- parameterGrid(", 
                   list(epsilons), ",", list(alphas), ",", list(beta0s), ",", list(beta1s), 
                    ")\n"))}
      
  if(is.null(existing)){cat(paste0("parameters.df",i, "= parametersplit[[",i,"]]\n"),
                            "newresults <- runManyRuns(",paste0("parameters.df",i,", world,'", filename, "','", results.dir,"')"))}
  
  if(!is.null(existing)){cat(paste0("parameters.df",i, "= parametersplit[[",i,"]]\n"),
                            "newresults <- runManyRuns(",paste0("parameters.df",i,", world,'", filename, "','", results.dir,"')"))}}
  
    sink()
    }


# save(newresults, file =",paste0("paste0('~/Rprojects/memorymigration/",results.dir,"/",filename, "run_", i,".rda'))\n"))


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
  cat("#SBATCH --exclusive=user \n")
 
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
#' @param epsilons values of epsilon parameter in a vector
#' @param alphas values of alpha parameter in a vector
#' @param betas values of beta parameter in a vector
#' @param kappas values of kappa parameter in a vector
#' @param lambdas values of lambda parameter in a vector
#' @param existing a data frame with existing values already tested
#' @return list of data frames 
#' @seealso \link{createShellScript}, \link{createSource}, \link{createFinalShellScript}
#' @export
#' @examples
#' parameterGrid(c(1,2,3), c(2,3), seq(1,4), 4, 20)

parameterGrid <- function(epsilons, alphas, betas, kappas, lambdas, existing=NULL){
  params.df <- expand.grid(epsilon = epsilons, alpha = alphas, beta = betas, kappa = kappas, lambda = lambdas)
  
  if(!is.null(existing))
    params.df <- setdiff(params.df, existing)
  
  params.df
}

#' Parameter Grid for Resource
#' 
#' Generates a data frame splitting up all of the combinations of parameters and considers resource
#' 
#' @export
parameterGridres <- function(params, existing){
  
    existing <- existing[c("epsilon","alpha","beta", "kappa", "lambda", "mu_x0", "mu_t0",
                           "beta_x", "beta_t", "n.years", "sigma_x", "sigma_t", "psi_x", "psi_t")]
    params.df <- dplyr::setdiff(params, existing)
  
  params.df
}
