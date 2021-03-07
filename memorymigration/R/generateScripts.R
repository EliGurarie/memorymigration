#' Create Sources
#' 
#' Generates R.scripts to run many different parameters of the same model
#' 
#' @param worldname string containing name of the world
#' @param resourcename string containing name of resource
#' @param code.dir string containing directory of where R scripts will be created
#' @param filename string containing base name of the file to be created 
#' @param results.dir string containing directory of where results from the R scripts will be stored
#' @param epsilon maximum value of epsilon parameter
#' @param depsilon a factor of the value of epsilon to 
#' indicate the different values of epsilon as a parameter. Then the epsilon values 
#' evaluated in the model will be 0:epsilon in equal steps of this difference value.
#' Set this value as 0 if only value of epsilon should be evaluated
#' @param alpha maximum value of alpha parameter
#' @param dalpha a factor of the value of alpha to 
#' indicate the different values of alpha as a parameter. Then the alpha values 
#' evaluated in the model will be 0:alpha in equal steps of this difference value.
#' Set this value as 0 if only value of alpha should be evaluated
#' @param beta0 maximum value of beta0 parameter
#' @param dbeta0 a factor of the value of beta0 to 
#' indicate the different values of beta0 as a parameter. Then the beta0 values 
#' evaluated in the model will be 0:beta0 in equal steps of this difference value.
#' Set this value as 0 if only value of beta0 should be evaluated
#' @param beta1 maximum value of beta0 parameter
#' @param dbeta1 a factor of the value of beta1 to 
#' indicate the different values of beta1 as a parameter. Then the beta1 values 
#' evaluated in the model will be 0:beta1 in equal steps of this difference value.
#' Set this value as 0 if only value of beta1 should be evaluated
#' @return creates .R files with scripts 
#' @export
#' @seealso \link{createShellScript}, \link{parameterGrid}, \link{createFinalShellScript}
#' @examples
#' createSource(worldname = "world", resourcename = "resource_R1", 
#' code.dir = "scripttest", filename = "testing", results.dir="results", 
#' epsilon = 5, depsilon = 1, alpha = 5, dalpha = 1, beta0 = 3, dbeta0 = 1, beta1 = 2, dbeta1 = 0)


createSource <- function(worldname = "world", resourcename, 
                         code.dir, filename, results.dir,
                         epsilon, depsilon, alpha, dalpha, 
                         beta0, dbeta0, beta1, dbeta1){
  runparametersplit <- parameterGrid(epsilon, depsilon, alpha, dalpha, 
                                     beta0, dbeta0, beta1, dbeta1)
  for(i in 1:length(runparametersplit)){
    sink(paste0(code.dir, "/", filename, i, ".R"))
    cat(
      "require(memorymigration)\n",
      "data(world); data(resources)\n",
      "world$resource <-", resourcename,"\n",
      paste0("parametersplit <- parameterGrid(",epsilon,",", depsilon,",", alpha,",", dalpha,",", 
             beta0,",", dbeta0,",", beta1,",", dbeta1,")\n"))
    cat(paste0("parameters.df",i, "= parametersplit[[",i,"]]\n"),
        "results <- runManyRuns(",paste0("parameters.df",i,", world)\n"),
        "save(results, file =",paste0("paste0('~/Rprojects/memorymigration/",results.dir,"/run",i,".rda'))\n"))
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
  cat("#SBATCH --time=06:00:00 \n")
  cat(paste0("#SBATCH --job-name=", runname, "\n"))
 
    cat(paste0("R CMD BATCH ~/Rprojects/memorymigration/",code.dir,"/", files[i],"\n"))
  sink() }
  
  }
  
#' Create Final Shell Scripts
#' 
#' Generates one .sh file listing all of the .sh files for model to run at once
#' 
#' @param shellfinal.dir string containing directory of where this final shell script will be created
#' @param shell.dir string containing directory of where shell scripts that were created are located
#' @param runname string containing run name for server 
#' @return creates one .sh file
#' @seealso \link{createSource}, \link{parameterGrid}, \link{createShellScripts}
#' @export

createFinalShellScript <- function(shellfinal.dir, shell.dir, runname){
  files <- list.files(shell.dir)
  sink(paste0(shellfinal.dir, "/", runname, "runthisshell", ".sh"))
  cat("#!/bin/bash \n")
  for(i in 1:length(files)){
    cat(paste0("sbatch --share ", files[i], "\n"))}
    sink()
}

  

#' Parameter Grid
#' 
#' Generates a data frame splitting up all of the combinations of parameters
#' 
#' @param epsilon maximum value of epsilon parameter
#' @param depsilon a factor of the value of epsilon to 
#' indicate the different values of epsilon as a parameter. Then the epsilon values 
#' evaluated in the model will be 0:epsilon in equal steps of this difference value.
#' Set this value as 0 if only value of epsilon should be evaluated
#' @param alpha maximum value of alpha parameter
#' @param dalpha a factor of the value of alpha to 
#' indicate the different values of alpha as a parameter. Then the alpha values 
#' evaluated in the model will be 0:alpha in equal steps of this difference value.
#' Set this value as 0 if only value of alpha should be evaluated
#' @param beta0 maximum value of beta0 parameter
#' @param dbeta0 a factor of the value of beta0 to 
#' indicate the different values of beta0 as a parameter. Then the beta0 values 
#' evaluated in the model will be 0:beta0 in equal steps of this difference value.
#' Set this value as 0 if only value of beta0 should be evaluated
#' @param beta1 maximum value of beta0 parameter
#' @param dbeta1 a factor of the value of beta1 to 
#' indicate the different values of beta1 as a parameter. Then the beta1 values 
#' evaluated in the model will be 0:beta1 in equal steps of this difference value.
#' Set this value as 0 if only value of beta1 should be evaluated
#' @return list of data frames 
#' @seealso \link{createShellScript}, \link{createSource}, \link{createFinalShellScript}
#' @export
#' @examples
#' parameterGrid(epsilon = 5, depsilon = 1, alpha = 5, dalpha = 1, beta0 = 3, dbeta0 = 1, beta1 = 2, dbeta1 = 0)

parameterGrid <- function(epsilon, depsilon, alpha, dalpha, 
beta0, dbeta0, beta1, dbeta1){
  if(depsilon>0){epsilonvector <- seq(0, epsilon, depsilon)} else 
    epsilonvector <- c(epsilon)
  if(dalpha>0){alphavector <- seq(0, alpha, dalpha)} else alphavector <- c(alpha)
  if(dbeta0>0){beta0vector <- seq(0, beta0, dbeta0)} else beta0vector <- c(beta0)
  if(dbeta1>0){beta1vector <- seq(0, beta1, dbeta1)} else beta1vector <- c(beta1)
  parametersexpansion <- expand.grid(epsilon = epsilonvector, alpha = alphavector, beta0 = beta0vector, beta1 = beta1vector)
  parametersplit <- split(parametersexpansion, parametersexpansion$epsilon)
  parametersplit
}
