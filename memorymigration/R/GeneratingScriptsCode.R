#' Create Sources
#' 
#' Generates R.scripts to run many different parameters of the same model
#' 
#' @param worldname string containing name of the world
#' @param resourcename string containing name of resource
#' @param directory string containing directory of where file will be created
#' @param filename string containing base name of the file to be created 
#' @param epsilon maximum value of epsilon parameter
#' @param epsilon.difference a factor of the value of epsilon to 
#' indicate the different values of epsilon as a parameter. Then the epsilon values 
#' evaluated in the model will be 0:epsilon in equal steps of this difference value.
#' Set this value as 0 if only value of epsilon should be evaluated
#' @param alpha maximum value of alpha parameter
#' @param alpha.difference a factor of the value of alpha to 
#' indicate the different values of alpha as a parameter. Then the alpha values 
#' evaluated in the model will be 0:alpha in equal steps of this difference value.
#' Set this value as 0 if only value of alpha should be evaluated
#' @param beta0 maximum value of beta0 parameter
#' @param beta0.difference a factor of the value of beta0 to 
#' indicate the different values of beta0 as a parameter. Then the beta0 values 
#' evaluated in the model will be 0:beta0 in equal steps of this difference value.
#' Set this value as 0 if only value of beta0 should be evaluated
#' @param beta1 maximum value of beta0 parameter
#' @param beta1.difference a factor of the value of beta1 to 
#' indicate the different values of beta1 as a parameter. Then the beta1 values 
#' evaluated in the model will be 0:beta1 in equal steps of this difference value.
#' Set this value as 0 if only value of beta1 should be evaluated
#' @return creates .R files with scripts 
#'  @seealso \link{createShellScript}

createSource <- function(worldname, resourcename, directory, filename, episilon, epsilon.difference, alpha, alpha.difference, beta0, beta0.difference, beta1, beta1.difference){
  if(epsilon.difference>0){epsilonvector <- seq(0, episilon, episilon.difference)} else epsilonvector <- c(episilon)
  if(alpha.difference>0){alphavector <- seq(0, alpha, alpha.difference)} else alphavector <- c(alpha)
  if(beta0.difference>0){beta0vector <- seq(0, beta0, beta0.difference)} else beta0vector <- c(beta0)
  if(beta1.difference>0){beta1vector <- seq(0, beta1, beta1.difference)} else beta1vector <- c(beta1)
  parametersexpansion <- expand.grid(epsilon = epsilonvector, alpha = alphavector, beta0 = beta0vector, beta1 = beta1vector)
  for(i in 1:nrow(parametersexpansion)){
    parameters <- parametersexpansion[i,]
  sink(paste0(filename, i, ".R"))
  cat(
    "require(memorymigration)\n",
    paste0("load(",worldname,")\n"),
    paste0("load(",resourcename,")\n"),
    paste("parameters.df = c(", paste0(names(parameters), "=", parameters, collapse = ", "), ")\n"), 
    "world$resource <- resourcename \n",
    "M <- runManyYears(world, Parameters = parameters, n.years = 30, threshold = 0.99) \n", 
    "indices <- computeIndices(M[[length(M)]], world$resource, world) \n",
    paste0("R", i), "<- data.frame(t(parameters), indices) \n",
    "save(", paste0("R", i, ", file =", "paste0(directory,", "results/run", i,".rda)", ")"))
  sink()
  }}

#'Create Shell Script
#'
#'Generates .sh files to run many different parameters of the same model through
#'the shell. 
#'
#'@param worldname string containing name of the world of the model
#' @param resourcename string containing name of resource of the model
#' @param directory string containing directory of where file results should be stored
#' @param filename string containing base name of the file of the R.script to run 
#' through the shell
#' @param epsilon maximum value of epsilon parameter
#' @param epsilon.difference a factor of the value of epsilon to 
#' indicate the different values of epsilon as a parameter. Then the epsilon values 
#' evaluated in the model will be 0:epsilon in equal steps of this difference value.
#' Set this value as 0 if only value of epsilon should be evaluated
#' @param alpha maximum value of alpha parameter
#' @param alpha.difference a factor of the value of alpha to 
#' indicate the different values of alpha as a parameter. Then the alpha values 
#' evaluated in the model will be 0:alpha in equal steps of this difference value.
#' Set this value as 0 if only value of alpha should be evaluated
#' @param beta0 maximum value of beta0 parameter
#' @param beta0.difference a factor of the value of beta0 to 
#' indicate the different values of beta0 as a parameter. Then the beta0 values 
#' evaluated in the model will be 0:beta0 in equal steps of this difference value.
#' Set this value as 0 if only value of beta0 should be evaluated
#' @param beta1 maximum value of beta0 parameter
#' @param beta1.difference a factor of the value of beta1 to 
#' indicate the different values of beta1 as a parameter. Then the beta1 values 
#' evaluated in the model will be 0:beta1 in equal steps of this difference value.
#' Set this value as 0 if only value of beta1 should be evaluated
#' @return creates .sh files 
#' @seealso \link{createSource}
createShellScript <- function(worldname, resourcename, directory, filename, episilon, epsilon.difference, alpha, alpha.difference, beta0, beta0.difference, beta1, beta1.difference){
  if(epsilon.difference>0){epsilonvector <- seq(0, episilon, episilon.difference)} else epsilonvector <- c(episilon)
  if(alpha.difference>0){alphavector <- seq(0, alpha, alpha.difference)} else alphavector <- c(alpha)
  if(beta0.difference>0){beta0vector <- seq(0, beta0, beta0.difference)} else beta0vector <- c(beta0)
  if(beta1.difference>0){beta1vector <- seq(0, beta1, beta1.difference)} else beta1vector <- c(beta1)
  parametersexpansion <- expand.grid(epsilon = epsilonvector, alpha = alphavector, beta0 = beta0vector, beta1 = beta1vector)
  for(i in 1:nrow(parametersexpansion)){
  sink(paste0(filename,i,".sh"))
  cat(
    paste("#!/bin/bash \n"),
    paste("#SBATCH --ntasks=1 \n"),
    paste("#SBATCH --time=00:05:00 \n"), 
    paste0("#SBATCH --job-name=",worldname, resourcename, i, "\n"),
    paste0("R CMD BATCH /research-home/",directory,"/", filename, i, ".R) \n")
  )
  sink()}
  }
