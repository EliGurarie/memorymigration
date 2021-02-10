
parameters <- c(epsilon = 10, alpha = 300, beta0=100, beta1 = 300)


createSource <- function(parameters, worldname, resourcename, directory, filename){
  dput(parameters) %>% is

  sink(filename)
  cat(
    "require(memorymigration)\n",
    paste0("load(",worldname,")\n"),
    paste0("load(",resourcename,")\n"),
    paste("parameters.df = c(", paste0(names(parameters), "=", parameters, collapse = ", "), ")\n"), 
    "world$resource <- resourcename \n",
    "M <- runManyYears(world, Parameters = parameters, n.years = 30, threshold = 0.99) \n", 
    "indices <- computeIndices(M[[length(M)]], world$resource, world) \n",
    "R2 <- data.frame(t(parameters), indices) \n", 
    paste("save(R2, file = paste0(", directory, "results/run2.rda"))
  sink()
}

createSource(parameters, "world", "resource", ".", "testing.R")

####################
# OLD EXAMPLE


# Break up the D's - each separate
try(setwd("c:/users/guru/box sync/non-local information"))
try(setwd("c:/users/elie/box sync/non-local information"))

for(detection in c("Uniform", "Gaussian", "Exponential"))
  for(resource in c("Square", "Gaussian")){
    filename <- paste0(detection, ".", resource, ".D0.robj")
    sink(paste0("./rcode/RunSims/D0/", detection, resource,".r"))
    cat(
      "source('./rcode/Source04_MasterModelRunWithDerivatives.r')
        alphas =  10^(seq(0,5,.5))
        rs = 1:20; Ds = ", d, "
        RunModel(alphas,Ds,rs,n.osc = 4, Detection = '", detection, "', Resource = '", resource, "', \n filename = '", filename, "', directory = './results/D0/') \n", sep="")
    sink()
  }


# Generate shell scripts

Ds <- 0
for(detection in c("Uniform", "Gaussian", "Exponential"))
  for(resource in c("Square", "Gaussian"))
  {
    runcodecode <- paste0("R CMD BATCH ./rcode/RunSims/D0/",detection, resource, ".r\n")
    sink(paste0("./rcode/shellscripts/Run",detection,resource,"D0.sh"))
    cat(
      paste0(
        "#!/bin/bash \n",
        "#SBATCH --ntasks=1 \n",
        "#SBATCH --time=06:00:00 \n", 
        "#SBATCH --job-name=",detection,resource,"\n",
        "module load dept/bioinfo \n",
        "module load bioinfo/R \n"), runcodecode)
    sink()
  }
