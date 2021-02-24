require(memorymigration)

createSource(worldname = "world", 
             resourcename = "resource_R1", 
             directory = "scripttest", 
             filename = "testing", 
             epsilon = 5, depsilon = 1, 
             alpha = 5, dalpha = 1, 
             beta0 = 3, dbeta0 = 1, 
             beta1 = 2, dbeta1 = 0)



createShellScript(shell.dir = "testscripts", 
                  code.dir = "testscripts/Rcode", 
                  runname = "testing", 
                  filename = "scripttest")




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
