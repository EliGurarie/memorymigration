#!/bin/bash 
#SBATCH --ntasks=6
#SBATCH --time=06:00:00 
#SBATCH --job-name=testing
R CMD BATCH /research-home/testscripts/Rcode//testing1.R
R CMD BATCH /research-home/testscripts/Rcode//testing2.R
R CMD BATCH /research-home/testscripts/Rcode//testing3.R
R CMD BATCH /research-home/testscripts/Rcode//testing4.R
R CMD BATCH /research-home/testscripts/Rcode//testing5.R
R CMD BATCH /research-home/testscripts/Rcode//testing6.R
