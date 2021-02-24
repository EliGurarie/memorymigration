#!/bin/bash 
 #SBATCH --ntasks=6
 #SBATCH --time=06:00:00 
 #SBATCH --job-name=worldresource_R1
R CMD BATCH /research-home/scripttest/testing1.R) 
R CMD BATCH /research-home/scripttest/testing2.R) 
R CMD BATCH /research-home/scripttest/testing3.R) 
R CMD BATCH /research-home/scripttest/testing4.R) 
R CMD BATCH /research-home/scripttest/testing5.R) 
R CMD BATCH /research-home/scripttest/testing6.R) 
