rm(list=ls())

try(setwd("C:/Users/Elie/box sync/UWiscLLab"))
try(setwd("/home/elzizi/Dropbox/CWD/"))
try(setwd("/home/elie/Dropbox/CWD/"))

pcks <- c("shiny","sf","ggplot2","magrittr","plyr", "gplots")
lapply(pcks, require, character = TRUE)

load("data/WI_data.rda")
WI_data$Z <- WI_data$X.km + 1i*WI_data$Y.km
M.distance <- with(WI_data, outer(Z,Z, function(z1,z2) Mod(z2-z1)))
source("functions.R")
source("SIP_ShinyScripts.R"); 
shinyApp(ui_SIP, server_SIP)
  