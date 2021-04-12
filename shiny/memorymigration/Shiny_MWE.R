rm(list=ls())

pcks <- c("shiny","sf","ggplot2","magrittr","plyr", "gplots", "memorymigration")
lapply(pcks, require, character = TRUE)

data(world)
data(resources_island)
data(resources_drifting)

source("shiny/memorymigration/ShinyScripts.R")
shinyApp(ui, server)
