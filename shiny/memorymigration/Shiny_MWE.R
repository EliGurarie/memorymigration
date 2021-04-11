rm(list=ls())

pcks <- c("shiny","sf","ggplot2","magrittr","plyr", "gplots", "memorymigration")
lapply(pcks, require, character = TRUE)

data(world)
data(resource_island)
data(resources_drifting)

shinyApp(ui_SIP, server_SIP)
