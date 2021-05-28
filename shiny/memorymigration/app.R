pcks <- c("shiny","sf","ggplot2","magrittr","plyr", "gplots", "memorymigration", "DT", "ggthemes")
lapply(pcks, require, character = TRUE)

source("shiny/memorymigration/ShinyScripts.R")
shinyApp(ui, server)


