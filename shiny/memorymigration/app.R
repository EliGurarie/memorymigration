pcks <- c("shiny","memorymigration")
lapply(pcks, require, character = TRUE)

source("shiny/memorymigration/ShinyScripts.R")
shinyApp(ui, server)
