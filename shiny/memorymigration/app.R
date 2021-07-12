pcks <- c("shiny","memorymigration", "shinythemes")
lapply(pcks, require, character = TRUE)

source("shiny/memorymigration/ShinyScripts.R")
shinyApp(ui, server)
