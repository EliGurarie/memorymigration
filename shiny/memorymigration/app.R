pcks <- c("shiny","sf","ggplot2","magrittr","plyr", "gplots", 
          "memorymigration", "DT", "ggthemes", "minpack.lm", "fields","scales")
lapply(pcks, require, character = TRUE)

source("code/functions_v6.R")
source("code/functions_discretemigration.R")
source("code/functions_plottingresults.R")
source("shiny/memorymigration/ShinyScripts.R")
shinyApp(ui, server)



