rm(list=ls())
library(shiny)

ui <- pageWithSidebar(
  
  headerPanel("Memory Migration model"),
  sidebarPanel(
    numericInput(inputId = "years", label = "Duration of simulation:", value = 5, step = 1),
    numericInput(inputId = "threshold", label = "Threshold of Similarity", value = 0.95, min = 0, max = 1, step = 0.01),
    numericInput(inputId = "alpha",
                label = "Resource Following Parameter",
                value = 0, min = 0, step = 0.01),
    numericInput(inputId = "beta",
                label = "Memory Parameter",
                value = 0, min = 0, step = 0.01),
    numericInput(inputId = "gamma",
                label = "Generation Memory Parameter",
                value = 0, min = 0, max = 1, step = 0.01),
    numericInput(inputId = "epsilon",
                label = "Diffusion Parameter",
                value = 0, min = 0, step = 0.01),
    radioButtons(inputId = "x.sd",
                 label = "Resource Space Distribution",
                 choices = c("3" = "x3", "6" = "x6", "9" = "x9", "12" = "x12", "15" = "x15"), inline = TRUE),
    radioButtons(inputId = "t.sd",
                 label = "Resource Time Distribution",
                 choices = c("3" = "t3", "6" = "t6", "9" = "t9", "12" = "t12", "15" = "t15"), inline = TRUE),
    radioButtons(inputId = "resource",
                 label = "Type of resource", 
                 choices = c("Island" = "resources_island", "Drifting" = "resources_drifting"), inline = TRUE),
    radioButtons(inputId = "world",
                 label = "Initial distribution of population in year 0", 
                 choices = c("Gaussian" = "world_gaussian", "Sinusoidal" = "world_sinusoidal"), inline = TRUE)
  ),
  
  mainPanel(
    plotOutput("Image", height = "800px")
  )
)


server <- function(input, output) {
  pcks <- c("shiny","sf","ggplot2","magrittr","plyr", "gplots", "memorymigration")
  lapply(pcks, require, character = TRUE)
  simulationRun <- reactive({
    data(world)
    data("resources_drifting")
    data("resources_island")
    world <- get(input$world)
    worldresource <- paste0("R_",input$t.sd,"_",input$x.sd)
    world$resource <- get(input$resource)[[worldresource]]
    
   runManyYears(World=get(input$world), parameters = c(epsilon = as.numeric(input$epsilon), 
                                                       alpha = as.numeric(input$alpha), 
                                                        beta = as.numeric(input$beta),
                                                       gamma = as.numeric(input$gamma)), 
                                  n.years = as.numeric(input$years), threshold = as.numeric(input$threshold), verbose=FALSE)
    
  })
  output$Image <- renderPlot({
    plotManyRuns(simulationRun(), nrow=1)
  }, res = 150)
}

shinyApp( ui=ui, server=server)
