rm(list=ls())
library(shiny)

ui <- pageWithSidebar(
  
  headerPanel("Memory Migration model"),
  sidebarPanel(
    numericInput(inputId = "years", label = "Duration of simulation:", value = 5, step = 1),
    numericInput(inputId = "threshold", label = "Threshold of Similarity", value = 0.95, min = 0, max = 1, step = 0.01),
    numericInput(inputId = "alphabeta",
                label = "Total taxis",
                value = 100, min = 0, step = 0.01),
    sliderInput(inputId = "kappa",
                 label = "Proportion resource following vs. memory",
                 value = 50, min = 0, max = 1),
    sliderInput(inputId = "gamma",
                label = "Proportion reference memory",
                value = 1, min = 0, max = 1),
    numericInput(inputId = "epsilon",
                label = "Diffusion Parameter",
                value = 1, min = 0, step = 0.01),
    radioButtons(inputId = "x.sd",
                 label = "Resource Space Distribution",
                 choices = seq(3,15,3), inline = TRUE),
    radioButtons(inputId = "t.sd",
                 label = "Resource Time Distribution",
                 choices = seq(3,15,3), inline = TRUE),
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
    worldresource <- paste0("R_t",input$t.sd,"_x",input$x.sd)
    world$resource <- input$resource[[worldresource]]
    
    input.numeric <- lapply(input, as.numeric)
    parameters <- with(input.numeric,list( 
                       epsilon = epsilon, 
                       alpha = alphabeta * kappa,
                       beta = alphabeta * (1-kappa),
                       gamma = gamma))
      
   runManyYears(World=get(input$world), parameters = parameters, 
                                  n.years = as.numeric(input$years), 
                threshold = as.numeric(input$threshold), verbose=FALSE)
    
  })
  output$Image <- renderPlot({
    plotManyRuns(simulationRun(), nrow=1)
  }, res = 150)
}

shinyApp( ui=ui, server=server)
