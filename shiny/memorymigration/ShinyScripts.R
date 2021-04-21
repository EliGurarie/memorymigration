library(shiny)
ui <- pageWithSidebar(
  
  headerPanel("Memory Migration model"),
  sidebarPanel(
    numericInput(inputId = "years", label = "Duration of simulation:", value = 5, step = 1),
    numericInput(inputId = "threshold", label = "Threshold of Similarity", value = 0.999, min = 0, max = 1, step = 0.01),
    numericInput(inputId = "alphabeta",
                label = "Total taxis",
                value = 100, min = 0, step = 0.01),
    sliderInput(inputId = "kappa",
                 label = "Proportion resource following vs. memory",
                 value = .50, min = 0, max = 1),
    sliderInput(inputId = "gamma",
                label = "Proportion reference memory",
                value = 1, min = 0, max = 1),
    numericInput(inputId = "epsilon",
                label = "Diffusion Parameter",
                value = 1, min = 0, step = 0.01),
    sliderInput(inputId = "x.sd",
                 label = "Resource Space Distribution",
                value = 3, min = 0, max = 15),
    sliderInput(inputId = "t.sd",
                 label = "Resource Time Distribution",
                value = 3, min = 0, max = 15),
    numericInput(inputId = "mu.x0", label = "Initial Resource Position in Space", value = 80, step = 1),
    numericInput(inputId = "mu.t0", label = "Initial Resource Position in Time", value = 25, step = 1),
    numericInput(inputId = "beta.x", label = "Resource Change in Space", value = 0, step = 1),
    numericInput(inputId = "beta.t", label = "Resource Change in Time ", value = 0, step = 1),
    radioButtons(inputId = "resource",
                 label = "Type of resource", 
                 choices = c("Island" = "resources_island", "Drifting" = "resources_drifting"), inline = TRUE),
    radioButtons(inputId = "world",
                 label = "Initial distribution of population in year 0", 
                 choices = c("Gaussian" = "world_gaussian", "Sinusoidal" = "world_sinusoidal"), inline = TRUE),
    actionButton(inputId = "run", 
                 label = "Run Model")
  ),
  
  mainPanel(
    plotOutput("Image", height = "800px")
  )
)


server <- function(input, output) {
  pcks <- c("shiny","sf","ggplot2","magrittr","plyr", "gplots", "memorymigration")
  lapply(pcks, require, character = TRUE)
  simulationRun <- eventReactive(input$run, {
    data(world)
    world <- get(input$world)
    if(input$resource == "resources_island"){
    par0 <- getCCpars(mu_x0 = as.numeric(input$mu.x0), 
                      mu_t0 = as.numeric(input$mu.t0),
                      beta_x = as.numeric(input$beta.x),
                      beta_t = as.numeric(input$beta.t),
                      n.years = as.numeric(input$years),
                      sigma_x = as.numeric(input$x.sd),
                      sigma_t = as.numeric(input$t.sd))
    
    Resource.CC <- aaply(par0, 1, function(p) getPulsedResource_v2(world, p))
    world$resource <- Resource.CC}
    
    if(input$resource == "resources_drifting"){
      par0 <- getCCpars(mu_x0 = as.numeric(input$mu.x0), 
                        mu_t0 = as.numeric(input$mu.t0),
                        beta_x = as.numeric(input$beta.x),
                        beta_t = as.numeric(input$beta.t),
                        n.years = as.numeric(input$years),
                        sigma_x = as.numeric(input$x.sd),
                        sigma_t = as.numeric(input$t.sd))
      
      Resource.CC <- aaply(par0, 1, function(p) getPulsedResource(world, p))
      world$resource <- Resource.CC}
    
    parameters <- c(epsilon = as.numeric(input$epsilon), 
                       alpha = as.numeric(input$alphabeta) * as.numeric(input$kappa),
                       beta = as.numeric(input$alphabeta) * (1-as.numeric(input$kappa)),
                       gamma = as.numeric(input$gamma))
      
   runManyYears(World=get(input$world), parameters = parameters, 
                                  n.years = as.numeric(input$years), 
                threshold = as.numeric(input$threshold), verbose=FALSE)
    
  })
  output$Image <- renderPlot({
    plotManyRuns(simulationRun(), nrow=1)
  }, res = 150)
}

