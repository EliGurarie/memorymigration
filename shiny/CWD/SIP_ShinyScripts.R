ui_SIP <- pageWithSidebar(
  
  headerPanel("SIP (Susceptible-Infected-Predation) model"),
  sidebarPanel(
    radioButtons("ImageType", "Image Type:", c("Time series" = "timeseries", "Wisconsin map" = "map")),
    numericInput("Tmax", "Duration of simulation:", value = 100, step = 10),
    numericInput("Tmax.map", "Map to year:", value = 100, step = 1),
    sliderInput(inputId = "alpha",
                label = "Selection preference",
                min = 0, max = 10, value = 1),
    sliderInput(inputId = "rho",
                label = "Adult recruitment",
                min = 0, max = 1, value = 0.4),
    sliderInput(inputId = "W_max",
                label = "Number of deer consumed / wolf / year",
                min = 10, max = 100, value = 70),
    sliderInput(inputId = "mu_S",
                label = "Mortality rate of non-CWD deer (1/year)",
                min = 0, max = 0.3, value = .06),
    sliderInput(inputId = "mu_I",
                label = "Mortality rate of CWD deer (1/year)",
                min = 0, max = 0.3, value = .12),
    sliderInput(inputId = "gamma",
                label = "rate of infection",
                min = 0, max = 0.1, value = .01),
    sliderInput(inputId = "lambda",
                label = "Dispersal kernel (km)",
                min = 0, max = 200, value = 40),
    radioButtons(inputId = "beta",
                label = "Dispersal type:", 
                choices = c("Gaussian" = "gaussian", "Exponential" = "exponential"), inline = TRUE)
  ),
  
  mainPanel(
    plotOutput("Image", height = "800px")
#    plotOutput(outputId = "timeSeries", height = "800px")
#    plotOutput(outputId = "finalWorldMap"),
  )
)


server_SIP <- function(input, output) {
    simulationRun <- reactive({
      run_SIP(WI_data, parameters = input, Tmax = input$Tmax, M.distance)
    })  
    output$Image <- renderPlot({
      simRun <- simulationRun()
      switch(input$ImageType, 
             timeseries = plotTimeSeries(simRun),
             map = mapResults(simRun, year = input$Tmax.map))
      #if(input$DrawMap) mapResults(simRun, year = input$Tmax.map)
      #else plotTimeSeries(simRun)
    }, res = 150)
    
#    output$timeSeries <- renderPlot({
#          simRun <- simulationRun()
#          plotTimeSeries(simRun)
#    }, res = 150)
  }
