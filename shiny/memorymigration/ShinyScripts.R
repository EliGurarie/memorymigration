library(shiny)
source("code/functions_v6.R")
source("code/functions_discretemigration.R")
source("code/functions_plottingresults.R")
ui <- fluidPage(
  
  h1("Memory Migration model"),
  fluidRow(
    column(3,
           h3("Migratory Population"),
           actionButton(inputId = "run", 
                        label = "Run Model"),
           downloadButton(outputId = "downloadData", 
                          label = "Download Results"),
           radioButtons(inputId = "world",
                        label = "Initial distribution of population in year 0", 
                        choices = c("Optimal" = "world_optimal", "Non-Migratory" = "world_nonmigratory", "Sinusoidal" = "world_sinusoidal"), inline = TRUE),
           numericInput(inputId = "years", label = "Duration of simulation:", value = 5, step = 1),
           numericInput(inputId = "threshold", label = "Threshold of Similarity", value = 0.999, min = 0, max = 1, step = 0.01),
           numericInput(inputId = "alpha",
                        label = "Resource Following - Alpha",
                        value = 100, min = 0, step = 1),
           numericInput(inputId = "beta",
                        label = "Spatial Scale of Sociality - Beta",
                        value = 100, min = 0, step = 1),
           sliderInput(inputId = "kappa",
                       label = "Memory Following - Kappa",
                       value = 1, min = 0, max = 1),
           numericInput(inputId = "lambda",
                        label = "Maximum Speed - Lambda",
                        value = 20, min = 0, step = 1),
           numericInput(inputId = "epsilon",
                        label = "Diffusion Parameter - Epsilon",
                        value = 1, min = 0, step = 1)
           ),
    column(6,
           h3("Migratory Population"),
           tableOutput("Indices"),
           plotOutput("Image", height = "400px"),
           plotOutput("Memory", height = "400px"),
          plotOutput("DoublePlot", height = "400px"),
           h3("Resource"),
           plotOutput("Resourceimage", height = "400px")
           ),
    column(3,
           h3("Resource"),
           actionButton(inputId = "viewresource", 
                        label = "View Resource"),
           sliderInput(inputId = "x.sd",
                       label = "Resource Space Distribution",
                       value = 5, min = 0, max = 15),
           sliderInput(inputId = "t.sd",
                       label = "Resource Time Distribution",
                       value = 12, min = 0, max = 15),
           numericInput(inputId = "mu.x0", label = "Initial Resource Position in Space", value = 40, step = 1),
           numericInput(inputId = "mu.t0", label = "Initial Resource Position in Time", value = 25, step = 1),
           numericInput(inputId = "beta.x", label = "Resource Change in Space", value = 0, step = 1),
           numericInput(inputId = "beta.t", label = "Resource Change in Time ", value = 0, step = 1),
           numericInput(inputId = "psi_x", label = "Stochasticity in Space", value = 0, step = 1),
           numericInput(inputId = "psi_t", label = "Stochasticity in Time ", value = 0, step = 1),
           radioButtons(inputId = "resource",
                        label = "Type of resource", 
                        choices = c("Island" = "resources_island", "Drifting" = "resources_drifting"), inline = TRUE)
    ),
           
    
))


server <- function(input, output) {
  pcks <- c("shiny","sf","ggplot2","magrittr","plyr", "gplots", 
            "memorymigration", "DT", "ggthemes", "minpack.lm", "fields","scales")
  lapply(pcks, require, character = TRUE)
  
  simulation <- eventReactive(input$run, {
      if(input$world == "world_optimal"){
        world <- getOptimalPop(tau=100, X.min = -100, X.max = 100, dx=1, 
                               x1 =as.numeric(input$mu.x0), 
                               x2 = -as.numeric(input$mu.x0),
                               t.peak=as.numeric(input$mu.t0), 
                               x.sd=as.numeric(input$x.sd), 
                               t.sd=as.numeric(input$t.sd))
      }
    
    if(input$world == "world_nonmigratory"){
      world <- getSinePop(tau = 100, peak.max = 1, peak.min = -1, sd = 10)
    }
    if(input$world == "world_sinusoidal"){
      world <- getSinePop(tau = 100, peak.max = 40, peak.min = -40, sd = 10)
    }
    world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))

        par0 <- getCCpars(mu_x0 = as.numeric(input$mu.x0), 
                          mu_t0 = as.numeric(input$mu.t0),
                          beta_x = as.numeric(input$beta.x),
                          beta_t = as.numeric(input$beta.t),
                          n.years = as.numeric(input$years),
                          sigma_x = as.numeric(input$x.sd),
                          sigma_t = as.numeric(input$t.sd),
                          psi_x = as.numeric(input$psi_x), 
                          psi_t = as.numeric(input$psi_t))
        if(input$resource == "resources_island"){ 
        Resource.CC <- aaply(par0, 1, function(p) getResource_island(world, p))
        world$resource <- Resource.CC
      }
      
      if(input$resource == "resources_drifting"){
        Resource.CC <- aaply(par0, 1, function(p) getResource_drifting(world, p))
        world$resource <- Resource.CC
      }
      
    resource_param <- data.frame(mu_x0 = as.numeric(input$mu.x0), 
                                 mu_t0 = as.numeric(input$mu.t0),
                                 beta_x = as.numeric(input$beta.x),
                                 beta_t = as.numeric(input$beta.t),
                                 n.years = as.numeric(input$years),
                                 sigma_x = as.numeric(input$x.sd),
                                 sigma_t = as.numeric(input$t.sd),
                                 psi_x = as.numeric(input$psi_x), 
                                 psi_t = as.numeric(input$psi_t),
                                 world = input$world,
                                 resource = input$resource) 
    
      parameters <- c(epsilon = as.numeric(input$epsilon), 
                      alpha = as.numeric(input$alpha),
                      beta = as.numeric(input$beta),
                      kappa = as.numeric(input$kappa),
                      lambda = as.numeric(input$lambda))
      
      param.df <- data.frame(
        epsilon = as.numeric(input$epsilon), 
        alpha = as.numeric(input$alpha),
        beta = as.numeric(input$beta),
        kappa = as.numeric(input$kappa),
        lambda = as.numeric(input$lambda)
      )

    sim <- runManyYears(world=world, parameters = parameters, 
                   n.years = as.numeric(input$years), 
                   threshold = as.numeric(input$threshold), FUN = runNextYear, verbose=TRUE)
  

    indices <- data.frame(computeIndices(sim$pop[[length(sim)]], 
                                         world$resource[length(sim)-1,,], world),
                          avgFE = computeAvgEfficiency(sim$pop, world$resource, world),
                          final_similarity = computeEfficiency(sim$pop[[length(sim)-1]], 
                                                               sim$pop[[length(sim)]], world), 
                          n.runs = length(sim$pop) - 1,
                          resource_param, param.df)
    
    #parameters.df <- ldply (parameters, data.frame)
    indices <- format(indices, digits=4)

 # memory <- doublePlot(sim$pop, world)

   yearplot <- plotManyRuns(sim$pop, world = world, nrow=ceiling(length(sim$pop)/6), labelyears=TRUE)

   
 # doubleplot <- plotMigrationHat(sim$m.hat, input$x.peak, input$t.peak, 
  #                                cols = c("darkorange", "darkblue"))

   # newlist <- list(sim,indices, memory, yearplot, doubleplot)
    newlist <- list(sim,indices, yearplot, world)
    
  })

  
  resourceImage <- eventReactive(input$run | input$viewresource,{
    if(input$world == "world_optimal"){
      world <- getOptimalPop(tau=100, X.min = -100, X.max = 100, dx=1, 
                             x1 =as.numeric(input$mu.x0), 
                             x2 = -as.numeric(input$mu.x0),
                             t.peak=as.numeric(input$mu.t0), 
                             x.sd=as.numeric(input$x.sd), 
                             t.sd=as.numeric(input$t.sd))
    }
    
    if(input$world == "world_nonmigratory"){
      world <- getSinePop(tau = 100, peak.max = 1, peak.min = -1, sd = 10)
    }
    if(input$world == "world_sinusoidal"){
      world <- getSinePop(tau = 100, peak.max = 40, peak.min = -40, sd = 10)
    }
    world$m0 <- fitMigration(t = world$time, x = getMem(world$pop, world))
    par0 <- getCCpars(mu_x0 = as.numeric(input$mu.x0), 
                      mu_t0 = as.numeric(input$mu.t0),
                      beta_x = as.numeric(input$beta.x),
                      beta_t = as.numeric(input$beta.t),
                      n.years = as.numeric(input$years),
                      sigma_x = as.numeric(input$x.sd),
                      sigma_t = as.numeric(input$t.sd),
                      psi_x = as.numeric(input$psi_x), 
                      psi_t = as.numeric(input$psi_t))
    if(input$resource == "resources_island"){
      
      Resource.CC <- aaply(par0, 1, function(p) getResource_island(world, p))
    }
    
    if(input$resource == "resources_drifting"){
      Resource.CC <- aaply(par0, 1, function(p) getResource_drifting(world, p))
    }
    
    par(mfrow = c(ceiling(min(dim(Resource.CC))/5), 5), mar = c(0,0,1,0), oma = c(2,2,0,2), tck = 0.01)
    for (i in 1:min(dim(Resource.CC))) image(Resource.CC[i,,], main = paste("year", i-1), yaxt = "n", xaxt = "n")
    
  })
  
 output$Image <- renderPlot({
   simulation()[[3]]
  }, res = 150)
  
  output$Resourceimage <- renderPlot({
    resourceImage()
  }, res = 150)
  
 output$Indices <- renderTable({
   simulation()[[2]][,1:6]
 }, digits = 3)
 
output$Memory <- renderPlot({
 doublePlot(simulation()[[1]]$pop,simulation()[[4]])
 }, res = 150)
 
#output$DoublePlot <- renderPlot({
 #  simulation()[[]]
#}, res = 150)
 
 output$downloadData <- downloadHandler(
   filename = "simulationRun.csv",
   content = function(file) {
     
     write.csv(simulation()[[2]], file)
   }
 )
}




  

  

