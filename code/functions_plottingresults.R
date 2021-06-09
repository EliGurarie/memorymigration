plotMemory <- function(M, add = FALSE){
  require(plyr)
  memory.df <- ldply(M, function(l)
    data.frame(time = 1:nrow(l), 
               memory = getMem(l, world = world)),.id = "year")
  
  if(!add) with(memory.df, plot(time, memory, type= "n"))
  n.years <- length(unique(memory.df$year))
  palette(rich.colors(n.years))
  ddply(memory.df, "year", function(df)
    with(df, lines(time, memory, col = as.integer(year))))
  #legend("topright", legend = 1:n.years, col = 1:n.years, 
  #       lty = 1, ncol = 2, title = "year", bty = "n")
}

doublePlot <- function(M, world){
  par(mfrow = c(1,2), mar = c(2,2,1,1), 
      tck = 0.01, mgp = c(1.5,.25,0), 
      bty = "l", cex.lab = 1.25, las = 1, xpd = NA)
  if(min(dim(world$resource))<=50){
  with(world, image(time, X, resource[1,,], col = grey.colors(100)))
    plotMemory(M, add = TRUE)
    FE1 <- ldply(M, computeEfficiency, 
                 resource = world$resource[1,,], world = world,
                 .id = "year") %>% mutate(year = 1:length(year))
    plot(FE1, type = "o")
  }
  else{
    with(world, image(time, X, resource, col = grey.colors(100)))
    plotMemory(M, add = TRUE)
    FE1 <- ldply(M, computeEfficiency, 
                 resource = world$resource, world = world,
                 .id = "year") %>% mutate(year = 1:length(year))
    plot(FE1, type = "o")
  }
}


#' @export
plotEfficiency <- function(M, world, ...){
  FE1 <- ldply(M, computeEfficiency, 
               resource = world$resource[1,,], world = world,
               .id = "year") %>% mutate(year = 1:length(year))
  plot(FE1, type = "o", ...)
}

plotMigrationHat <- function(mhat, x.peak, t.peak, 
                             cols = c("darkorange", "darkblue")){
  par(mfrow = c(1,2), mar = c(3,3,2,2), xpd = FALSE); with(mhat,{
    plot(year, t1, ylim = c(0,100), ylab = "migration timing (day of year)", col = cols[1])
    segments(year, t1, year, t1+dt1, col = cols[1])
    points(year, t1 + dt1, col = cols[1])
    
    points(year, t2, col = cols[2])
    points(year, t2 + dt2, col = cols[2])
    segments(year, t2, year, t2+dt2, col = cols[2])
    abline(h = c(t.peak,100-t.peak), col =alpha("black",.3), lwd = 3, lty =3)
    
    plot(year, x1, type = "o", ylim = c(-100,100), ylab = "seasonal centroids", col = cols[1])
    lines(year, x2, type = "o", col = cols[2])
    abline(h = c(-x.peak,x.peak), col =alpha("black",.3), lwd = 3, lty =3)
    
    legend("topright", pch = c(1,1,NA), lty = c(1,1,3), 
           lwd = c(1,1,3), 
           legend = c( "summer", "winter", "true value"), col = c(cols, "darkgrey"), bty = "n")
  })
}








