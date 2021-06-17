

plotMigration <- function(M, add = FALSE){
  require(plyr)
  memory.df <- ldply(M, function(l)
    data.frame(time = 1:nrow(l), 
               memory = getMem(l, world = world)),.id = "year")
  
  if(!add) with(memory.df, plot(time, memory, type= "n"))
  n.years <- length(unique(memory.df$year))
  palette(rich.colors(n.years))
  ddply(memory.df, "year", function(df)
    with(df, lines(time, memory, col = as.integer(year))))
}

doublePlot <- function(M, world, par = FALSE){
  if(!par) 
    par(mfrow = c(1,2), mar = c(2,2,1,1), 
        tck = 0.01, mgp = c(1.5,.25,0), 
        bty = "l", cex.lab = 1.25, las = 1, xpd = NA)
  
  with(world, image(time, X, resource, col = grey.colors(100),
                    ylab = "space", xlab = "time of year"))
  plotMigration(M, add = TRUE)
  FE1 <- ldply(M, computeEfficiency, 
               resource = world$resource, world = world,
               .id = "year") %>% mutate(year = 1:length(year))
  plot(FE1, type = "o", ylab = "foraging efficiency")
}


plotSomeYears <- function(sim, world, years = NULL, nrow = 1, outer = TRUE, 
                          labelyears = FALSE, 
                          par = FALSE, ylab = "", ...){
  
  require(gplots)
  if(is.null(years)){
    years <- 1:length(sim)
    n <- length(years)
    zmax <- max(sapply(sim, max))
  } else{
    zmax <- max(sapply(sim, max)[paste0("Year",c(0,years[-length(years)]))])
    n <- length(years)
  }
  
  parameters <- attributes(sim)$parameter
  if(!par)
    par(mfrow = c(nrow,ceiling(n/nrow)), mar = c(1,0,1,0), tck = 0.01)
  
  for(i in years){
    image(1:world$tau, world$X, sim[[i]], 
          breaks = seq(0, zmax, length = 100), col = rich.colors(99),
          yaxt = "n", xaxt = "n", ylab = "", xlab = "")
    if(i == 1){ 
      axis(2, at = seq(-80,80,20))
      mtext(side = 2, line = 1.5, 
            "space", las = 0, cex = 1.25)
      title(side = 3, "year 0", line = 0.3)
    }
    if(i == round(max(years)/2))
      mtext(side = 1, line = 2, "time", cex = 1.25)
    
    if(labelyears & i > 1) title(substring(names(sim)[i],5), line = 0.3)
    if(i == 1) mtext(side = 2, ylab, ...)
    box(col = "white", bty = "o")
  }
  
  mtext(side = 1, at = (-(n-1):1)*100, text = (0:n)*100, 
        cex = 0.8, line = 0.25)
  
}

plotMigrationHat <- function(mhat, x.peak, t.peak, 
                             cols = c("darkorange", "darkblue"), par = FALSE, ylim1 = c(0,100)){
  if(!par) par(mfrow = c(1,2), mar = c(3,3,2,2), xpd = FALSE)
  with(mhat,{
    plot(year, t1, ylim = ylim1, ylab = "migration timing (day of year)", col = cols[1])
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