# Illustrating resources

require(memorymigration)

t.low <- 3
x.low <- 3
t.high <- 12
x.high <- 15

world <- getOptimalPop(tau = 100, x1 = 50, x2 = -50, 
                       t.peak = 25, t.sd = 12, x.sd = 5)
R1 <- getResource_island(world, 
                   c(t.peak = 25, t.sd = t.low, 
                     x.peak = 30, x.sd = x.high))
R2 <- getResource_island(world, 
                         c(t.peak = 25, t.sd = t.high, 
                           x.peak = 30, x.sd = x.low))

R3 <- getResource_drifting(world, 
                         c(t.peak = 25, t.sd = t.low, 
                           x.peak = 30, x.sd = x.high), x.null = 30)
R4 <- getResource_drifting(world, 
                         c(t.peak = 25, t.sd = t.high, 
                           x.peak = 30, x.sd = x.low), x.null = 30)
R.list <- list(R1, R2, R3,R4)
R.max <-  R.list%>% sapply(max) %>% max

b  <- seq(0, R.max, length = 101)
require(viridis)
cols <- rich.colors(100)
  
x <- world$X
t <- world$time

png("text/figures/ResourceExamples.png", width = 2000, height = 1200, res = 300)
{
  par(mfrow = c(2,2), mar = c(1,1,1,1), oma = c(2,2,1,1), 
      tck = 0.01, mgp = c(1,.25,0), las = 1, xpd = NA)
  
  for(i in 1:4){ 
    image(t,x,R.list[[i]], 
          col = cols, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    
    if(i == 1){
      axis(2, at = seq(-80,80,40))
      text(50, 80, col = "white", expression(sigma[t] == 3~";"~sigma[x]==12))
      mtext(side = 2, line = 1.5, "space", las = 0)
    }
    if(i == 2){
      text(50,80, col = "white", expression(sigma[t] == 12~";"~sigma[x]==3))
      mtext(side = 3, at = -10, "isolated resource", font = 2, line = .25)
    }
    
    if(i == 3){
      axis(2, at = seq(-80,80,40))
      mtext(side = 2, line = 1.5, "space", las = 0)
    }
    if(i %in% 3:4){
      axis(1,  at = seq(0,100,20))
      mtext(side = 1, "time", line = 1.25)
      if(i == 4)
        mtext(side = 3, at = -10, "weakly drifting resource", font = 2, line = .25)
      }
    }
}
dev.off()