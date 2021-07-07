
plotCCspacepanel <- function(df, cols = c("darkblue", "darkorange")){
  with(df, {
    plot(lambda, SAI_recent, type = "n", ylab = "", xlab = "", 
         ylim = c(-2.3,2), xlim = c(0,200), xaxt = "n", yaxt = "n")
    abline(h = c(0,1), lty = 1:2, col = "grey", lwd = 2)
    points(lambda[alpha == 100], SAI_recent[alpha == 100], 
           col = cols[1], bg = alpha(cols[1], .4),
           type = "o", pch = 21, cex = FE[alpha == 100]*4)
    points(lambda[alpha == 400], SAI_recent[alpha == 400], 
           col = cols[2], bg = alpha(cols[2], .4), 
           type = "o", pch = 21, cex = FE[alpha == 400]*4)
  })
}

load("results/climate_space/ccspace.rda")

boplotFE <- function(){
  cols <- c("grey", "orange", "purple", "darkblue")
  par(mar = c(0,0,2,0), bty = "l", tck = 0.02, 
      mgp = c(1,.25,0), las = 1)
  with(ccspace %>% subset(alpha == 100), {
    boxplot(avgFE ~ cut(SAI_recent, c(-1,0,.5,1,10)), varwidth = TRUE, 
            border = cols, col = alpha(cols, .5), 
            at = 1:4, xaxt = "n", ylim = c(0,0.8), xlim = c(.5,8))
  })
  
  with(ccspace %>% subset(alpha == 400), {
    boxplot(avgFE ~ cut(SAI_recent, c(-1,0,.5,1)), varwidth = TRUE, 
            border = cols, col = alpha(cols, .5), 
            at = 5:7 + 0.5, add = TRUE, xaxt = "n")
    
  })
  
  mtext(side = 3, at = 2.5, expression(alpha == 100), cex = 1.25)
  mtext(side = 3, at = 6.5, expression(alpha == 400), cex = 1.25)
  
  axis(1, at = c(1:4, 5.5:7.5), c("< 0", "0 - 0.5", "0.5 - 1", "> 1", "< 0", "0 - 0.5", "0.5 - 1"))
  abline(v = 5)
  
  mtext(side = 1, line = 1.5, "spatial adaptation index", cex = 1.25, outer = TRUE)
  mtext(side = 2, line = 2, "foraging efficiency", outer = TRUE, xpd= NA, las = 0, cex = 1.25)
}

png("text/figures/SpatialClimateChange.png", width = 3000, height = 2400, res = 300)
{
  layout(cbind(matrix(1:6, ncol = 2, byrow = TRUE), 7:9), widths = c(1,1,1.5))
  par(bty = "l", mar = c(1,1,1,1)/2, las = 1, tck = 0.02, mgp = c(1,0.25,0),
      oma = c(4,3,3,3), cex.axis = 1.25)
  
  rightlabel <- function(text) text(220, 0, srt = 270, text, xpd = NA, cex = 1.5)
  
  for(dx in c(-.25,-.5,-1)) for(b in c(100,400)){
    plotCCspacepanel( subset(ccspace, beta == b & beta_x == dx))
    if(dx == -.25){
      mtext(side = 3, bquote(beta == .(b)), cex = 1.2)
    }
    if(b == 400){
      if(dx == -.25) rightlabel("slow (dx = 0.25)")
      if(dx == -.5) rightlabel("medium (dx = 0.5)")
      if(dx == -1) rightlabel("rapid (dx = 1)")
    }
    if(b == 100){
      axis(2)
      if(dx == -.5) mtext(side = 2, line = 1.75, "spatial adaptation index", 
                          xpd = NA, las = 0, cex = 1.25)
    } 
    if(dx == -1){
      axis(1)
      if(b == 400) mtext(side = 1, at = -5, expression("swarm size"~lambda), 
                         line = 1.75, cex = 1.25)
    }}
  
  par(mar = c(0,6,2,4), xpd = NA)
  plot.new()
  legend("top", bty= "n", col = c("darkblue", "darkorange"), 
         title = expression("resource\nfollowing"~(alpha)), 
         pch = 19, cex = 1.55, legend = c(100,400))
  
  legend("topright", bty= "n", pt.cex = (1:5)/5*4, title = "foraging\nefficiency", 
         pch = 19, cex = 1.55, legend = (1:5)/5, col = "grey")

  cols <- c("darkblue", "darkorange")
  
  par(mar = c(.5,8,2,0), bty = "l", tck = 0.02, 
      mgp = c(1,.25,0), las = 1, xpd = FALSE)
  with(ccspace %>% subset(alpha == 100), {
    boxplot(FE ~ cut(SAI_recent, c(-1,0,.5,1,10)), varwidth = TRUE, 
            border = cols[1], col = alpha(cols[1], .5),  ylab = "",
            at = 1:4, xaxt = "n", ylim = c(0.2,0.8), xlim = c(.5,4.5))
  })
  mtext(side = 2, line = 2.25, "final foraging efficiency", xpd= NA, las = 0, cex = 1.25, at = .1)
  title(expression(alpha == 100), cex.main = 2)
  
  with(ccspace %>% subset(alpha == 400), {
    boxplot(FE ~ cut(SAI_recent, c(-1,0,.5,1,10)), varwidth = TRUE, 
            border = cols[2], col = alpha(cols[2], .5), ylab = "",
            at = 1:4, xaxt = "n", ylim = c(0.2,0.8), xlim = c(.5,4.5))
  })
  title(expression(alpha == 400), cex.main = 2)
  axis(1, at = 1:4, c("< 0", "0 - 0.5", "0.5 - 1", "> 1"))
  mtext(side = 1, line = 1.75, "spatial adaptation index", cex = 1.25)
}
dev.off()