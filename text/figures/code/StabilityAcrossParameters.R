plotBlock <- function(block, cols = rich.colors(6)[2:5], side1 = FALSE, side2 = FALSE){
  require(reshape2)
  b <- block[,c("TE", "alpha", "beta", "sigma_t", "sigma_x")]
  ns <- apply(b[,-1], 2, function(x) length(unique(x)))
  
  M <-   b %>% acast(alpha + sigma_t ~ beta +  sigma_x, 
                     value.var = "TE")
  
  #M[is.na(M)] <- max(M, na.rm = TRUE)
  image(1:ncol(M)-.5, 1:nrow(M)-.5, M, breaks = c(0,1,5,50,1000), 
        col = cols, xaxt ="n", xlab = "", ylab = "", yaxt = "n")
  abline(v = 1:ncol(M), col = "darkgrey")
  abline(h = 1:nrow(M), col = "darkgrey")
  abline(v = (0:ns["alpha"]) * ns["sigma_t"], lwd = 2)
  abline(h = (0:ns["beta"]) * ns["sigma_x"], lwd = 2)
  
  if(side1)
    text(1:ns["alpha"] * ns["sigma_t"] - ns["sigma_t"]/2, -1, 
         unique(b$alpha), xpd = NA)
  
  if(side2)
    text(nrow(M)+ 1, 
         1:ns["beta"] * ns["sigma_x"] - ns["sigma_x"]/2,
         unique(b$beta), xpd = NA, srt = 270)
}

plotMiniBlock <- function(cols = rich.colors(6)[2:5]){
  miniM <- subset(df, 
                  lambda == 100 & epsilon == 1 & 
                    beta == 400 & alpha == 200)[,c("sigma_t", "sigma_x", "TE")] %>% 
    acast(sigma_t ~ sigma_x)
  
  sigs <- seq(3,15,3)
  image(sigs - 1.5, sigs - 1.5, miniM, breaks = c(0,1,5,50,1000), 
        col = cols, xaxt ="n", xlab = "", ylab = "", yaxt = "n", asp = 1)
  mtext(side = 1, at = sigs-1.5, sigs, line = .5, cex = 1.15)
  mtext(side = 2, at = sigs-1.5, sigs, line = .5, cex = 1.15)
  abline(v = sigs, col = "darkgrey")
  abline(h = sigs, col = "darkgrey")
  mtext(side = 1, expression("resource duration "~(sigma[t])), line = 3, cex = 1.1)
  mtext(side = 2, expression("resource extent "~(sigma[x])), line = 2, cex = 1.1)
}

plotStability <- function(stability,  cols = rich.colors(6)[2:5]){
  layout(rbind(1:3,4:6) %>% cbind(7:8))
  par( mar = c(1,1,1,1)/2, oma = c(3,3,3,3), xpd = FALSE)
  for(e in c(1,8)) for(l in c(20,50,100)){
    myblock <- subset(stability, epsilon == e & lambda == l) 
    if(e == 1){
      if(l == 20){
        plotBlock(myblock, cols = cols)
        mtext(side = 3, expression("tight swarm "~(lambda == 20)), line = 1, cex = 1.25)
        mtext(side = 2, expression("low diffusion "~(epsilon == 1)), line = 1, cex = 1.25)
      }
      if(l == 50){
        plotBlock(myblock, cols = cols)
        mtext(side = 3, expression("medium swarm"~(lambda == 50)), line = 1, cex = 1.25)
      }
      if(l == 100){
        plotBlock(myblock, side2 = TRUE, cols = cols)
        mtext(side = 3, expression("loose swarm"~(lambda == 100)), line = 1, cex = 1.25)
      }} 
    else{ 
      if(l == 20){  
        plotBlock(myblock, side1 = TRUE, cols = cols)
        mtext(side = 2, expression("high diffusion "~(epsilon == 8)), line = 1, cex = 1.25)
      }
      if(l == 50){ 
        plotBlock(myblock, side1 = TRUE, cols = cols)
        mtext(side = 1, expression("resource following "~(alpha)), line = 2, cex = 1.25)
      }
      if(l == 100){  
        plotBlock(myblock, side1 = TRUE, side2 = TRUE, cols = cols)
      }
    }
  }
  b <- myblock[,c("TE", "alpha", "beta", "sigma_t", "sigma_x")]
  ns <- apply(b[,-1], 2, function(x) length(unique(x)))
  
  text(ns["sigma_t"] * ns["alpha"]  + 3, 
       ns["sigma_x"] * ns["beta"], srt = 270, xpd = NA, 
       cex = 2, 
       expression("social attraction " ~(beta)))
  
  par(mar = c(4,8,4,0))  
  plot.new()
  legend("center", fill = cols, legend = c("< 1", "1-5", "5-50", "> 50"), cex = 2, 
         title = "total mismatch")
  plotMiniBlock(cols = cols)
}


load("results/stability/stability_compiled.rda")
png("text/figures/StabilityResults.png", width = 3800, height = 2000, res = 300)
plotStability(stability, cols = c("darkblue", "purple", "orange", "lightgrey"))
dev.off()


df <- stability %>% mutate(mismatch = cut(TE, c(0,1,5,50,300)))

ggplot(df %>% subset(alpha > 0), aes(TE, FE)) + geom_point(aes(col = factor(alpha))) + geom_smooth() + 
  facet_grid(epsilon~sigma_t) + scale_x_continuous(trans='log10') + 
  theme_few()


png("text/figures/ForagingEfficiency.png", width = 2400, height = 900, res = 300)
cols <- c("darkblue", "purple", "orange", "lightgrey")
par(mfrow=c(1,5), mar = c(0,.5,0,.5), oma = c(2,3,4,2), bty = "l", tck = 0.02, mgp = c(1,.25,0), xpd = NA)
for(s in seq(3,15,3)){
  boxplot(FE~mismatch, data = subset(df, sigma_t == s), 
          varwidth = TRUE, yaxt = "n", xaxt = "n", xlab = "", ylab = "", ylim = c(0,1),
          col = cols, lty = 1, border = grey(.4), pch = 21, bg = cols)
  mtext(side = 3, bquote(sigma[t] == .(s)), line = 1, cex = 1.1)
  # mtext(side = 1, at = 1:4, c("< 1", "1-5", "5-50", ">50"), line = 0.5)
  if(s == 3){ 
    axis(2, las = 2)
    mtext(side = 2, "foraging efficiency", line = 1.75)
    legend("top", fill = cols, legend = c("< 1", "1-5", "5-50", "> 50"), 
           ncol = 2, cex = 1.3, xpd = NA, 
           title = "total mismatch", bty = "n")
  }
}
dev.off()

