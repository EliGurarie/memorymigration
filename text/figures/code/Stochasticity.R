require(magrittr)
require(plyr)
require(ggplot2)
require(ggthemes)

rm(list=ls())
load("results/trendstochasticity/trendstochasticity.rda")

cc.summary <- ccmusigma %>% subset(alpha == 100 & beta == 400 & beta_x == 0) %>% 
  ddply(c("lambda", "kappa", "beta_x", "psi_x"), 
        summarize, 
        n = sum(!is.na(avgTE)), 
        FE_mean = mean(avgTE, na.rm = TRUE), 
        FE_sd = sd(avgTE, na.rm = TRUE)) %>% 
  mutate(FE_se = FE_sd/sqrt(n),
         FE_high = FE_mean + 2*FE_se,
         FE_low = FE_mean - 2*FE_se)


cc.summary %>% subset(lambda == 80) %>% 
    ggplot(aes(kappa, FE_mean, col = factor(lambda), ymin = FE_low, ymax = FE_high)) +
    facet_grid(.~psi_x) + geom_point() + theme_few() + geom_errorbar() + 
    geom_path()



rm(list=ls())

load("results/trendstochasticity/trendstochasticity.rda")


cc.summary <- ccmusigma %>% subset(alpha == 100 & beta == 400 & lambda == 80) %>% 
  ddply(c("lambda", "kappa", "beta_x", "psi_x"), 
        summarize, 
        n = sum(!is.na(SAI_total)), 
        ST_mean = mean(SAI_total, na.rm = TRUE), 
        ST_sd = sd(SAI_total, na.rm = TRUE),
        SR_mean = mean(SAI_recent, na.rm = TRUE), 
        SR_sd = sd(SAI_recent, na.rm = TRUE)) %>% 
  mutate(ST_se = ST_sd/sqrt(n),
         ST_high = ST_mean + 2*ST_se, 
         ST_low = ST_mean - 2*ST_se,
         SR_se = SR_sd/sqrt(n),
         SR_high = SR_mean + 2*SR_se,
         SR_low = SR_mean - 2*SR_se)

head(cc.summary)

ylim <- range(cc.summary[,c("SR_high", "SR_low", "ST_high", "ST_low")], na.rm = TRUE)

cc.summary %>% subset(beta_x < 0) %>% 
  ggplot(aes(kappa, SR_mean, 
             ymin = SR_low, 
             ymax = SR_high,
             col = factor(beta_x))) +
  facet_grid(.~psi_x) + theme_few() + 
  geom_point() + geom_errorbar() + geom_hline(yintercept = c(0,1)) + 
  ggtitle("Adaptation - recent years") +
  geom_path()



cc.summary %>% subset(psi_x == 0)

df <- subset(cc.summary, psi_x == 3)
cols <-  c("red", "blue")


plotSR <- function(df, ...){
  plot(c(0,1), ylim, type = "n", xlab = "", ylab = "")
  abline(h = c(0,1), lty = c(1,3), lwd = 3, col = "grey", xpd = FALSE)
  with(df, {
    points(kappa, SR_mean, col = cols[factor(beta_x)], pch = 19, ...)
    segments(kappa, SR_low, kappa, SR_high, col = cols[factor(beta_x)])
    lines(kappa[beta_x == -.25], SR_mean[beta_x == -.25], col = cols[2])
    lines(kappa[beta_x == -.5], SR_mean[beta_x == -.5], col = cols[1])
  })
  box(bty = "l")
}

ylim <- range(cc.summary[,c("SR_high", "SR_low")], na.rm = TRUE)

png("text/figures/TrendStochasticity.png", width = 2400*2, height = 1000*2, res = 600)
{
  par(mfrow = c(1,4), mar = c(0,1,0,1), oma = c(4,4,4,4), tck = 0.01, mgp = c(1,.25,0), 
      bty = "l", yaxt = "n", xpd = NA)
  subset(cc.summary, psi_x == 0) %>% plotSR(cex = 1.5); axis(2, xpd = NA)
  mtext(side = 2, 
        at = seq(-.4,1.4,.2), 
        seq(-.4,1.4,.2), las = 1, line = .5, cex = 0.75)
  mtext(side = 2, 
        line = 2.5, las = 0, cex = 1.1,  
        "spatial adaptation index")
  mtext(expression(psi[x] == 0), side=3, line = 1)
    
  for(psi in c(3,6,12)){
    subset(cc.summary, psi_x == psi) %>% plotSR()
    mtext(bquote(psi[x] == .(psi)), side = 3, line = 1)
  }
  mtext(side = 1, outer = TRUE, expression("proportion reference memory"~(kappa)), line = 2.5, 
        cex = 1.1)
  
  legend("top", legend = c("slow (dx = 0.25)", "moderate (dx = 0.5)"), 
         pch = 19, lty = 1, col = cols[2:1], bty = "n")
}
dev.off()
