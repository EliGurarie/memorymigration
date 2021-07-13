require(magrittr)
require(plyr)
require(ggplot2)
require(ggthemes)

rm(list=ls())


  
  
  b <- load("results/trendstochasticity/betax0.rda")
  
  cc.summary <- ccsigma %>% 
    ddply(c("lambda", "kappa", "beta_x", "psi_x"), 
          summarize, 
          n = sum(!is.na(avgFE)), 
          FE_mean = mean(avgFE, na.rm = TRUE), 
          FE_sd = sd(avgFE, na.rm = TRUE)) %>% 
    mutate(FE_se = FE_sd/sqrt(n),
           FE_high = FE_mean + 2*FE_se,
           FE_low = FE_mean - 2*FE_se)
  cc.summary %>% subset(lambda > 40) %>% 
      ggplot(aes(kappa, FE_mean, col = factor(lambda), ymin = FE_low, ymax = FE_high)) +
      facet_grid(beta_x~psi_x) + geom_point() + theme_few() + geom_errorbar() + 
      geom_path()
  
  head(cc.summary)
  
  png("text/figures/stochasticity.png", width = 2800*2, height = 1000*2, res = 600)
  ccsigma %>% subset(lambda > 40) %>% 
    ggplot(aes(kappa, avgFE, col = factor(lambda))) +
    facet_grid(beta_x~psi_x) + geom_smooth() + theme_few() + 
    ylab("average foraging efficiency") + 
    xlab(expression("proportion reference memory"~(kappa))) + 
    guides(col=guide_legend(title=expression("spatial scale "~(lambda))))
  dev.off()
  
  with(cc.summary, table(lambda, n))

  
  plotFE <- function(df, ...){
    plot(c(0,1), c(.6,.70), type = "n", xlab = "", ylab = "")
    abline(h = c(0,1), lty = c(1,3), lwd = 3, col = "grey", xpd = FALSE)
    with(df, {
      points(kappa, FE_mean, col = cols[factor(lambda)], pch = 19, ...)
      segments(kappa, FE_low, kappa, FE_high, col = cols[factor(lambda)])
      lines(kappa[lambda == 80], FE_mean[lambda == 80], col = cols[1])
      lines(kappa[lambda == 120], FE_mean[lambda == 120], col = cols[2])
    })
    box(bty = "l")
  }
  
  df <- subset(ccsigma, psi_x == 12)
  ylim <- c(0.6, 0.72)
  cols <- c("darkblue", "darkred")
    
  
  names(df)
  
  plotFE <- function(df, ...){
    kappa <-  seq(0,1,.01)
    
    loess80 <- loess(avgFE~kappa, data = subset(df,lambda == 80))
    p80 <- predict(loess80, se = TRUE, newdata = data.frame(kappa = kappa))
    loess120 <- loess(avgFE~kappa, data = df %>% subset(lambda == 120))
    p120 <- predict(loess120, se = TRUE, newdata = data.frame(kappa = kappa))
    
    predict.df <- data.frame(
      with(p80, data.frame(kappa, fit80 = fit, low80 = fit - 2*se.fit, high80 = fit + 2*se.fit)),
      with(p120, data.frame(fit120 = fit, low120 = fit - 2*se.fit, high120 = fit + 2*se.fit)))
    
    
    n <- nrow(predict.df)
    plot(c(0,1), ylim, type = "n", xlab = "", ylab = "")
    with(predict.df,{
        lines(kappa, fit80, col = cols[1], lwd = 2)
        polygon(c(kappa, kappa[n:1]), c(low80, high80[n:1]), col = alpha(cols[1], .2), 
                border = NA)
        
        lines(kappa, fit120, ylim = ylim, type = "l", col = cols[2], lwd = 2)
        polygon(c(kappa, kappa[n:1]), c(low120, high120[n:1]), col = alpha(cols[2], .2), 
                border = NA)
        box(bty = "l")
    })
    with(ddply(df, c("kappa", "lambda"), summarize, FE = mean(avgFE)),
         points(kappa, FE, col = cols[as.integer(factor(lambda))], cex= 1.25, lwd = 1.25,
                pch = 21, bg = 0))
  }
  
  # plotFE <- function(df, ...){
  #   plot(c(0,1), c(.6,.70), type = "n", xlab = "", ylab = "")
  #   abline(h = c(0,1), lty = c(1,3), lwd = 3, col = "grey", xpd = FALSE)
  #   with(df, {
  #     points(kappa, FE_mean, col = cols[factor(lambda)], pch = 19, ...)
  #     segments(kappa, FE_low, kappa, FE_high, col = cols[factor(lambda)])
  #     lines(kappa[lambda == 80], FE_mean[lambda == 80], col = cols[1])
  #     lines(kappa[lambda == 120], FE_mean[lambda == 120], col = cols[2])
  #   })
  #   box(bty = "l")
  # }
  
  
  png("text/figures/stochasticity.png", width = 2400*2, height = 1000*2, res = 600)
  {
    cols <-  c("red", "blue")
    par(mfrow = c(1,5), mar = c(0,1,0,.5), oma = c(4,4,4,1), tck = 0.01, mgp = c(1,.25,0), 
        bty = "l", xpd = NA, las = 1)
    subset(ccsigma, psi_x == 0) %>% plotFE(cex = 1.25); 
    par(yaxt = "n")
    mtext(side = 2, 
          line = 2.5, las = 0, cex = 1.1,  
          "foraging efficiency")
    mtext(expression(psi[x] == 0), side=3, line = 1)
    axis(1, at = seq(0,1,.2), gap.axis = -1)
    for(psi in c(3,6,9,12)){
      subset(ccsigma, psi_x == psi) %>% plotFE(cex = 1.25)
      axis(1, at = seq(0,1,.2), gap.axis = -1)
      mtext(bquote(psi[x] == .(psi)), side = 3, line = 1)
    }
    mtext(side = 1, outer = TRUE, expression("proportion reference memory"~(kappa)), line = 2.5, 
          cex = 1.1)
    
    legend("top", legend = c("80", "120"), title = expression("spatial scale"~(lambda)),
           pch = 19, lty = 1, box.col = "white", bg = "white",  col = cols, cex = 1.25)
  }
  dev.off()
  



rm(list=ls())

load("results/trendstochasticity/trendstochasticity.rda")


cc.summary <- ccmusigma %>% subset(alpha == 100 & beta == 400) %>% 
  ddply(c("lambda", "kappa", "beta_x", "psi_x"), 
        summarize, 
        n = sum(!is.na(SAI_total)), 
        ST_mean = mean(SAI_total, na.rm = TRUE), 
        ST_sd = sd(SAI_total, na.rm = TRUE),
        SR_mean = mean(SAI_recent, na.rm = TRUE), 
        SR_sd = sd(SAI_recent, na.rm = TRUE),
        FE_mean = mean(avgFE, na.rm = TRUE),
        FE_sd = sd(avgFE, na.rm = TRUE)) %>% 
  mutate(ST_se = ST_sd/sqrt(n),
         ST_high = ST_mean + 2*ST_sd/sqrt(n), 
         ST_low = ST_mean - 2*ST_sd/sqrt(n),
         SR_se = SR_sd/sqrt(n),
         SR_high = SR_mean + 2*SR_sd/sqrt(n),
         SR_low = SR_mean - 2*SR_sd/sqrt(n),
         FE_se = FE_sd/sqrt(n),
         FE_high = FE_mean + 2*FE_sd/sqrt(n),
         FE_low = FE_mean - 2*FE_sd/sqrt(n))

head(cc.summary)

ylim <- range(cc.summary[,c("SR_high", "SR_low", "ST_high", "ST_low")], na.rm = TRUE)

cc.summary %>%  subset(beta_x < 0) %>% 
  ggplot(aes(kappa, SR_mean, 
             ymin = SR_low, 
             ymax = SR_high,
             col = factor(lambda))) +
  facet_grid(beta_x~psi_x) + theme_few() + 
  geom_point() + geom_errorbar() + geom_hline(yintercept = c(0,1)) + 
  ggtitle("Adaptation - recent years") +
  geom_path()


cc.summary %>%  subset(beta_x == -.5) %>% 
  ggplot(aes(kappa, FE_mean, 
             ymin = FE_low, 
             ymax = FE_high,
             col = factor(lambda))) +
  facet_grid(beta_x~psi_x) + theme_few() + 
  geom_point() + geom_errorbar() +  
  ggtitle("Adaptation - recent years") +
  geom_path()


plotSR <- function(df, ...){
  plot(c(0,1), ylim, type = "n", xlab = "", ylab = "")
  abline(h = c(0,1), lty = c(1,3), lwd = 3, col = "grey", xpd = FALSE)
  with(df, {
    segments(kappa, SR_low, kappa, SR_high, col = cols[factor(lambda)])
    lines(kappa[lambda == 80], SR_mean[lambda == 80], col = cols[1])
    lines(kappa[lambda == 120], SR_mean[lambda == 120], col = cols[2])
    points(kappa, SR_mean, col = cols[factor(lambda)], pch = 21, 
           bg = cols[factor(lambda)] %>% alpha(.3),  lwd = 1.1,  ...)
  })
  box(bty = "l")
}

cc.plot <- cc.summary %>% subset(beta_x == -.5)
cols <- c("red", "blue")
png("text/figures/TrendStochasticity.png", width = 2400*2, height = 1000*2, res = 600)
{
  par(mfrow = c(1,5), mar = c(0,1,0,1), oma = c(4,4,4,1), tck = 0.01, mgp = c(1,.25,0), 
      bty = "l", xpd = NA, las = 1)
  subset(cc.plot, psi_x == 0) %>% plotSR(cex = 1.25); 
  par(yaxt = "n")
  mtext(side = 2, 
        line = 2.5, las = 0, cex = 1.1,  
        "spatial adaptation index")
  mtext(expression(psi[x] == 0), side=3, line = 1)
  
  
  for(psi in c(3,6,9,12)){
    subset(cc.plot, psi_x == psi) %>% plotSR(cex = 1.25)
    mtext(bquote(psi[x] == .(psi)), side = 3, line = 1)
  }
  mtext(side = 1, outer = TRUE, expression("proportion reference memory"~(kappa)), line = 2.5, 
        cex = 1.1)
  
  legend("top", legend = c("80", "120"), title = expression("spatial scale"~(lambda)),
         pch = 21, lty = 1, box.col = "white", bg = "white",  col = cols, cex = 1.25, 
         pt.bg = alpha(cols, .3))
}
dev.off()

