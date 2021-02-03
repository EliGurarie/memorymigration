world <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=1, 
                    peak.max = 80, peak.min = 20, sd = 10)
par(mfrow = c(1,2))
plotYearList(list(world$pop))
plotYearList(list(world$pop), persp = TRUE)
