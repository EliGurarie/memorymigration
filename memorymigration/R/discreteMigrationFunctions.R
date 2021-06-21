#' Discrete migration functions
#'
#' Functions to fit the discrete step-wise migration process
#'
#'@export

stepMigration <- function(t, t1, dt1, t2, dt2, x1, x2, tau = 100){
  s12 <- (x2-x1)/(t2 - t1 - dt1)
  s21 <- (x1-x2)/(t1 - (t2 - tau) - dt2)
  ifelse((t > t1) & (t <= t1 + dt1), x1, 
         ifelse((t > t1 + dt1) & (t <= t2), s12*(t - t1 - dt1) + x1, 
                ifelse(t > t2 & t <= t2 + dt2, x2, 
                       ifelse(t > t2+dt2, s21*(t - t2 - dt2)+ x2,
                              s21*(t - t2 - dt2 + tau) + x2))))
}

#' Fit Migration
#' 
#'@export
fitMigration <- function(t, x, m.start = NULL, tau = 100){
  if(is.null(m.start)) m.start <- c(t1 = 15, dt1 = 30, t2 = 55, dt2 = 30, x1 = min(x), x2 = max(x))
  migration.fit <- nlsLM(x ~ stepMigration(t, t1, dt1, t2, dt2, x1, x2, tau = 100), 
                         start = as.list(m.start),
                         lower = c(-100,-50,0,0,-100,-100), 
                         upper = c(100,100,200,100,100,100)) 
  summary(migration.fit)$coef[,1]
}

#'@export
squashMigration <- function(m.hat, v.max, tau = 100){
  p.list <- as.list(m.hat)
  s12 <- with(p.list, (x2-x1)/(t2 - t1 - dt1))
  s21 <- with(p.list, (x1-x2)/(t1 - (t2 - tau) - dt2))
  m.new <- m.hat
  if(abs(s12) > v.max){
    if(s12 < 0) v.max <- -v.max 
    dx.migration <- with(p.list, x2 - x1)
    t.migration <- with(p.list, t2 - (t1+dt1))
    t.migration.new <- dx.migration/v.max
    dt.migration <- t.migration.new - t.migration
    m.new["dt1"] <- m.new["dt1"] - dt.migration/2
    m.new["t2"] <- m.new["t2"] + dt.migration/2
    m.new["dt2"] <- m.new["dt2"] - dt.migration/2
  }
  if(abs(s21) > v.max){
    if(s21 < 0) v.max <- -v.max
    dx.migration <- with(p.list, x1 - x2)
    t.migration <- with(p.list, t1 - (t2 - tau + dt2))
    t.migration.new <- dx.migration/v.max 
    dt.migration <- t.migration.new - t.migration
    m.new["t1"] <- m.new["t1"] + dt.migration/2
    m.new["dt1"] <- m.new["dt1"] - dt.migration/2
    m.new["dt2"] <- m.new["dt2"] - dt.migration/2
  }
  m.new
}
