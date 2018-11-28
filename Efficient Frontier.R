library(timeSeries)
library(fPortfolio)
library(quantmod)

getSymbols(c('IBM', 'AMZN', "AAPL", "GOOG"), from="2006-01-01", to="2017-12-13")
AAPL = as.numeric(na.omit(Cl(AAPL)))
AMZN = as.numeric(na.omit(Cl(AMZN)))
GOOG = as.numeric(na.omit(Cl(GOOG)))
IBM = as.numeric(na.omit(Cl(IBM)))

assets = data.frame(AAPL, AMZN, GOOG, IBM)

return = log(tail(assets, -1) / head(assets, -1))
return <- log(tail(assets, -1) / head(assets, -1))
Q <- rbind(cov(return), rep(1, ncol(assets)), colMeans(return))
Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
mu <- 0.005
b <- c(rep(0, ncol(assets)), 1, mu)
solve(Q, b)

minvariance <- function( assets, mu = 0.005) { 
  return <- log( tail( assets, -1) / head( assets, -1)) 
  Q <- rbind( cov( return), rep( 1, ncol( assets)), colMeans( return)) 
  Q <- cbind( Q, rbind( t( tail( Q, 2)), matrix( 0, 2, 2))) 
  b <- c( rep( 0, ncol( assets)), 1, mu) 
  solve( Q, b) 
}

minvariance(assets)

frontier <- function(assets) {
  return <- log(tail(assets, -1) / head(assets, -1))
  Q  <- cov(return)
  n  <- ncol(assets)
  r  <- colMeans(return)
  Q1 <- rbind(Q, rep(1, n), r)
  Q1 <- cbind(Q1, rbind(t(tail(Q1, 2)), matrix(0, 2, 2)))
  rbase <- seq(min(r), max(r), length = 100)
  s  <- sapply(rbase, function(x) {
    y <- head(solve(Q1, c(rep(0, n), 1, x)), n)
    y %*% Q %*% y
  })
  plot(s, rbase, xlab='Target Return', ylab='Target Risk', main='Efficient Frontier', col='darkblue')
  grid()
#  plot(rbase, s, ylab='Return', xlab='Variance', main='Efficient Frontier')
}

frontier(assets)

getSymbols(c('IBM', 'AMZN', "AAPL", "GOOG"), from="2006-01-01", to="2017-12-13")
assets = 