source("global.R")
library(MASS)

bg<- get.bg(a = 3, b = 50)
Sigma <- bg[["g"]]*NR*solve(t(X) %*% X)
mu<- c(1,0,0)*bg[["b"]] 

#one method with only variances (DOES NOT WORK!)
sds<- sqrt(diag(Sigma))
b <- mapply(sds, mu, FUN = function(sd, mui) {
  rnorm(1000, mean = mui, sd = sd)
  })

#multivariate version
b <- mvrnorm(1000, mu = mu, Sigma = Sigma)

#probs from logit transform
p <- apply(b,1,function(bi) {
  apply(X,1,function(xi) {
    exp(xi%*%bi)/(1+exp(xi%*%bi))
  })
})
p <- c(p)
plot(density(p))
