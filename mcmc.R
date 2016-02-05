if(!require("rcppbugs")) {
  install.packages("rcppbugs")
  library(rcppbugs)
}

createModel<- function(parameters) {

  # params for q logreg model with a g-prior
  bg<- get.bg(a = parameters[["q.alpha"]],
                b = parameters[["q.beta"]])
  b.tau<- 1/diag(bg[["g"]]*NR*solve(t(X) %*% X))
  b.mu<- c(1,0,0)*bg[["b"]] 
  
  
  # total (N)

  N.mc <- mcmc.gamma(runif(1,min=Nmin,max=3000),
                     alpha = parameters[["N.shape"]], 
                     beta = parameters[["N.scale"]])
  N.d <- deterministic(function(N.mc, Nmin) {
    d(round(max(N.mc,Nmin)))
  }, N.mc, Nmin)
 
  # migration (p)
  p.mu<- mcmc.normal(runif(1, min = 1, max = 60), 
                      mu = parameters[["pmu.mu"]], 
                      tau = parameters[["pmu.tau"]])
  p.sd <- mcmc.gamma(runif(1, min = 10, max = 15),
                          alpha = parameters[["psd.shape"]], 
                          beta = parameters[["psd.scale"]])

  p.overdisp.tau <- mcmc.uniform(runif(1),0,100)
  p.overdisp <- mcmc.normal(rnorm(NR),0,p.overdisp.tau)
  
  p<- deterministic(function(p.mu,p.sd, day, p.overdisp) {
    p<- exp(-0.5*((day - p.mu + p.overdisp)/p.sd)^2)
    d(p/sum(p))
  }, p.mu, p.sd, day, p.overdisp)

  
  # catchability (q)
  q.overdisp.tau <- mcmc.uniform(runif(1),0,100)
  q.overdisp <- mcmc.normal(rnorm(NR),0, q.overdisp.tau)
  q.coefs <- mcmc.normal(b.mu, mu = b.mu, tau = b.tau)
  
  q <- deterministic(function(X, q.coefs, q.overdisp) {
  1/(1 + exp(- X %*% q.coefs + q.overdisp))
  }, X, q.coefs, q.overdisp)
  
  # daily smolts (n)  
  n.mc <- mcmc.binomial(d(rbinom(n = p, p = p, size = N.d)),
                        p = p, 
                        n = N.d)  
  n <-   deterministic(function(n.mc, dailymin) {
    d(round(mapply(max,n.mc,dailymin)))
  },n.mc, dailymin)
 
  # catches (x)
  x <- mcmc.binomial(d(catch),
                         p = q, n = n,
                         observed = TRUE)
  
  # debug
#   nodes<- list( N.mc = N.mc, N.d,
#                 p.overdisp.tau = p.overdisp.tau, 
#                 p.overdisp = p.overdisp, 
#                 p.mu = p.mu, p.sd = p.sd,
#                 p = p,
#                 q.overdisp.tau = q.overdisp.tau ,
#                 q.overdisp = q.overdisp,
#                 q.coefs = q.coefs,
#                 q = q,
#                 n.mc = n.mc, n = n,
#                 x = x)
#   sapply(nodes,function(node) {
#     print(c(node,logp(node)))})
  
  m <- create.model(N.mc, N.d,
                    p.overdisp.tau, p.overdisp, p.mu, p.sd, p,
                    q.overdisp.tau, q.overdisp, q.coefs, q,
                    n.mc, n,
                    x)
  m
}

runModel<- function(parameters, 
                    iter = 10000,
                    burn = 1000,
                    thin = 5,
                    adapt = 1000) {
  
  smolt <- createModel(parameters)
  
  results <- run.model(smolt, 
                     iterations = iter, 
                     burn = burn, 
                     thin = thin,
                     adapt = adapt)
  results
}