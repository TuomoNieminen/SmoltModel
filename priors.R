
# (N)
total <- function(mu,sd) {
  scale <- sd^2 /mu
  shape <- mu^2 / sd^2

  sample<- rgamma(2000, 
                 scale = scale, 
                 shape = shape)
  list(sample=sample, 
       scale = scale,
       shape = shape,
       mu = mu,
       sd = sd)
}

# (p)
migration<-function(mu,sd,day=1:nrow(data)) {
  p<- exp(-0.5*((day-mu)/sd)^2)
  p/sum(p)
}

# (q)
catchability<-function(alpha,beta) {
  sample<- rbeta(2000, 
                 shape1 = alpha, 
                 shape2 = beta)
  expect<- alpha/(alpha+beta)
  return(list(sample = sample, expect = expect))
}

# (n)
daily<- function(p,N) {
  sapply(p, function(pi) {
    sum(rep(pi,N))
  })
}

# (x)
catch<-function(En, Eq) {
  En*Eq
}

# (N)
prior.N<-reactive({
  mu<-ifelse(is.na(input$N.mu),1,input$N.mu)
  sd<-ifelse(is.na(input$N.sd),1,input$N.sd)
  total(mu = mu, sd = sd)
})
prior.p<-reactive({
  migration(mu = input$p.mu,
            sd = input$p.sd)
})
prior.q<-reactive({
  catchability(alpha = input$q.alpha,
               beta = input$q.beta)
})
prior.n<-reactive({
  daily(p = prior.p(),
        N = input$N.mu)
})
prior.x<-reactive({
  catch(En = prior.n(),
        Eq = prior.q()$expect)
})