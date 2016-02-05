
# Priors

output$prior.N <- renderPlot({
  par(mar=c(5,1,4,2))
  plot(density(prior.N()$sample), 
       yaxt="n",
       xlab="smolts",
       main= "Total",
       col = "coral2",
       xlim=c(quantile(prior.N()$sample,c(0.001,0.999))))
})
output$prior.q<- renderPlot({
  par(mar=c(5,1,4,2))
  plot(density(prior.q()$sample),
       yaxt="n",
       xlab ="Probability",
       main ="Catchability",
       col = "cadetblue2",
       xlim= c(0,1))
})
output$prior.p<- renderPlot({
  par(mar=c(5,1,4,2))
  plot(prior.p(),
       yaxt='n',
       xlab="Day",
       main="Migration",
       col="seagreen2",
       xlim = c(0,nrow(data)))
})

# Projections

output$expect.n<-renderPlot({
  plot(prior.n(), 
       type = "h", 
       col = "steelblue4",
       main="Expected migrating smolts",
       ylab = "Smolts",
       xlab = "Day")
})
output$expect.x<-renderPlot({
  plot(prior.x(),
       type = "h",
       col = "darkseagreen3",
       main= "Expected catches",
       xlab = "Day",
       ylab = "Smolts")
})

# Results

output$traces <- renderPlot({
  plot(mc.object(), density = FALSE)
})

output$posterior.N<-renderPlot({
  plot(density(posterior.N()$sample),
       col = "coral2",
       main = "Posterior density of the total (N)",
       yaxt='n',
       xlab = "Smolts")
})
output$posterior.n<-renderPlot({
  plot(1:nrow(data),posterior.n(),
       type="h",
       col = "steelblue4",
       main="Posterior of migrating smolts",
       ylab = "Smolts",
       xlab = "Day")
})
output$posterior.q<-renderPlot({
  plot(1:nrow(data),posterior.q(), 
       type = "h",
       main = "Posterior catchability modelled with logistic regression",
       ylab = "Catchability",
       xlab = "day",
       ylim = c(0,0.5))
  
})
