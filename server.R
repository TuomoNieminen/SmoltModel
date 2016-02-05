if(!require("coda")) {
  install.packages("coda")
  library(coda)
}
library(shiny)

source("global.R")
source("mcmc.R")

shinyServer(function(input, output) {
  
source("priors.R", local = TRUE)
source("posteriors.R", local = TRUE)
source("plots.R",local=TRUE)

priors<-reactiveValues()
results<- eventReactive(input$run, {
  
  starttime<- Sys.time()
    
    priors$N.scale <- prior.N()$scale
    priors$N.shape <- prior.N()$shape
    priors$pmu.mu <- d(input$p.mu)
    priors$pmu.tau <- d(1/5^2)
    priors$psd.shape <- 5
    priors$psd.scale <- d(input$p.sd/5)         
    priors$q.alpha <- d(input$q.alpha)
    priors$q.beta <- d(input$q.beta)
    priors$iter <- input$iter
    priors$thin <- input$thin    
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Running the mcmc simulation", 
                 value = 1)
    
    results<-  runModel(parameters = priors,
                           iter=input$iter,
                           burn=input$burn,
                           thin = input$thin)   
  priors$runtime <- difftime(Sys.time(),starttime)
    results
})

output$runtime<- renderText({
  seconds <- 5 + (input$iter + input$adapt)/1100
  min <- paste(floor(seconds/60), " minutes")
  sec <- ifelse(seconds < 5*60, 
                    paste(" and ", 
                          floor(seconds) %% 60, 
                          " seconds."), 
                    ".")
  paste("Estimated runtime is ", min, sec, sep ="")
})

output$summary.stats<-renderTable({
  summary(mc.object())$statistics
})
output$summary.quantiles<-renderTable({
  summary(mc.object())$quantiles
})
output$summary.text <- renderText({
  mins <- floor(as.numeric(priors$runtime, units="mins"))
  secs <- floor(as.numeric(priors$runtime, units="secs")) - 60*mins
  paste("The run lasted ", 
        mins, " minutes and ",
        secs, " seconds. ",
        "The results are for ",
        priors$iter, " iterations with a 
        thinning of ",priors$thin,". ",
        "The acceptance rate of the simulation was ",
        round(get.ar(results()),4))
})

output$results <- renderText({
  paste("The posterior mean number of migrating smolts is ",
        posterior.N()$mean, 
        " (95% prob interval:  ",paste(posterior.N()$ci,collapse=" - "),"). ",
        " Below is the plot of this posterior density and 
        some other posterior distributions.
        You can read more about these results and the model
        in the 'about' page.", sep ="")
  })

})