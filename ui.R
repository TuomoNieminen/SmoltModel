

shinyUI(fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),

tabsetPanel(

#Welcome
  
  tabPanel("Welcome",
           div(class="welcome-container",
            div(class='smolt', h2("Smolt", 
                             align='center')),
           br(),
           br(),
           fluidRow(
             
             column(6,
                div(class='intro',h5("Welcome to the Smolt model")),   
                div(class='intro',id='intro', 
                  p("A smolted trout is a trout that is ready to migrate 
                    to the sea. The smolt model is an interactive version of 
                   a Bayesian model developed by the students of 
                   the University of Helsinki, which was used 
                   during the spring of 2015 to estimate 
                   the total number of migrating trout smolts 
                   from the river Vantaa."),
                  p("This website has six pages."),
                  p("In the 'About' page 
                    you will find detailed information of the model."),
                  p("In the 'Priors' page you can insert your prior knowledge 
                   of the total number of migrating smolts, catchability 
                   of the smolt screw and the 
                  distribution of the migration rate."),
                  p("In the 'Projections' page you can observe the expected 
                   numbers of daily smolts and catches using the 
                   priors you have chosen."),
                  p("[DEPRECATED] In the 'MCMC' page you can run the model 
                    and observe some simple diagnostics results."),
                  p("[DEPRECATED] In the 'Results' page are the main results 
                    and some plots of interesting posterior distributions."),
                  p("Have fun!")
                 )),
             
                 column(6,
                  img(id='modelpic',src="Smoltmodel.jpg")             
                  )
  )
           )),

# Priors

  tabPanel("Priors",           
    div(class='priors',
    fluidRow(
      
      column(4, h4("Total"),
             
             div(class='numeric-widget',
             numericInput("N.mu",
                          "Number of smolts (N)",
                          min = 1,
                          max = 10^6,
                          step = 100,
                          value = 2600
                          )),
             div(class='numeric-widget',
             numericInput("N.sd",
              		"Standard deviation of N",
              		min = 1,
              		max = 10^6,
              		value = 2000
              		))
        ),
      
      column(4, h4("Catchability"),
            
            div(class='numeric-widget',
            numericInput("q.alpha",
                           "Successes",
                           min = 1,
                           value = 1
                         )),
            div(class='numeric-widget',
            numericInput("q.beta",
                         "Failures",
                         min = 1,
                         value = 10
                         ))
    ),
    
      column(4, h4("Migration rate"),
             
            div(class ='slider-widget',
            sliderInput("p.mu",
              		"Peak day of the migration (p)",
              		min = 1,
              		max = 60,
              		value = 25,
              		step = 1
              		)),
            div(class='slider-widget',
            sliderInput("p.sd",
              		"Standard deviation of p",
              		min= 1,
              		max = 60,
              		value = 10,
              		step = 1
              		))
            )
    )
    ),
    
    fluidRow(
      column(4,plotOutput("prior.N", height='320px')),
      column(4,plotOutput("prior.q", height='320px')),
      column(4,plotOutput("prior.p", height = '320px'))
  )
  ),
  
# Projections

  tabPanel("Projections",
           fluidRow(
             column(6,
                    plotOutput("expect.n")
                    ),
             column(6,
                    plotOutput("expect.x"))
             
             ) 
           ),

# mcmc
  tabPanel("MCMC",
           br(),
           p("DEPRECATED"),
           fluidRow(
             column(4,
                    sliderInput("thin", 
                                "Thinning",
                                min = 1,
                                max = 10,
                                step = 1,
                                value = 5),
                    numericInput("iter",
                                 "Iterations",
                                 min= 5e3,
                                 max= 1e8,
                                 step = 5e3,
                                 value = 10^5)
                    ),
             column(4,
                    br(),br(),
                    textOutput("runtime"),
                    #textOutput("starttime"),
                    hr(),
                    div(style="text-align:center;",
                        actionButton("run","Go!"))
             ),
             
             column(4,
                    br(),
                    numericInput("burn", 
                                 "Burn-in",
                                 min = 100,
                                 max = 10^4,
                                 step = 100,
                                 value = 1000),
                    numericInput("adapt",
                                 "Adaptive period",
                                 min = 0,
                                 max = 10^5,
                                 step = 100,
                                 value = 1000)
             )
             ),
           hr(),
           htmlOutput("summary.text"),
           br(),
           tableOutput("summary.stats"),
           br(),
           tableOutput("summary.quantiles"),
           plotOutput("traces", height = 800)
           ),

# Results
    tabPanel("Results",
             br(),
             p("DEPRECATED"),
             textOutput("results"),
             plotOutput("posterior.N"),
             plotOutput("posterior.n"),
             plotOutput("posterior.q")
             ),
  
    tabPanel("About",
             source("about.R", local=TRUE)$value
             )

)

))