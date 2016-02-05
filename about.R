div(class = "welcome-container smolt", id ="about",
HTML("<p> The interactive smolt model is an effort 
          to execute the 

<a href='http://blogs.helsinki.fi/taimenlaskenta/?page_id=7'>
  hierarchial bayesian model developed by the 
      students of the University of Helsinki </a>

      using only the R-program for statistical computing. 
      The model was originally used during the summer of 2015 
      to estimate to total number of migrating smolts (trouts) 
      from the River Vantaa. 
      The main motivation of the author of this interactive version 
      is to make sensitivity 
      analysis of the model as smooth as possible 
      and make it easy to explore the output of the model 
      with different prior assumptions.</p> 
<h3 align = 'center'> R-packages and the mcmc simulation</h2>

  <p> Several R-packages are used in this project.
      The web-app is created using the 'Shiny' package 
      and (currently) the mcmc simulation is executed using 
      the 'rcppbugs' package. 
      Coda-package is used for diagnostics of the 
      mcmc simulation.
      The reason not to use programs such as Stan or BUGS, is that 
      if based solely on R, the Smolt app could be published 
      and run online.</p>
<p> Currently the diagnostics of the simulation indicate that 
    the model spesification needs to be changed 
    or other simulation methods need to be considered. 
    The author is currently studying computational statistics 
    so it is possible that in the future the project 
    will continue with the implementation of a new 
    mcmc simulation method.
</p>

<h3 align = 'center'>Whats new</h2>

<p>
    There are several differences compared to the original model. 
    In some aspects the interactive model is more simple. 
    It does not implement Bayesian model avaraging 
    and only models migration with a 'normal like' 
    discrete prior distribution.
    Also, even though 2 devices were used to catch smolts 
    (a fyke and a screw), this model only considers 
    the catches of the screw.</p>
<p>
    The R-package used for the mcmc simulation only allows 
    for the use of a limited number of distributions, 
    but does offer a lot of freedom to 
    manipulate the output of these distributions 
    for further use as hyperparameters. 
    Therefore, even though the use of Poisson distribution 
    was not available, the use of Binomial distribution 
    to model daily migration and catches was plausible.</p>
<p>
    Despite this freedom and the availability of the normal 
    distribution, 
    Gamma distribution was used as the prior 
    for the total number of Smolts
    (in an effort to keep things simple).</p>
<p>
    The most important theoretical effort of improvement 
    in this interactive model is a logistic regression 
    model to explain the catchability of the smolt screw.
<p>
<h3 align='center'> Logistic regression model for catchability</h2>

<p>
    Other than the choice of different distributions 
    in many parts of the model, 
    the biggest differences are with the modelling of catchability.
    As mentioned , the smolts were actually being caught 
    by two different traps; a screw and a fyke.
    Only the screw is considered in this 
    interactive model and an effort has been made in order to 
    improve the modelling of this catchability.</p>

<p>
    A logistic regression model is implemented 
    to explain the daily catchability of the screw 
    using daily water level and temperature 
    as explanatory variables. 
    It is quite difficult to combain this regression model 
    with general prior knowledge of catchability, 
    but it is possible.  
    <a href = 'http://andrewgelman.com/wp-content/uploads/2013/07/g_prior6.pdf'>
    An informative g - prior </a> 
    can be used to match the prior distribution 
    of the model parameters such that when
    evaluated over the distribution of the explanatory 
    variables, the density of 
    the logit transform of the parameters 
    is approximately beta(a, b). 
</p>
<p>
    It has been shown that if
</p>
<pre>
B ~ N(b*e, g*n*(X'X)^-1)
x ~ N(u, Z)

where

b = digamma(a) - digamma(b)
g = [trigamma(a) + trigamma(b)] / p
e = c(1,0,..0) <- p-length
p = number of parameters
n = number of observations

x = explanatory variables

then

exp(x'B) / (1 + exp(x'B) is approximately beta(a,b)
</pre>
<p>
    This means that it is possible to build 
    a logistic regression model 
    that has a prior density approximate to beta(a,b). 
    For example let X be a design matrix containing a column 
    of ones and the water level and temperature. 
    Then once that matrix is defined in R as X,  
    the below code can be used to check that the prior 
    density of a logreg model does indeed closely match 
    a chosen beta distribution.
</p>
<pre>
library(MASS)

get.bg <- function(alpha, beta, parameters = 3) {
           b <- digamma(alpha) - digamma(beta)
           g <- trigamma(alpha) + trigamma(beta)
           g <- g/parameters

           return(c(b = b, g = g))
     }
     
     bg <- get.bg(alpha = 3, beta = 50)
     Sigma <- bg[['g']]*nrow(X)*solve(t(X) %*% X)
     mu <- c(1,0,0)*bg[['b']]
     
     B <- mvrnorm(1000, mu = mu, Sigma = Sigma)
     
     p <- apply(B,1,function(bi) {
            apply(X,1,function(xi) {
              exp(xi%*%bi)/(1+exp(xi%*%bi))
            })
          })
     p <- c(p)

     plot(density(p))
</pre>
<p> 
     Unfortunately the mcmc package used does not 
    allow simulation from the multinormal distribution so this 
    idea could not be implemented as such. 
    Instead of the covariance matrix of B, 
    the variances are used to give each B[i] a univariate 
    normal distribution. However this method does not lead to the 
    prior beta density that is desired.</p>
<h3 align='center'> </h3>
      
     </p>")
)