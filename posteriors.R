# posterior.N<-reactive({
#   N <- results()[["N.mc"]]
#   mean = round(mean(N))
#   ci <- round(quantile(N, c(0.025, 0.975)))
#   list(sample = N, mean = mean, ci = ci)
# })
# posterior.q <- reactive({
#   1/(1+ rowMeans(apply(results()[["q.coefs"]],1,
#                        FUN = function(coefs) {
#                          exp(- X %*% coefs)
#                        })))
# })
# posterior.p <- reactive({
#   p <- results()[["p"]]
#   colMeans(p)
# })
# posterior.n<-reactive({
#   n<- results()[["n"]]
#   colMeans(n)
# })
# 
# mc.object <- reactive({
#   mcmc(data.frame(N = results()[["N.mc"]],
#                   p.mu = results()[["p.mu"]],
#                   p.sd = results()[["p.sd"]],
#                   q.coefs = results()$q.coefs),       
#        thin = priors$thin)
# })