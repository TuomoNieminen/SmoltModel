load("data.Rda")

d<-function(numeric) {
  as.double(numeric)
}

VK <- data$VK/100
VL <- data$VL
NR <- nrow(data)
X<-matrix(1.0, ncol = 3, nrow=NR)
X[,2:3] <- c(VK,VL)
dailymin<- data$TotalX
Nmin <- sum(dailymin)
catch <- d(data$ScrewX)
day<- 1:length(catch)

get.bg <- function(alpha, beta, parameters = 3) {
    b <- digamma(alpha) - digamma(beta)
    g <- trigamma(alpha) + trigamma(beta)
    g <- g/parameters
    return(c(b = b,g = g))
} 

ci <- function(vector,roundto) {
  paste(round(quantile(vector,
                 c(0.025,0.975)),
        roundto), collapse= " - ")
}

meanci<-function(vector, name) {
  mean<-round(mean(vector),2)
  ci<-ci(vector,2)
  paste("<p>Mean ",
        name,": ",
        mean,
        " (",ci,")</p>",
        sep="")
}
