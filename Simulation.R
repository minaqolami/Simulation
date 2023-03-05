#-----------------------#
#  simulation           #
#-----------------------#
#########exp
n=10
u=runif(n)
lambda=5
x=c()
for(i in 1:n){
  x[i]=-lambda*(log(1-u[i]))
}
hist(x)
########
n=20
u=runif(n)
lambda=5
x1=c()
for(i in 1:n){
  x1[i]=-lambda*(log(1-u[i]))
}
hist(x1)
########
n=100
u=runif(n)
lambda=5
x2=c()
for(i in 1:n){
  x2[i]=-lambda*(log(1-u[i]))
}
hist(x2)
#####################
n=1000
u=runif(n)
lambda=5
x3=c()
for(i in 1:n){
  x3[i]=-lambda*(log(1-u[i]))
}
hist(x3)
####### chisq
n=1000
u=runif(n)
x4=c()
for(i in 1:n){
  x4[i]=-2*(log(u[i]))
}
hist(x4,main="chisq")
####### Gamma(1,5)
n=1000
u=runif(n)
x5=c()
for(i in 1:n){
  x5[i]=-5*(log(u[i]))
}
hist(x5,main="Gamma")
####### logstic(0,1)
n=1000
u=runif(n)
x6=c()
a=c()
for(i in 1:n){
  a[i]=u[i]/(1-u[i])
  x6[i]=log(a[i])
}
hist(x6,main="logstitic")
#-----------------------#
#  metro police           #
#-----------------------#
metropolisfun <- function(n, p, T){
  X <- runif(1)
  for (t in 2:T) {
    Y <- rbeta(1,3,6)
    #    alpha <- min(1, (dbeta(Y, n, p) / dbeta(X[t - 1], n, p))*(dbeta(Y, n, p) / dbeta(X[t - 1], n, p)))
    alpha <- min(1, (dbeta(Y, n, p) / dbeta(X[t - 1], n, p))*(dbeta(X[t - 1], 3, 6) / dbeta(Y, 3, 6)))
    if (runif(1) < alpha) X[t] <- Y
    else X[t] <- X[t - 1]
  }
  return(X)
}
metropolisfun (1,3,6)
library(ggplot2)
t <- metropolisfun(3.5,6,1000)
hist(t,probability = T)
lines(density(t))
###########Beta(3,7)
metropolisfun1 <- function(n, p, T){
  X <- runif(1)
  for (t in 2:T) {
    Y <- rbeta(1,3,7)
    #    alpha <- min(1, (dbeta(Y, n, p) / dbeta(X[t - 1], n, p))*(dbeta(Y, n, p) / dbeta(X[t - 1], n, p)))
    alpha <- min(1, (dbeta(Y, n, p) / dbeta(X[t - 1], n, p))*(dbeta(X[t - 1], 3, 7) / dbeta(Y, 3, 7)))
    if (runif(1) < alpha) X[t] <- Y
    else X[t] <- X[t - 1]
  }
  return(X)
}
metropolisfun1 (1,3,7)
library(ggplot2)
t1 <- metropolisfun1(3.5,6,1000)
hist(t1,probability = T)
lines(density(t1))
###########Beta(4,6)
metropolisfun2 <- function(n, p, T){
  X <- runif(1)
  for (t in 2:T) {
    Y <- rbeta(1,4,6)
    #    alpha <- min(1, (dbeta(Y, n, p) / dbeta(X[t - 1], n, p))*(dbeta(Y, n, p) / dbeta(X[t - 1], n, p)))
    alpha <- min(1, (dbeta(Y, n, p) / dbeta(X[t - 1], n, p))*(dbeta(X[t - 1], 4, 6) / dbeta(Y, 4, 6)))
    if (runif(1) < alpha) X[t] <- Y
    else X[t] <- X[t - 1]
  }
  return(X)
}
metropolisfun2 (1,4,6)
library(ggplot2)
t2 <- metropolisfun1(3.5,6,1000)
hist(t2,probability = T)
lines(density(t2))

