#pregunta 3
#Verique grácamente que se cumple la Ley de los Grandes Números (LGN) para la distribución Exp(λ).
set.seed(31416)
n <- 2000 #definimos una serie del 20 al 100 con incrementos de 20 para ver el progreso de convergencia
lambda <- 11
vectorexp <- rexp(n,lambda)
vectorProm <- 1:n
for (j in 1:n){
  vectorProm[j] <- mean(vectorexp[1:j])
}
plot(vectorProm, type="l")
abline(h=1/lambda, col="red", lty=2)

#pregunta 2
#Verique grácamente que se cumple el Teorema Central del Límite (TCL) para la distribución Exp(λ).
m <- 1000
n <- 1000
z <- 1:n
lamda <- 11
for (i in 1:m){
  z[i] <- (mean(rexp(n,lambda)) - 1/lambda)/((1/lambda)/sqrt(n))
}
hist(z, probability = T)
lines(density(z))
lines(seq(-4,4,.1),dnorm(seq(-4,4,.1)),col="red", lty=2)

#pregunta 3
#Modique su código de mo do que sólo deba de cambiar las siguientes tres funciones cuando se desee probar
#otra distribución:
rdistn <- function (n , param ){
  mu <- param[1]
  sigma <- param[2]
  return ( rlnorm ( n , mu, sigma ))
}
mean_distn <- function ( param ) {
  mu <- param[1]
  sigma <- param[2]
  return ( exp(mu + sigma/2) )
}
sigma2_distn <- function ( param ) {
  mu <- param[1]
  sigma <- param[2]
  return ( (exp(sigma)-1)*exp(2*mu+sigma))
}

lgn <- function (n,param){
  vectorexp <- rdistn(n,param)
  vectorProm <- 1:n
  for (j in 1:n){
    vectorProm[j] <- mean(vectorexp[1:j])
  }
  plot(vectorProm, type="l", main="Ley de los Grandes Números", ylab="Medias Muetrales", xlab="")
  abline(h=mean_distn(param), col="red", lty=2)
}

#lgn(2000,10)

tcl <- function(n,m,param){
  z <- 1:n
  for (i in 1:m){
    z[i] <- (mean(rdistn(n,param)) - mean_distn(param))/(sqrt(sigma2_distn(param))/sqrt(n))
  }
  hist(z, probability = T, main="Teorema Central Límite")
  lines(density(z))
  lines(seq(-4,4,.1),dnorm(seq(-4,4,.1)),col="red", lty=2)
}

#tcl(2000,2000,10)

#Modique las 3 funciones anteriores para vericar los resultados p ero ahora para una distribución lognormal
#de parámetros μ y σ2

lgn(2000,c(0,1))
tcl(2000,2000,c(0,1))

