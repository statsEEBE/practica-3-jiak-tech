#introduccion

x <- c(0, 1)
fx <- c(0.68, 0.32)

#tabla de probabilidad
cbind(x, fx)

plot(x,fx, pch=16, col="green", ylim = c(0,1))

lines(x, fx, type = "h", col="green")

mu <- sum(x*fx)
mu 

sigma <- sum((x-mu)^2*fx)
sigma


#solucion pregunta 1 Q2

n<-43 #numero de muestras

sample(x, n, prob = fx, replace = TRUE)  #sacar muestras con prob = fx

sum(sample(x, n, prob = fx, replace = TRUE)) 

#probabilidad de que salga 13?

Y <- function(i){sum(sample(x, n, prob = fx, replace = TRUE))}

#bucle
m <- 400000
muestra <- sapply(1:m, Y)
muestra


fi <- table(muestra)/m
fi

#frequencias relativas
data.frame(fi, Fi = cumsum(fi))
barplot(fi)


y <- 0:44
pi <- dbinom(13, 43, 0.32)
 df <- data.frame(Y = 0.44, Prob = dbinom((0:44), 44, 0.32))
#pregunta 2, probabilidad de menos de 17. P(y<17)
 
Fi <- cumsum(df$Prob)
Fi 
cbind(y, fi, Fi)


plot(y, Fi, type = "s", col = "red")


#pregunta 3, azar 24 muestras, valor esperado, variancia, quentil quart.

#valor esperado

x <- 24
Pi <- dbinom(x, 24, 0.68)
Pi
mu <- sum(x*Pi)

#variancia

va <- (sum(x - mu)^2*Pi)
va

#primer cuartil 
Fi <- cumsum(Pi)
plot(x, Fi, type = "s", col = "red")

qbinom(0.25, 24, 0.68)













