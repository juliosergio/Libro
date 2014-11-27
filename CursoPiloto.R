curso <- read.csv("CursoPiloto.csv", header=T)
reprobados <- curso$Grupo-curso$Aprobados
aprobados <- curso$Aprobados
horasCurso <- curso$Horas
exito <- aprobados/(aprobados+reprobados)
fit <- glm(cbind(aprobados,reprobados) ~ horasCurso, family=binomial)
summary(fit)

plot(horasCurso, exito)
ff <- function(x) predict(fit, newdata=data.frame(horasCurso=x), type="response")
curve(ff, add=T)
 
# binomiales

# Creador de funciones de probabilidades binomiales 
creaFBinom <- function(n,p) function(k) dbinom(k, n, p) 
# Para el caso del ejemplo:
n <- 21
p <- 6/21 # probabilidad
ffb <- creaFBinom(n, p)
# ffb(k) sería la función binomial para una k dada
# .. para el caso de todas las k de nuestro interés:
k <- 0:n # Esto es: 0,1,..,21
barplot(ffb(k), names.arg=k, xlab="k", ylab="Pr(k)") 

library(boot)
curve(inv.logit, col="blue", ylim=c(0,1), xlim=c(-6,6), ylab="logistic(x)")
curve(logit, col="blue", xlim=c(0,1), ylim=c(-6,6))



