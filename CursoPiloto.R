curso <- read.csv("CursoPiloto.csv", header=T)
reprobados <- curso[,2]-curso[,3]
aprobados <- curso[,3]
horasCurso <- curso[,1]
exito <- aprobados/(aprobados+reprobados)
fit <- glm(cbind(aprobados,reprobados) ~ horasCurso, family=binomial)
summary(fit)

plot(horasCurso, exito)
ff <- function(x) predict(fit, newdata=data.frame(horasCurso=x), type="response")
curve(ff, add=T)

