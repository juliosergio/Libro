## ---- EjCap5 ----
#         ^
#         |
#         +--- No quitar, etiqueta para LyX/Knitr
# Solucion para 
# Ejemplo de cap 5
source("NwRphSimult.R")

# Lectura de los datos
# Precipitaciones medias acumuladas del estado de
# Guerrero para el mes
# de octubre, de los anios 1970 al 2010
#>> pp <- scan("GroPrecip.txt")
pp <- read.table("PrecipOctGro.txt")
# su media
mu <- mean(pp$Precip)
sigma <- sd(pp$Precip)
# su varianza
vz <- var(pp$Precip)

# -- Distribucion gamma

# f1 sera la ec. de la media (m)
# f2 sera la ec. de la varianza (v)

# Las dos funciones definidas en terminos de los
# dos parametros func. de distribucion son
# k: shape y e: scale
# esos parametros son:
# (1) u(k,e) = k*e - m = 0
# (2) v(k,e) = k*e^2 - v = 0
# ========
# Las tenemos que poner como una funcion vectorial
# donde p[1]==k, p[2]==e
miFun <- function(p) c(p[1]*p[2]-mu, 
                       p[1]*p[2]^2-vz)
# ponemos como primer aproximacion lo siguiente:
p0 <- c(5,10)
p <- NwtRph(miFun, p0)
dgammaX <- function(x) dgamma(x, shape=p[1], scale=p[2])

test <- function(){
  print(p) 
  h <- hist(pp$Precip, freq=F, xlim=c(0,max(pp$Precip)), 
            col="blue", density=25, breaks=15) 
  curve(dgammaX, col="green", lwd=3, add=T)
  invisible(h)
}

# Comparacion con el metodo de maxima similitud
library(MASS)

compare <- function() {
    ff <- fitdistr(pp$Precip, dgamma, start=list(shape=p0[1], scale=p0[2]))
    print(ff)
    dgammaXX <- function(x) {
        dgamma(x, shape=ff$estimate[[1]], scale=ff$estimate[[2]])
    }
    test()
    curve(dgammaXX, col="red", lwd=3, add=T)
}

