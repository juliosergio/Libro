# Solucion para 
# Ejemplo de cap 5
source("NwRphSimult.R")

# Lectura de los datos
# Precipitaciones medias acumuladas del estado de
# Guerrero para el mes
# de octubre, de los anios 1970 al 2010
datos <- scan("GroPrecip.txt")
# su media
m <- mean(datos)
# su varianza
v <- var(datos)

# -- Distribucion gamma

# f1 sera la ec. de la media (m)
# f2 sera la ec. de la varianza (v)

# Las dos funciones definidas en terminos de los
# dos parametros func. de distribucion son
# k: shape y e: scale
# esos parametros son:
# (1) u(k,e) = k*e - m = 0
# (2) v(k,e) = k*e^2 - v = 0
# -------
# Las tenemos que poner como una funcion vectorial
# donde p[1]==k, p[2]==e
miFun <- function(p) c(p[1]*p[2]-m, 
                       p[1]*p[2]^2-v)
# ponemos como primer aproximacion lo siguiente:
p0 <- c(5,2)
p <- NwtRph(miFun, p0)
print(p)

h <- hist(datos, freq=F, xlim=c(0,max(datos)), 
          col="blue", density=25, breaks=15)

g <- function(x) dgamma(x, shape=p[1], scale=p[2])
curve(g, col="green", lwd=3, add=T)


