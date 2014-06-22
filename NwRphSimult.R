# ---- NwRph ----
# JSS
# Metodo de Newton-Raphson para resolver ecuaciones no lineales
# simultaneas
library(numDeriv) # Para calcular el jacobiano

# El modulo o magnitud de un vector
modulus <- function(x) sqrt(sum(x^2))

NwtRph <- function(ff, x0, eps=0.0001, lim=500, absComp=F) {
  # ff: La funcion --o conjunto de funciones-- ver ejemplo
  # adelante.
  # x0: vector inicial
  # eps: el criterio de convergencia
  # lim: limite maximo de iteraciones
  # absComp: si la comparacion es absoluta o no
  #-------------------
  # funcioncita adicional para cada iteracion
  nxt <- function(x) {x - solve(jacobian(ff,x))%*%ff(x)}
  n <- 0 # numero de iteracion
  repeat {
    x <- nxt(x0) # la siguiente aprox
    # Hacemos el modulo de la diferencia para checar
    r <- modulus(x-x0) # distancia entre x y x0
    # Comparacion absoluta o relativa
    if (absComp) { # <-absoluta
      if (r <= eps) return(x)
    } else { # <-relativa
      if (r <= eps*modulus(x0)) return(x)
    }
    # si llega al maximo de iteraciones
    # salimos con null
    if (n > lim) return (NULL)
    n <- n+1
    x0 <- x # para la siguiente iteracion
  }
}

NwtRph0 <- function(ff, x0, eps=0.0001, lim=500, absComp=F) {
  # ff: La funcion --o conjunto de funciones-- ver ejemplo
  # adelante.
  # x0: vector inicial
  # eps: el criterio de convergencia
  # lim: limite maximo de iteraciones
  # absComp: si la comparacion es absoluta o no
  # DIFERENCIA CON LA ANTERIOR:
  #   Aca no se invertira el Jacobiano, sino que se 
  #   resolvera el sistema de ecuaciones
  #-------------------
  # funcioncita adicional para cada iteracion
  #  -- a diferencia de la anterior, esta funcion entrega
  #  -- el incremento que se tiene que aniadir a x
  nxtD <- function(x) {solve(jacobian(ff, x), -ff(x))}
  n <- 0 # numero de iteracion
  repeat {
    difx <- nxtD(x0) # diferencia de la sig. aprox
    x <- x0 + difx # la siguiente aprox
    # Hacemos el modulo de la diferencia para checar
    r <- modulus(difx) # distancia entre x y x0
    # Comparacion absoluta o relativa
    if (absComp) { # <-absoluta
      if (r <= eps) return(x)
    } else { # <-relativa
      if (r <= eps*modulus(x0)) return(x)
    }
    # si llega al maximo de iteraciones
    # salimos con null
    if (n > lim) return (NULL)
    n <- n+1
    x0 <- x # para la siguiente iteracion
  }
}


test <- function(i=1) {
  # Para probar las funciones anteriores
  # nuestras ecuaciones son como sigue:
  # (1) u(x,y) = x^2 +   y*x   - 10 = 0
  # (2) v(x,y) =  y  + 3*x*y^2 - 57 = 0
  # -------
  # Las tenemos que poner como una funcion vectorial
  # donde p[1]==x, p[2]==y
  miFun <- function(p) c(p[1]^2+p[1]*p[2]-10, 
                         p[2]+3*p[1]*p[2]^2-57)
  # ponemos como primer aproximacion lo siguiente:
  p0 <- c(1.5, 3.5)
  # La funcion que usaremos
  NWR <- switch(i, NwtRph, NwtRph0)
  p <- NWR(miFun, p0)
  print(p)
}


