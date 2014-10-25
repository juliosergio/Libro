# Ejamplo de graficacion 3D
g <- function(x) x/2
h <- function(y) (1+3*y^2)/2
f <- function(x,y) g(x)*h(y)
x <- seq(0,2, length.out=16)
y <- seq(0,1, length.out=8)
z <- outer(x,y,f)

x11()

persp(x,y,zz,theta = 30, phi = 30, expand=0.7, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "zz", main="PRUEBA"
) -> res # Se guarda la transformacion

