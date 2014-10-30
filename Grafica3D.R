# Ejamplo de graficacion 3D
# g <- function(x) x/2
g <- function(x) x
# h <- function(y) (1+3*y^2)/2
h <- function(y) y
f <- function(x,y) g(x)*h(y)
x <- seq(-1,1, length.out=16)
y <- seq(-1,1, length.out=16)
z <- outer(x,y,f)

titulo="Paraboloide hiperbólico"

x11()
theta=30
phi=10
repeat {
    ss <- readline("SU-VAR>")
    if (nchar(ss)==0) break
    aa <- strsplit(ss, "=")
    assign(aa[[1]][1],as.numeric(aa[[1]][2]))
    persp(x,y,z,theta = theta, phi = phi, expand=0.7, col = "lightblue",
        ltheta = 120, shade = 0.75, ticktype = "detailed",
        xlab = "X", ylab = "Y", zlab = "z", main=titulo
    ) -> res # Se guarda la transformacion
}
dev.off()
