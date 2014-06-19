# Triangulo rotado
m <- cbind(c(1,0), c(2,1), c(1,1))
# Triangulo
plot(c(0,0), # Esto no se pinta
     type="n", xlim=c(0,2.5),ylim=c(0,2.5),
     main="Triangulo rotado",
     xlab="X", ylab="Y")
# matriz de rotacion
alpha <- 32*pi/180
tr <- rbind(c(cos(alpha),-sin(alpha)), c(sin(alpha),cos(alpha)))
mt <- tr %*% m
# lines(c(mt[1,],mt[1,1]), c(mt[2,],mt[2,1]),col="red")
# Se dibujan los dos triangulos:
polygon(x=c(m[1,],NA,mt[1,]), y=c(m[2,],NA,mt[2,]),
        density = 25,
        col=c("black","red"))
lines(c(0,m[1,3]), c(0,m[2,3]), col="blue",lty="dashed")
lines(c(0,mt[1,3]), c(0,mt[2,3]), col="blue",lty="dashed")
