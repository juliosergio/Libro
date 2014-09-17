## noisySinus.R
x <- seq(0,2*pi, length.out=20)
set.seed(2)
y <- sin(x) + 0.30*rnorm(20)
y1 <- sin(x) + 0.15*rnorm(20)
x11()
plot(x,y, type="o", pch=15, col=colors()[120])

x11()
plot(x,y1, type="o")

dev.set(dev.prev())
curve(sin, add=T, lwd=2, col="blue")

dev.set(dev.next())
curve(sin, add=T, lwd=2, col="red")

x11()
plot(x,y, pch=15, col="red")
points(x,y1)
segments(x, y, x, y1, col=colors()[99])
curve(sin, add=T, lwd=3, col="navyblue")

# Copiaremos esto a un pdf
pdf("miArch.pdf") 
# Este se vuelve el actual 
# .. entonces nos regresamos al anterior
dev.set(dev.prev())
# Se hace la copia
dev.copy() # El default es el siguiente
# ahora el pdf es el actual; se tiene que cerrar
# para que la copia tenga efecto:
dev.off()
