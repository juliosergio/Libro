# 
g <- 9.81 # aceleracion gravedad
x0 <- 0   # x inicial
y0 <- 15  # y inicial
vi <- 7  # velocidad inicial
alphaD <- 50 # angulo-grados
alpha <- (pi/180)*alphaD # angulo-radianes
vox <- vi*cos(alpha) # componente x de velocidad inicial
voy <- vi*sin(alpha) # componente y de velocidad inicial

ft <- function(x) (x-x0)/vox # tiempo para una x dada
y <- function(x) {t <- ft(x); -(g/2)*t^2 + voy*t + y0}
curve(y,xlim=c(0,11),asp=1, main="Lanzamiento proyectil")
abline(v=0,col="red")
abline(h=0,col="red")
points(x0,y0,pch=19, col="red")

las.x <- seq(from=0, to=11, by=0.5)
las.t <- (las.x - x0)/vox
las.y <- -(g/2)*las.t^2 + voy*las.t + y0