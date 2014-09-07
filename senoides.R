# funciones senoidales
# ==========
# Funcion productora de funciones 
# senoidales
fsin <- function(i,j) {
    function (x) 1/(1+j)*sin(x + i*pi/4)
}

n <- 4
for (i in 0:n) {
    curve(fsin(i,i)(x), xlim=c(0,2*pi), col=rainbow(n+1)[i+1], add=(i!=0))
}
