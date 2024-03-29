## Poisson distributions

x <- 0:20
ds10 <- dpois(x, 10)
plot(x=x, y=ds10, type="h", lwd=2, xlab="y", ylab="ds10(y)")
# Por claridad se agregan puntos al grafico:
points(x, ds10, pch=21, bg="red")

Ps10A <- Reduce('+', ds10, accumulate=T)
Ps10 <- ppois(x, 10)

plot(x, Ps10, type="s", col="red", lwd=2, xlab="y", ylab="Ps10(y)") 
points(x, Ps10, pch=16)


cc <- c("yellow", "springgreen", "navyblue")
lambdas <- c(6, 10, 13)
Ps <- as.data.frame(lapply(lambdas, function(ll) ppois(x, ll)))
ds <- as.data.frame(lapply(lambdas, function(ll) dpois(x, ll)))
names(Ps) <- lambdas
names(ds) <- lambdas
funcs <- list(plot, lines)

for (i in 1:3) {
    ff <- funcs[[1+(i!=1)]]
    ff(x,ds[[i]], type="o", pch=21,bg=cc[i])
}

# Construccion de una expresion para las pintar
# los simbolos correspondientes a lambda

qq <-as.expression(lapply(lambdas, function(x) bquote(lambda==.(x))))

legend("topright", 
       legend=qq,
       lty=1, pch=21, pt.bg=cc)

for (i in 1:3) {
    ff <- funcs[[1+(i!=1)]]
    ff(x,Ps[[i]], type="s")
    points(x,Ps[[i]],pch=21,bg=cc[i])
}
legend("bottomright", 
       legend=qq,
       lty=1, pch=21, pt.bg=cc)

lapply(lambdas, function(ll) 1-ppois(14, ll))

lapply(lambdas, function(ll) ppois(14, ll, lower.tail=F))


