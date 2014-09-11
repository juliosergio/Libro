## Poisson distributions

x <- 0:20
ds10 <- dpois(x, 10)
plot(x=x, y=ds10, type="h", lwd=2, xlab="y", ylab="ds10(y)")
# Por claridad se agregan puntos al gráfico:
points(x, ds10, pch=21, bg="red")

Ps10 <- Reduce('+', ds10, accumulate=T)
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
legend("topright", 
       legend=paste0("lambda=",lambdas),
       lty=1, pch=21, pt.bg=cc)

for (i in 1:3) {
    ff <- funcs[[1+(i!=1)]]
    ff(x,Ps[[i]], type="s")
    points(x,Ps[[i]],pch=21,bg=cc[i])
}
legend("bottomright", 
       legend=paste0("lambda=",lambdas),
       lty=1, pch=21, pt.bg=cc)

