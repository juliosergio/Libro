## Poisson distributions

x <- 1:20
cc <- c("yellow", "springgreen", "navyblue")
lambdas <- c(6, 10, 13)
Ps <- as.data.frame(lapply(lambdas, function(ll) ppois(x, ll)))
ds <- as.data.frame(lapply(lambdas, function(ll) dpois(x, ll)))
names(Ps) <- lambdas
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

