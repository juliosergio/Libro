ff <- rep(1/6, 6)
ff
FF <- Reduce("+", ff, accumulate=T)
FF

plot(ff, type="h", col="red", lwd=2, xlab="y", ylab="f(y)")
points(ff)

plot(FF, type="s", col="red", lwd=2, xlab="y", ylab="F(y)")
points(FF, pch=16)

