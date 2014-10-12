## ---- Gamma1 ----
# Funciones gamma
d <- data.frame(
    k = c(1,2,3,5,9),
    theta = c(2,2,2,1,0.5),
    col = c("red","green","blue","cyan","brown"),
    stringsAsFactors=F
)

# txt <- paste0("k=", d$k, ", theta=", d$theta)
txt <- as.expression(
    lapply(1:nrow(d),
           function(i) bquote(f(y,k==.(d[i,1]), theta==.(d[i,2])))
    )
)
    
ff <- function(x) dgamma(x, shape=d[1,]$k, scale=d[1,]$theta)
curve(ff, main="", col=d[1,]$col, xlim=c(0,20), ylim=c(0, 0.5),
      xlab="y", ylab=expression(f(y,k,theta)))

for (i in 2:nrow(d)) {
  ff <- function(x) dgamma(x, shape=d[i,]$k, scale=d[i,]$theta)
  curve(ff, add=T, main="", col=d[i,]$col)
}

legend("topright", legend=txt, col=d$col, pch=15)
