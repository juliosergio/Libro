# Experimento glm
pp <- read.table("PrecipOctGro.txt")
pp
head(pp)
pp <- data.frame(MiT=as.integer(rownames(pp)), Precip=pp$Precip)
ff <- Precip ~ MiT
plot(pp, type="h", xlim=c(1900, 2050))
m1 <- lm(ff, data=pp)
abline(m1,col="blue")
f1 <- function(x) predict(m1, newdata=data.frame(MiT=x), type="response")
f1(1980)

m2 <- glm(ff, data=pp, family=Gamma)
# m2 <- glm(ff, data=pp, family=Gamma(link="log"))
f2 <- function(x) predict(m2, newdata=data.frame(MiT=x), type="response")
f2(1980)
curve(f2, add=T, col="red")

ff3 <- Precip ~ MiT + I(MiT^2) + I(MiT^3) + I(MiT^4)
m3 <- glm(ff3, data=pp, family=Gamma)
f3 <- function(x) predict(m3, newdata=data.frame(MiT=x), type="response")
curve(f3, add=T, col="magenta")
