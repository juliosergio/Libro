# Experimento glm
pp <- read.table("PrecipOctGro.txt")
pp
head(pp)
pp <- data.frame(MiT=as.integer(rownames(pp)), Precip=pp$Precip)
ff <- Precip ~ MiT
plot(pp)
m1 <- lm(ff, data=pp)
abline(m1,col="blue")
f1 <- function(x) predict(m1, newdata=data.frame(MiT=x), type="response")
f1(1980)

m2 <- glm(ff, data=pp, family=Gamma)
f2 <- function(x) predict(m2, newdata=data.frame(MiT=x), type="response")
f2(1980)
curve(f2, add=T, col="red")
