# Cap 7
source("TiroParabolico.R")
# Experimento -- para cap. 7
set.seed(5)
mis.x <- 0:11 + 0.25*rnorm(12)
yy <- y(mis.x) + 0.48*rnorm(length(mis.x))
plot(mis.x, yy)
mi.df <- data.frame(mis.x, yy)
names(mi.df) <- c("x","y")
mm <- lm(y ~ x+I(x^2), data=mi.df)
print(mm)
ff <- function(x) predict(mm, newdata=data.frame(x=x), type="response")
curve(ff, add=T)
