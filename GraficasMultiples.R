#===================
# Areas y perimetros
aa <- "área"
pp <- "perímetro"
xl <- "área, perímetro"
radio <- seq(0,5,by=0.5) # Vector de radios
area <- pi*radio^2 # Vector de areas
perimetro <- 2*pi*radio # Vector de perimetros
plot(area, radio, type="b", xlab=xl, pch=21, col="red3", bg="gold")
lines(perimetro, radio, type="b", pch=23, col="navyblue", bg="violetred")
legend("bottomright", 
       legend=c(aa,     pp         ), 
       lty=   c(1,      1          ),
       pch=   c(21,     23         ), 
       col=   c("red3", "navyblue" ), 
       pt.bg= c("gold", "violetred") )