# Los fulanos
participantes <-c(
    "Fulano", "Sutano", "Mengano", "Perengano", "Metano",
    "Etano", "Propano", "Butano", "Pentano", "Hexano",
    "Heptano", "Octano", "Ciclopentano",
    "Enano", "Decano", "Hermano")
transporte <- c(
    "aereo", "terrestre", "tren", "maritimo", "tren",
    "tren", "terrestre", "terrestre", "aereo", "terrestre",
    "maritimo", "terrestre", "aereo",
    "terrestre", "terrestre", "aereo")
info <- data.frame(participantes, transporte)
plot(info$transporte,
     las=2) # hace los textos perpendiculares al eje
tt <- table(info$transporte)
pie(tt, col=terrain.colors(4))
rr <-tt/sum(tt)
barplot(rr,col=rainbow(4))
pie(tt, col=rainbow(4))

congreso2014 <- c(aereo=0.12, maritimo=0.1875, 
                  terrestre=0.4925, tren=0.2)
congreso2014

rr1 <- rbind(rr, congreso2014)
rownames(rr1) <- c("2012","2014") # se dan nombres a renglones

# El resultado mas adecuado se obtiene 
# con la matriz
barplot(rr1, beside=T, col=c(1,2), las=2)
legend("topleft", legend=rownames(rr1), col=c(1,2), pch=15)
# Pero tambien se puede tener el caso de 
# la traspuesta de la matriz original
barplot(t(rr1), beside=T, col=1:4) 
legend(x=4.1, y=0.48, legend=colnames(rr1), col=1:4, pch=15)

# Barras apiladas
barplot(rr1, beside=F, col=c(1,2), las=2)
legend("topleft", legend=rownames(rr1), col=c(1,2), pch=15)

# Pero tambien se puede tener el caso de 
# la traspuesta de la matriz original
barplot(t(rr1), beside=F, col=1:4, xlim=c(0.2, 3*1.2)) 
legend("topright", legend=colnames(rr1), col=1:4, pch=15)


