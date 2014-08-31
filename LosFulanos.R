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

congreso2014 <- c(aereo=0.12, maritimo=0.1875, terrestre=0.4925, tren=0.2)
rr1 <- cbind(rr, congreso2014)
colnames(rr1) <- c("2012","2014")
rr1

barplot(t(rr1), beside=T, col=c(1,2))
legend("topleft", legend=c("2012","2014"), col=c(1,2), pch=15)

