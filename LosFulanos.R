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
