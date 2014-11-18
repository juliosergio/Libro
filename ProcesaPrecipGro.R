# Procesamiento de precipitaciones en Gro
tt <- read.table("GroPrecip1a12.txt")
names(tt) <- 1:12
dd <- NULL
for (nn in rownames(tt)) {
    dd <- rbind(
        dd,
        data.frame(
            Tiempo=as.numeric(nn) + (as.numeric(names(tt))-1)/12,
            Precip=as.numeric(tt[nn,])
            )
        )
}

