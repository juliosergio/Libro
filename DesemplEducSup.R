prcnt <- 
    c(
        34.6594306764,
        35.2754003647,
        35.40613204,
        35.1855137062,
        36.6282823891,
        37.3816513577,
        37.5314871844,
        36.3784124999,
        37.8949982178,
        37.9752539821,
        37.5238097329,
        38.8349502588,
        39.5894958061,
        40.4058337918
    )
anio <- 
    c(
        rep(2011,4),
        rep(2012,4),
        rep(2013,4),
        rep(2014,2)
    )
trim <-
    c(
        rep(c("I","II","III","IV"),3),
        c("I","II")
    )

desempl.educ.sup <- data.frame(anio,trim,prcnt)
rownames(desempl.educ.sup) <- paste0(anio,".", trim)

tinf <- trunc(desempl.educ.sup$prcnt[1])
desempl.educ.sup$d1 <- with(desempl.educ.sup, prcnt-tinf)
desempl.educ.sup

barplot(desempl.educ.sup$prcnt, 
        main="Desempleo- Educ. Media Sup o Superior\n en Mexico",
        xlab="% del total de desempleados",
        names.arg=rownames(desempl.educ.sup),
        col=cm.colors(14),
        horiz=T,
        las=1)


barplot(desempl.educ.sup$d1, main="Incremento en \nDesempleo- Educ. Media Sup o Superior",
        xlab="% del total de desempleados",
        xaxt="n",
        names.arg=rownames(desempl.educ.sup),
        col=heat.colors(14)[14:1],
        horiz=T,
        las=1)
axis(1, at=0:6, lab=tinf:(tinf+6))

