# Porcentajes consecutivos por trimestre
prcnt <-c(
    34.6594306764, 35.2754003647, 35.40613204, 35.1855137062,
    36.6282823891, 37.3816513577, 37.5314871844, 36.3784124999,
    37.8949982178, 37.9752539821, 37.5238097329, 38.8349502588,
    39.5894958061, 40.4058337918
)
# Años de los trimestres
anio <- c(rep(2011,4), rep(2012,4), rep(2013,4), rep(2014,2))
# Los trimestres
trim <- c(rep(c("I","II","III","IV"),3), c("I","II"))
# Se construye un data frame comprensivo con
# la información anterior:
desempl.educ.sup <- data.frame(anio,trim,prcnt)
# Se hacen los nombres de los renglones
rownames(desempl.educ.sup) <- paste0(anio,".", trim)

# Se restara una cota inferior para
# producir una vista con la infomación de interés
tinf <- trunc(desempl.educ.sup$prcnt[1]) # entero inferior
# la resta se agregará como una columna del data frame
desempl.educ.sup$d1 <- desempl.educ.sup$prcnt-tinf
desempl.educ.sup$ind <- 1:nrow(desempl.educ.sup)
desempl.educ.sup

plot(desempl.educ.sup$prcnt, 
     ylim=c(34, max(desempl.educ.sup$prcnt)),
     main="Incremento en Desempleo\nEduc.Media Sup o Superior",
     xlab="Trimestres a partir de 2011",
     ylab="% del total de desempleados"
     )

mm <- lm(prcnt ~ ind, data=desempl.educ.sup)
print(mm)
abline(mm, col="red")


