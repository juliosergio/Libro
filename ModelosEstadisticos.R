# Modelos Estadisticos

tt <- read.table("GroPrecip1a12.txt")
tMango <- read.csv("GroProdMangoR.csv", header=T)
tMgs <- split(tMango, tMango$anio) # Partimos por anio
# Los maximos de produccion por anio
MaxMgs <- do.call(
    rbind, 
    by(tMango, tMango$anio, function (elt) elt[which.max(elt$Rndmto.Sem),])    
    )

plot(MaxMgs$anio, MaxMgs$Rndmto.Sem, type="h")

# Dividimos la lluvia en dos periodos: Primavera(Ene:1-Abr:4), 
#  Verano(May:5-Sep:9)
tt$prim <- Reduce('+', tt[,1:4])
tt$vern <- Reduce('+', tt[,5:9])

# Recortemos (1999:2010)
MaxMgs <- MaxMgs[1:(nrow(MaxMgs)-2),]
tt <- tt[rownames(MaxMgs),]
# Pongamos lo que interesa en una tabla
tb <- as.data.frame(
        cbind(anio=MaxMgs$anio, pp1.4=tt$prim, pp5.9=tt$vern, 
              Sembrado=MaxMgs$Sembrado, Producido=MaxMgs$Producido, 
              Rndmto.Sem=MaxMgs$Rndmto.Sem)
)

aa <- range(tb$Rndmto.Sem)
bb <- c(0,1)
mm <- lm(bb ~ aa)
ff <- function(x) predict(mm, newdata=data.frame(aa=x), type="response")
tb$Prop <- ff(Rndmto.Sem)
tb$Good <- tb$Prop*tb$Sembrado
tb$Bad <- tb$Sembrado - tb$Good














