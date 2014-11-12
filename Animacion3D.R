

# SCRIPT
# -----------
rm(list=ls()) # comando para limpar a memória do R

#---------------------------------------­----------------------------------
# Entrada de dados
#---------------------------------------­----------------------------------

Y=c(16.68,11.5,12.03,14.88,13.75,18.11,8.0,17.83,79.24,21.5,40.33,21,13.5,
19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75)
X1=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
X2=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,200,
132,36,770,140,810,450,635,150)

ex1=as.data.frame(cbind(Y,X1,X2)); ex1

attach(ex1) # anexando os dados na memória do R
names(ex1)
dim(ex1)

#---------------------------------------­-------------------
# Gráfico com as 3 variáveis em estudo em 2D
#---------------------------------------­-------------------
library(scatterplot3d)
fig = scatterplot3d(X1,X2,Y, box=F, type='p', lwd=1, pch=19,
xlim=c(0,50), ylim=c(0,1500), zlim=c(0,150))
plano = lm(Y~X1+X2)
fig$plane3d(plano, lty.box = "solid")

#---------------------------------------­-------------------
# Gráfico com as 3 variáveis em estudo em 3D
#---------------------------------------­-------------------

#>> require(Rcmdr)
library(car)
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
bg='black', sphere.size=1.2, revolutions=2, axis.col='white',
xlab="Quantidade Estocada", ylab="Tempo (min)", zlab="Distancia (pes)")

#---------------------------------------­-------------------
# Script montado pelo prof. Dr. Silvano Cesar da Costa (Lattes: http://lattes.cnpq.br/4370110434524936 )
#
# Apresentado pela prof. Dra. Ana Vergínia Libos Messetti (Lattes: http://lattes.cnpq.br/1137362392468500 )
#
# Ambos do Departamento de Estatística (DSTA) do Centro de Ciências Exatas (CCE) da Universidade Estadual de Londrina (UEL) - http://www.uel.br/cce/dsta


