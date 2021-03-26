library(readxl)
library(moments)
library(stats)
library(fBasics)
library(tidyverse)


DatosBovespa <- read_excel("DatosBovespa.xlsx")
DatosPBR <- read_excel("DatosPBR.xlsx")

########################### DatosBovespa ################################
####### LITERAL 1 log ret 
attach(DatosBovespa)
basicStats(Log_ret)
#media del rendimiento logaritmico
t.test(Log_ret)
#Dado que el valor p> 5%, no podemos rechazar H0. Por tanto, la verdadera media es igual a cero.

###### LITERAL 2
hist(Log_ret, freq = F, xlim = c(-0.068953,0.068585), col= "lightskyblue1")
#lines(density(Log_ret))
curve(dnorm(x, mean(Log_ret), sd(Log_ret)), lwd = 2, col = "lightseagreen", add = T)
legend("topright", c("curva observada", "curva (normal) teórica"),
       lty = 2, lwd = 2, col = c("black", "lightseagreen"), bty = "n",
       cex = 0.6)
#####pruebas de normalidad
ks.test(x = Log_ret,"pnorm", mean(Log_ret), sd(Log_ret))
####metodo de Jarque-Bera 
normalTest(Log_ret,method="jb")
#Dado que p valor <5%, deberÃ­amos rechazar H0. Por tanto, el retorno no sigue la distribuciÃ³n normal.
########## LITERAL 3

media_anual<-mean(Log_ret)
volatilidad_anual <- media_anual*(sqrt(245))   ##en informe poner 252

########## LITERAL 4


######################## Datos Petrobras ###############################
attach(DatosPBR)
####### LITERAL 1 log ret 
basicStats(Log_ret1)
#media del rendimiento logaritmico
t.test(Log_ret1)
#Dado que el valor p> 5%, no podemos rechazar H0. Por tanto, la verdadera media es igual a cero.
###### LITERAL 2

hist(Log_ret1, freq = F, xlim = c( -0.151437,0.144089), col= "lightskyblue1")
#lines(density(Log_ret1))
curve(dnorm(x, mean(Log_ret1), sd(Log_ret1)), lwd = 2, col = "lightseagreen", add = T)
legend("topright", c("curva observada", "curva (normal) teórica"),
       lty = 1, lwd = 2, col = c("black", "lightseagreen"), bty = "n",
       cex = 0.6)

#####pruebas de normalidad
ks.test(x = Log_ret1,"pnorm", mean(Log_ret1), sd(Log_ret1))
####metodo de Jarque-Bera 
normalTest(Log_ret1,method="jb")
#Dado que p valor <5%, deberÃ­amos rechazar H0. Por tanto, el retorno no sigue la distribuciÃ³n normal.

a<-select(DatosBovespa,Log_ret)
b<-select(DatosPBR,Log_ret1)
h<-cbind(a,b)
cor(h)


DataiB <- read_excel("Simulaciones.xlsx", 
                           sheet = "iBovespa")
DataPet <- read_excel("Simulaciones.xlsx", 
                     sheet = "Petrobrás")

ksm<-DataiB[200,]
ksm<-select(ksm,-N)
ll<-as.numeric(ksm)
ask<-log(ll)
hist(ll)
hist(ask)

basicStats(ll)

hist(ll, freq = F, xlim = c(  1.423999e+04,4.082861e+04), col= "lightskyblue1")
lines(density(ll))
curve(dlnorm( ll,0, 1, log = F), lwd = 2, col = "lightseagreen", add = T)


VAR<-quantile(ll,0.99)
VAR
dlnorm()
mlln
mlln


lines(mllnorm(ll), lwd = 2, lty = 1, col = "blue")
