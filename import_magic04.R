
library(psych)
library(help = "datasets")
library(modeest)

setwd("C:/Users/Dany/Documents/Inferencia Estadistica")
data<-read.csv(header=FALSE,col.names=c("fLength","fWidth","fSize","fConc","fConc1","fAsym",
                              "fM3Long","fM3Trans","fAlpha","fDist","class"),"magic04.data")

###########################################################################################
#Funciones usadas en el laboratorio 2
###########################################################################################


primerosdiez<-head(data,n=10)

fLength<-data$fLength
fWidth<-data$fWidth
fSize<-data$fSize
fConc<-data$fConc
fConc1<-data$fConc1
fAsym<-data$fAsym
fM3Long<-data$fM3Long
fM3Trans<-data$fM3Trans
fAlpha<-data$fAlpha
fDist<-data$fDist
fclass<-data$class


atributo<-c("fLength","fWidth","fSize","fConc","fConc1","fAsym","fM3Long",
            "fM3Trans","fAlpha","fDist")
minimo<-c(min(fLength),min(fWidth),min(fSize),min(fConc),min(fConc1),min(fAsym),
          min(fM3Long),min(fM3Trans),min(fAlpha),min(fDist))
maximo<-c(max(fLength),max(fWidth),max(fSize),max(fConc),max(fConc1),max(fAsym),
          max(fM3Long),max(fM3Trans),max(fAlpha),max(fDist))

table<-data.frame(atributo,minimo,maximo)
rm(fLength,fWidth,fSize,fConc,fConc1,fAsym,fM3Long,fM3Trans,fAlpha,fDist,atributo,
   minimo,maximo)

###########################################################################################

#Funciones para obtener los totales de cada clase
temp<-which(data$class=="g")
typeg<-data[temp,]
temp<-which(data$class=="h")
typeh<-data[temp,]

#funciones para obtener las frecuencias de cada clase
fg<-nrow(typeg)
fh<-nrow(typeh)
total<-nrow(data)

relfg<-fg/total
relfh<-fh/total

typeNames<-c("hadron","gamma")

absFreq<-c(fh,fg)
relFreq<-c(relfh,relfg)
freqTable<-matrix(c(absFreq,relFreq),nrow=2,ncol=2)

dimnames(freqTable) = list(c("Hadron","Gamma"),c("Frecuencia absoluta","Frecuencia relativa"))

#Grafico de barras con la frecuencia absoluta
barplot(c(fh,fg),space=0,names.arg=typeNames,main="Cantidad de Destellos de Luz en la base de datos",
  xlab="Tipo de Destellos de Luz",ylab="Numero de Destellos de Luz", col=c("orange","cyan"))


#funciones creadas

#Funcion para calcular el coef de variacion
CoefVar<-function(x){
  cv<-0
  mean<-0
  sd<-0
  {
  mean<-sapply(x,mean,na.rm=TRUE)
  sd<-sapply(x,sd,na.rm=TRUE)
  cv<-sd/mean
  }
  return(cv)
}

#Función para obtener la correlacion de Pearson
PearsonCorrelation<-function(data){
  correlationFactor<-0
  {
  correlationFactor<-cor(data,use="complete.obs",method="pearson")
  }
  return(correlationFactor)
}

#Función que elimina  la columna de clase
DeleteIDType<-function(data){
  {
    data<-data[,-11]
  }
  return(data)
}

#CATEGORiA de Hadrones
#Estadisticas de la primera categoria hadrones
typeh<-DeleteIDType(typeh)
meanh<-sapply(typeh,mean,na.rm=TRUE)
sdh<-sapply(typeh,sd,na.rm=TRUE)
varh<-sapply(typeh,var,na.rm=TRUE)
minh<-sapply(typeh,min,na.rm=TRUE)
maxh<-sapply(typeh,max,na.rm=TRUE)
coefVarh<-CoefVar(typeh)

statsh<-matrix(c(meanh,sdh,varh,minh,maxh,coefVarh),nrow=10,ncol=6)
dimnames(statsh) = list(c("fLength","fWidth","fSize","fConc","fConc1","fAsym","fM3Long","fM3Trans",
                  "fAlpha","fDist"),c("Media","Desviacion","Varianza","Minimo","Maximo","Coef. Var."))

#Calculo de las modas
trendh_fLenght<-mfv(typeh$fLenght)	
trendh_fWidth<-mfv(typeh$fWidth)	
trendh_fSize<-mfv(typeh$fSize)	
trendh_fConc<-mfv(typeh$fConc)	
trendh_fConc1<-mfv(typeh$fConc1)	
trendh_fAsym<-mfv(typeh$fAsym)	
trendh_fM3Long<-mfv(typeh$fM3Long)	
trendh_fM3Trans<-mfv(typeh$fM3Trans)	
trendh_fAlpha<-mfv(typeh$fAlpha)
trendh_fDist<-mfv(typeh$fDist)

trendsh<-c(trendh_fLenght,trendh_fWidth,trendh_fSize,trendh_fConc,trendh_fConc1,trendh_fAsym,
           trendh_fM3Long,trendh_fM3Trans,trendh_fAlpha,trendh_fDist)

#Histogramas para las variables continuas de la categoria	hadron
histBy(typeh,var="fLength",main="Histograma para fLength en los destellos de hadrones")
histBy(typeh,var="fWidth",main="Histograma para fWidth en los destellos de hadrones")estimacion[n]
histBy(typeh,var="fSize",main="Histograma para fSize en los destellos de hadrones")
histBy(typeh,var="fConc",main="Histograma para fConc en los destellos de hadrones")
histBy(typeh,var="fConc1",main="Histograma para fConc1 en los destellos de hadrones")
histBy(typeh,var="fAsym",main="Histograma para fAsym en los destellos de hadrones")
histBy(typeh,var="fM3Long",main="Histograma para fM3Long en los destellos de hadrones")
histBy(typeh,var="fM3Trans",main="Histograma para fM3Trans en los destellos de hadrones")
histBy(typeh,var="fAlpha",main="Histograma para fAlpha en los destellos de hadrones")
histBy(typeh,var="fDist",main="Histograma para fDist en los destellos de hadrones")

#calculo de la correlacion entre las variables
corh<-PearsonCorrelation(typeh)


#CATEGORiA de Gamma
#Estadisticas de la primera categoria gamma
typeg<-DeleteIDType(typeg)
meang<-sapply(typeg,mean,na.rm=TRUE)
sdg<-sapply(typeg,sd,na.rm=TRUE)
varg<-sapply(typeg,var,na.rm=TRUE)
ming<-sapply(typeg,min,na.rm=TRUE)
maxg<-sapply(typeg,max,na.rm=TRUE)
coefVarg<-CoefVar(typeg)

statsg<-matrix(c(meanh,sdg,varg,ming,maxg,coefVarg),nrow=10,ncol=6)
dimnames(statsg) = list(c("fLength","fWidth","fSize","fConc","fConc1","fAsym","fM3Long","fM3Trans",
                  "fAlpha","fDist"),c("Media","Desviacion","Varianza","Minimo","Maximo","Coef. Var."))

#Calculo de las modas
trendg_fLength<-mfv(typeg$fLength)	
trendg_fWidth<-mfv(typeg$fWidth)	
trendg_fSize<-mfv(typeg$fSize)	
trendg_fConc<-mfv(typeg$fConc)	
trendg_fConc1<-mfv(typeg$fConc1)	
trendg_fAsym<-mfv(typeg$fAsym)	
trendg_fM3Long<-mfv(typeg$fM3Long)	
trendg_fM3Trans<-mfv(typeg$fM3Trans)	
trendg_fAlpha<-mfv(typeg$fAlpha)
trendg_fDist<-mfv(typeg$fDist)

trendsg<-c(trendg_fLength,trendg_fWidth,trendg_fSize,trendg_fConc,trendg_fConc1,trendg_fAsym,
           trendg_fM3Long,trendg_fM3Trans,trendg_fAlpha,trendg_fDist)

#Histogramas para las variables continuas de la categoria	hadron
histBy(typeg,var="fLength",main="Histograma para fLength en los destellos de gamma")
histBy(typeg,var="fWidth",main="Histograma para fWidth en los destellos de gamma")
histBy(typeg,var="fSize",main="Histograma para fSize en los destellos de gamma")
histBy(typeg,var="fConc",main="Histograma para fConc en los destellos de gamma")
histBy(typeg,var="fConc1",main="Histograma para fConc1 en los destellos de gamma")
histBy(typeg,var="fAsym",main="Histograma para fAsym en los destellos de gamma")
histBy(typeg,var="fM3Long",main="Histograma para fM3Long en los destellos de gamma")
histBy(typeg,var="fM3Trans",main="Histograma para fM3Trans en los destellos de gamma")
histBy(typeg,var="fAlpha",main="Histograma para fAlpha en los destellos de gamma")
histBy(typeg,var="fDist",main="Histograma para fDist en los destellos de gamma")

#calculo de la correlacion entre las variables
corg<-PearsonCorrelation(typeg)

