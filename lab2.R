setwd(/home/usuario/Escritorio/”)
data<-read.csv(header=FALSE,col.names=c(”fLength”,”fWidth”,”fSize”,”fConc”,”fConc1”,”fAsym”,
”fM3Long”, ”fM3Trans”,”fAlpha”,”fDist”,”class”),”magic04.data”)

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
atributo<-c(”fLength”,”fWidth”,”fSize”,”fConc”,”fConc1”,”fAsym”,”fM3Long”, ”fM3Trans”,”fAlpha”,”fDist”)

minimo<-c(min(fLength),min(fWidth),min(fSize),min(fConc),min(fConc1),min(fAsym),min(fM3Long),
min(fM3Trans), min(fAlpha),min(fDist))

maximo<-c(max(fLength),max(fWidth),max(fSize),max(fConc),max(fConc1),max(fAsym),max(fM3Long),
max(fM3Trans),max(fAlpha),max(fDist))

table<-data.frame(atributo,minimo,maximo)

rm(fLength,fWidth,fSize,fConc,fConc1,fAsym,fM3Long,fM3Trans,fAlpha,fDist,atributo,minimo,maximo)
