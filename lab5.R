source('C:\Users\Dany\Documents\Inferencia Estadistica/import_magic04.R')

#Variables a trabajar
MayorEje<-data$fLength
log<-data$fSize
MayorSuma<-data$fConc

#Muestreos para las variables
n<-1000
prob<-c(rnorm(19020,mean=0.5,sd=0.1))

#mayorEje
masMayorEje=sample(MayorEje, size=n) #muestreo aleatorio
masMayorEje2=sample(MayorEje, size=n) #muestreo aleatorio

#log
maslog=sample(log, size=n) #muestreo aleatorio
maslog2=sample(log, size=n) #muestreo aleatorio

#MayorSuma
masMayorSuma=sample(MayorSuma, size=n) #muestreo aleatorio
masMayorSuma2=sample(MayorSuma, size=n) #muestreo aleatorio

library(stats)
normalTestMayorEje<-ks.test(unique(masMayorEje),"pnorm",mean(masMayorEje),sd(masMayorEje))
normalTestMayorEje2<-ks.test(unique(masMayorEje2),"pnorm",mean(masMayorEje2),sd(masMayorEje2))

normalTestLog<-ks.test(unique(maslog),"pnorm",mean(maslog),sd(maslog))
normalTestLog2<-ks.test(unique(maslog2),"pnorm",mean(maslog2),sd(maslog2))

normalTestMayorSuma<-ks.test(unique(masMayorSuma),"pnorm",mean(masMayorSuma),sd(masMayorSuma))
normalTestMayorSuma2<-ks.test(unique(masMayorSuma2),"pnorm",mean(masMayorSuma2),sd(masMayorSuma2))

#Test para la media poblacional con dos muestras no pareadas
testMeanDiffMayorEje<-t.test(masMayorEje,masMayorEje2)
testMeanDiffLog<-t.test(maslog,maslog2)
testMeanDiffMayorSuma<-t.test(masMayorSuma,masMayorSuma2)


#Media con varianza desconocida
#los valores mu son las medias reales obtenidas del laboratorio anterior
testMeanMayorEje<-t.test(masMayorEje,mu=53.25015)
testMeanMayorEje2<-t.test(masMayorEje2,mu=53.25015)

testMeanLog<-t.test(maslog,mu=2.825017)
testMeanLog2<-t.test(maslog2,mu=2.825017)

testMeanMayorSuma<-t.test(masMayorSuma,mu=0.3803271)
testMeanMayorSuma2<-t.test(masMayorSuma2,mu=0.3803271)


#Test Wilcoxon no pareado
testWilcoxMayorEje<-wilcox.test(masMayorEje,masMayorEje2)
testWilcoxLog<-wilcox.test(maslog,maslog2)
testWilcoxMayorSuma<-wilcox.test(masMayorSuma,masMayorSuma2)


#Test Kolmogorov-Smirnov 
testKSMayorEje<-ks.test(masMayorEje,masMayorEje2)
testKSLog<-ks.test(maslog,maslog2)
testKSMayorSuma<-ks.test(masMayorSuma,masMayorSuma2)