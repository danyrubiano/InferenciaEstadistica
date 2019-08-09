source('home/dany/Escritorio/R/import_magic04.R')

#Funcion para el coeficiente de variacion
CoefVar <- function(x){
  cv <- 0
  mean <- 0
  sd <- 0
  {
    mean <- mean(x)
    sd <- sd(x)
    cv <- sd/mean
  }
  return(cv)
}

#Variables a trabajar 
MayorEje <- dataSfLength
log <- data$fSize
MayorSuma <- dataSfConc

#Muestreos para las variables
n <- 1000 
prob <- c(rnorm(19020, mean=0.5, sd=0.1))
#MayorEje 
masMayorEje = sample(MayorEje, size=n) #muestreo aleatorio
mpMayorEJe = sample(MayorEje, size=n, replace-FALSE, prob=prob) #muestreo probabilistico 
bootMayorEje = sample(MayorEje, size=n, replace-TRUE) #remuestreo bootstrap 

#log
maslog = sample(log, size=n) #muestreo aleatorio 
mplog = sample(log, size=n, replace-FALSE, prob=prob) #muestreo probabilistico 
bootlog = sample(log, size=n, replace-TRIJE) #renuestreo bootstrap

#Mayor Suma
masMayorSuma = sample(MayorSuma, size=n) #nuestreo aleatorio 
mpMayorSuma = sample(MayorSuma, size=n, replace=FALSE, prob=prob) #muestreo probabilistico
bootMayorSuma = sample(MayorSuma, size=n, replace-TRIJE) #renuestreo bootstrap


#Analisis para cada muestreo
#Prinero todos los datos (19020)
neantlayorEjec-meari(MayorEje)
sdMayorEje <- sd(MayorEje)
varMayorEje <- var(MayorEje)
nintlayorEjec-nin(MayorEje)
naxMayorEje <- nax(MayorEje)
coefVarMayorEje <- CoefVar(MayorEje)

meanlog <- nean(log)
sdlog <- sd(log)
varlog <- var(log)
minlog <- min(log)
maxlog <- max(log)
coefVarlog <- CoefVar(log)

meanMayorSuna <- nean(Mayor5uma)
sdMayorSuna <- sd(MayorSuma)
varMayor5una <- var(MayorSuma)
mintlayor5una <- min(MayorSuma)
maxMayor5una <- max(MayorSuma)
coefVarMayorSuma <- CoefVar(MayorSuma)

#MAS
meannasMayorEje <- mean(masMayorEje)
sdnasMayorEje <- sd(masMayorEje)
varnastlayorEje <- var(masMayorEje)
minnastlayorEje <- min(masMayorEje)
maxnastlayorEje <- max(masMayorEje)
coefVarmasMayorEje <- CoefVar(masMayorEje)

meanmaslog <- mean(maslog)
sdmaslog                             -sd(maslog)
varmaslog <- var(maslog)
minmaslog <- min(maslog)
maxmaslog <- max(maslog)
coefVarmaslog <- CoefVar(maslog)

meanmas1ayor5una <- mean(masMayorSuma)
sdmasMayor5una <- sd(masMayorSuma)
varmasMayorSuma <- var(masMayorSuma)
minmasMayorSuma <- min(masMayorSuma)
maxmasMayorSuma <- max(masMayorSuma)
coefVarmasMayorsuma <- CoefVar(masMayorSuma)

#MP
meanmpMayorEje <- mean(mpMayorEje)
sdmpMayorEje <- sd(mpMayorEje)
varmpMayorEje <- var(mpMayarEje)
minmpMayorEje <- min(mpMayorEje)
naxmpMayorEje <- max(mpMayorEje)
coefVarmpMayorEjec <- CoefVar(mpMayorEje)

meanmplog <- mean(mplog)
sdmplog <- sd(mplog)
varmplog <- var(mplog)
minmplog <- (mplog)
maxmplog <- max{mplog)
coefVarmplog <- CoefVar(mplog)

meanmpMayorSuna <- mean(mpMayorSuma)
sdmpMayorSurna <- sd(mpMayorSuma)
varmpMayorSuna <- var(mpMayorSuma)
minmpMayorSumac <- min(mpMayorSuma)
maxmpMayorSuma <- max(mpMayorSuma)
coefVarripMayorSuma <- CoefVar(mpMayorSuma)

#Bootstrap
meanboottlayorEje <- mean(bootMayorEje)
sdboottlayorEje <- sd(bootMayorEje)
varbootMayorEje <- var(bootMayorEje)
minbootMayorEje <- min(bootMayorEje)
maxbootMayorEje <- max(bootMayorEje)
coefVarboottlayorEje <- CaefVar(bootMayorEje)

meanbootlog <- mean(bootlog)
sdbootlog <- sd(bootlog)
varbootlog <- var(bootlog)
minbootlog <- min(bootlog)
maxbootlog <- max(bootlog)
coefVarbootlog <- CoefVar(bootlogd

meanboottlayorSuna <- mean(bootMayorSuma)
sdbootMayorSunac-sd(bootMayorSuma)
varbootMayorSuna <- var(bootMayorSuma)
minbootMayorSuna <- min(bootMayorSuma)
maxbootMayorSuna <- max(bootMayorSuma)
coefVarbootMayorSumac <- CoefVar(bootMayorana)

#tablas resunen
#Tabla resunen MayorEje

statsMayorEje‹-c(meanMayorEje,sdMayorEje,varMayorEjeoinMayorEje,naxMayorEje,coefVarMayorEje)
statsMASMayorEje‹-c(meanmasMayorEje,sdnasMayorEje,yarmasMayorEje,ninnasMayorEje,naxmasMayorEje,coefVarmasMayorEje)
statsMPMayorEje‹-c(mearimpMayorEje,sdmpMayorEje,varnpMayorEje,minmpMayorEje,naxnpMayorEje,coefVarmpMayorEje)
statsbootMayorEje‹-c(mearibootMayorEje,sdbootMayorEje,varbootMayorEje,ninbootMayorEjetmaxbootMayorEje,coefVarbootMayorEje)

tableStatsMayorEje‹-matrix(c(statsPMayorEje,statsMASMayorEje,statsMPRayorEje,statsbootMayorEje),nrow=4,ncol=6,byrow = TRUE)
dinnanes(tableStatsMayorEje) = list(c("Poblacion","MAS","Mr,"Bootstrap"),
                                 c("Media","Desviacion","Varianza","Mininc","Maximo","Coef.  Var."))
#Tabla resunen log
statsPlog‹-c(neanlog,sdlogtvarlog,mirilog,maxtog,coefVarlog)
statsMASlog‹-c.(neanmaslog,sdmaslog,varmaslog,ninnaslagoaxmaslag,coefVarnaslog)
statsMPlog‹-c(neanmplog,sdmplogtuarmplog,minnplogoamplog,caefVarnplog)
statsbootlog‹-c(nearibootiog,sdbootlog,varboottog,minlaootlogoaxbootlog,coefVarboottog)

tableStatslog‹-natrix(c(statsPlog,statsMASlog,statsMPlog,statsbootlog),nrow=4,ncol=6tbyrow= TRUE)
dinnanes(tableStatslog) = list(c("Poblacion","MASVMP","Bootstrap"),c("Media","Desviacion","Varianze,"Mininotlaxinc","Coef.  Var."))

#Tabla resunen MayorSuma

statsMayorSuna‹-c(meanMayorSumapsdMayorSuna,varMayorSunaoinMayorSuna,naxMayorSuma,coefVarMayorama)
statsMA5MayorSuna‹-c(meanmasMayorSuna,sdnasMayorSuna,varnasMayorSuna,ninnastlayorSumatmaxmasMayorama,coefVarmasMayorSuna)
statsMPMayorSuna‹-c(meanmpMayorSuma,sdnpMayorSuma,varripMayorSuna,ninnOlayorSuna,naxmpMayorSumatcoefVarmpMayorSuma)
statsbootMayorSuna‹-c(meanbootMayorSuna,sdbootMayorSuma,varbootMayorSuna,ninboottlayorSumatmaxbootMayorSuma,coefVarbootMayorSuna)

tableStatsMayorSuma‹-matrix(c(statsPMayorama,statsMASMayorSuna,statOPMayorSuna,statsbootMayorSuma),nrow=4,ncol=6,15yrow   = TRUE}
dinnanes.(tableStatsMayorSuma) = list(c("Poblacion","MAS","MP","Bootstrap"),
									c("Media","Desviacion","Varianza","Minino","Maximo","Coef.  Var."))

#Histogranas
#Construir histograms con la poblacion y con cada nuestreo.

#Poblacion
hist(MayorEje,prob=TRUE,main="Histograma Poblacion hayorEje",xlab="Porcentaje)
hist(log,prob=7FJE,main="Histograma Poblacion log",xlab="Porcentaje")
hist(MayorSuna,prob=TRUE,main="Histograma Poblacion MayorSuna",xlab="Porcentaje)

#MAS
hist(nasMayorEje,prob=TRUE,main="Histograma mas MayorEje",xlab="Porcentaje")
hist(naslog,prob=TRUE,main="Histograma mas log",xlab="Porcentaje")
hist(nasMayorSuna,prob=TRUE,main="Histograma mas MayorSuna",xlab="Porcentajel

#A1P
hist(npMayorEje,prob=TRUE,main="Histograma mp MayorEje",xlab="Porcentaje")
hist(nplog,prob=TRUE,main="Histograma mp log",xlab="Porcentaje")
hist(npMayorSuna,prob=TRUE,main="Histograma rip MayorSuna",xlab="Porcentaje)

#Bootstrap
hist(bootMayorEjetprob=TRUE,main="Histograma Bootstrap MayorEje",xlab="Porcentaje")
hist(bootlog,prob=TRUE,main="Histograma Bootstrap log",xlab="Porcentaje")
hist(bootMayorSuna,prob=TRUE,main="Histograma Bootstrap MayorSuma",xlab="Porcentaje")

#Seccion para la estimacion de los valores (se usara normal}
#El muestreo usado es el MAS porque es el que dio mejores resultados en la parte anterior

library(MASS)
#EMV

#Funcion normal
Funcionhornal‹-function(parametros) 
fsun(0.5*(x-parametros[1])A2/parametros[2] + 3.S*log(parametros[2]))

#EMV pars el Mayor eje, considerando cue se distribuye de nanera normal
x‹-nastlayorEje
MornalMin=nln(FuncionNorma1,th<-  c(0,5),  hess= TRUE)
estinacionMuMayorEje<-fNormalMInSestimate[1]
estinacionSignaMayorEje‹-sqrt(fNormalMinSestimate[2])

envMayorEje=fitdistr(x,"normal")
envMuMayorEje‹-envMayorEjeSestimate[1]
emvSignaMayorEje‹-emvMayorEjeSestimate[2]

#Tabla que muestra los valores reales y la estimacion EMV con nininizacion e implementada en R
realValuesMayorEje‹-c(meanMayorEje,sdMayorEje)
estinationMayorEje‹-c(estimacionMuMayorEje,estimacionSignaMayorEje)
inplenentedEstinatIonMayorEje‹-c(emvMuMayorEje,emvSignaMayorEje)

tableEstinationEMVMayorEje‹-matrix(c(realValuesMayorEje,estinationtlayorEje,inplenentedEstimationMayorEje),nrow=3,ncol=2,byrow=TRUE)
dinnanes(tableEstimationEMVMayorE = list(c("Valor real","EMV","EMV R"), c("Media","Desviacion"))

#EMV pars log, considerando que se distribuye de nanera normal
x<-maslog
MornalMin=nlm(FuncionNormal,theta<-  c(@,5), hessian = TRUE)
estinaciontlulog‹-fNormalMinSestimate[1]
estinacionSignalog‹-sut(fNormalMinSestimate[2])

envlog=fitdistr(x,"normal.")
envMulog          -emvlogSesttmate[1]
envSignalog‹-envlogSestimatel[2]
#Tabla que nuestra los valores reales y la estimacion EMV con ninimizacion a inplementada en R
realValueslog‹-c(meanlog,sdlog)
estinationlog‹-c(estlmacionMulog,estimacionSigmalog)
inplenentedEstinatIonlog‹-c(emvMulog,emvSigmalog)

tableEstimationEMV1og‹-matrix(c(rea1Valueslog,estinationlog,inplementedEstinationlog),nrow=3,ncol=2,byrow= TRUE)
         
         
dinnanes(tableEstimationEMVlog)  = list(c("Valor  real","EMV","EMV R"), c("Media","Desviacion"))
#EMV para Mayor Stoat considerando que se distribuye de rianera normal
x‹-nasMayorSuna
MornalMin=nln(FuncionNormalttheta<-  c(0,5),  hessian= TRUE)
estinacionMuMayorSumac-fNormalMinSestimate[1]
estinacionSignaMayorSuma‹-scirt(fNormalMinSestinate[2])

envMayorSuna=fitdistr(xt"normal")
envMuMayorSunac-emvMayorSumaSestimate[1]
envSignaMayorSuna‹-emvMayorSumaSestimate[2]

ft-labia que nuestra los valores reales y la estinacion EMV con nininizacion e inplementada en R
realValuesMayorSuma‹-c(meanMayorSuna,sdMayorSuma)
estinationMayorSumac-c(estimaclonMuMayorSuma,estinacionSignaMayorSuna)
inplenentedEstinatIonMayorSuma‹-c(emvMuMayorSuma,envSignaMayarSuna)

tableEstinationEMVMayorSuma‹-matrix(c(realValuesMayorSuna,estinationMayorSuna,inplementedEstimationMayorSuma),nrow=3,ncol=2,byrow=TRUE)
dinnanes(tableEstimationEMVMayorSuma)  = list(c("Valar real","EMV","EMV R"), c("Media","Desviacion"))

#Estinacion par el rietodo de los momentos para el MAS

#Estinacion nonentos para MayorEje,  considerando que se distribuye normal
x‹-nasMayorEje
estinacionMuMayorEjeM-c-mean(x)
estinacionSignaMayorEjeM‹-sugmean(e2)-mean(x)A2)

#tabla que nuestra los valores reales y la estinacian par nonentos
realValuesMayorEje‹-c(meanmasMayorEje,sdmasMayorEje)
estinationMayorEjeM‹-c(estimacionMuMayorEjeM,estinacionSignaMayorEjeM)

tableEstinationMMayorEje‹-matrix(c(realValuesMayorEje,estinationMayorEjeM),nrow=2,ncol=2,byrow=TRUE)
dinnanesUableEstimationMayorEje) = list(c("Valor real","E. Pimentos"), c("Media","Desuiacion"))
#Estlmacton monentos para  Log, considerando que se distribuye normal
x‹-maslog
estimacionMuloOk-nean(x)
estimacionSigmalogM‹-sqrt(nean(e2)-nean(x)A2)

ft-labia que nuestra los vaLores reales y la esttnacion por nonentos
realValueslog‹-c(neannaslog,schaslog)
estimationlogM‹-c(estimacionMulogM,estinacionSignalogM)

tableEstimationMlog‹-natrtx(c(realValueslog,estinationloW,nrow.2,ncol=2,byrow= TRUE)
dimnames(tableEsttnationMlog) = ltst(c("Valor  real","E. Monentos"), grows
c("Media","Desviacion")}1
#Estlmacion momentos para MayorSuna,  considerando que se distribuye normal
x‹-masMayorSuma
estinacionMuMayorSunaM‹-nean(x)
estinacion5ignaMayorSunaM‹-sqrt(nean(e2)-nean(xy2)

#Tabla que nuestra los valores reales y la estinacion por nomentos
realValuesMayor5uma‹-c(neanmasMayorSuna,sdnasMayorSuma)
estinationMayorSunaM‹-c(estinactonMuMayorSunaM,estinacionSignaMayor5unaM)

tableEstimationMayor5una‹-matrix(c(realValuesMayor5una,estimationMayorSumaM),nrow=2,ncol=2,byrow = TRUE)
dimnames(tableEstinationMMayorSuma) = list(c("Valor real","E. Monentos"),c("Media","Deswiacion"))
#Comparacion entre las estinactones MayorEje
tableComparisonEstMayorEje-c-natrix(c(realValuesMayorEje,inplenentedEstinationMayorEje,estinattonMayorEjeM),nrow=3,ncol.2,byrow = TRUE)
dimnames(tableComparisonEstMayorEje) = list(c("Valor real","EMV","E.  Momentos"), c("Media","Desuiacion"))
#Comparacion entre las estimaciones log
tableComparisonEstlog‹-natrix(c(realValueslog,inplenentedEstimationlog,estimationlogM),nrow=3,ncol=2,byrow = TRUE)
dimnames(tableCompartsonEstlog) = list(c("Valor  real","EMV","E. Momentos"), c("Media","Desuiacion")
#Comparacion entre las estimaciones MayorSuna
tableComparisonEstMayor5una‹-natrix(c(realValuesMayorSuna,implementedEstimationMayorSuna,esttnationMayorSunaM),nrow.3,ncol.2,byrow = TRUE)
dimnames(tableCompartsonEstMayorSuna) = list(c("Valor real","EMV","E.  Monentos"), c("Media','Desviacion"))
