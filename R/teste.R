remove.packages("iplan")
setwd("~/")
edges=read.csv(paste(getwd(),"/iplan-precedencias.csv",sep=""),comment.char="#")
vertices=read.csv(paste(getwd(),"/iplan-funcionalidades.csv",sep=""),comment.char="#")
capital=read.csv(paste(getwd(),"/iplan-portfolio.csv",sep=""),comment.char="#")
releases=read.csv(paste(getwd(),"/iplan-ciclos-entrega.csv",sep=""),comment.char="#")

portfolio<-loadPortfolio()

#l<-getRandomValidReleasePlan(portfolio)
#plot(l$releases[[1]],vertex.size=50,layout=layout_as_tree)
#plot(l$releases[[2]],vertex.size=50,layout=layout_as_tree)

l<-sampleValidReleasePlan(15)
#l<-c(l,sampleValidReleasePlan(1))
#plot(l[[2]]$releases[[1]],vertex.size=50,layout=layout_as_tree)

##################################################################################
v1<-c("CNC1","CNC2","CNC3","CNC4","CNC5","CNC6","CNC7","TSI1","TSI2","TSI3","TSI4","TSI5","TSI6","TSI1","TSI2","TSI3","TSI4","TSI5","TSI6","SAI1","SAI2","SAI3","SAI4","SAI5","SAI6", "HTV1","HTV2","HTV3","HTV4","HTV5")
v2<-NULL
for(i in 1:length(l)) {v2<-c(v2,l[[i]]$epicosEmRelease)}
t<-table(v2[v2 %in% v1])
t<-t[order(t)]
t
for(i in 1:length(l)) {
  if(grepl("TSI5",paste(l[[i]]$epicosEmRelease,collapse=""))) {
    print(paste(c("PLANO ",i," => ",l[[i]]$epicosEmRelease),collapse=","))
  }
}
for(i in 1:length(l)) {
  print(sort(l[[i]]$epicosEmRelease))
}
pevs$ciclos
##################################################################################
l<-sampleValidReleasePlan(15)
pevsDuplicados<-dataFramePlanosDeEntregaValidos(l)

#x<-sampleValidReleasePlan(1)

pevsSemDuplicados<-pevsDuplicados[!duplicated(pevsDuplicados[c("ciclos")]),]
nrow(pevsDuplicados)
nrow(pevsSemDuplicados)
#pevsSemDuplicados$id <- row.names(pevsSemDuplicados)
#pevs<-pevsSemDuplicados[pevsSemDuplicados$id %in% c(2,3,7,10,11,12,13,15),]
pevs<-pevsSemDuplicados
##########

xSample<-cbind(pevs$investimento)
ySample<-cbind(pevs$beneficiosTangiveis,pevs$beneficiosIntangiveis)

Benchmarking::dea.plot(pevs$investimento-10,pevs$beneficiosIntangiveis-0.5,RTS="vrs",txt=1:15)

Benchmarking::dea.plot(pevs$investimento-10,pevs$beneficiosTangiveis-10,RTS="vrs",txt=1:15)

returnToScale=1

###########
b1<-1/Benchmarking::dea(xSample,ySample,ORIENTATION="out",RTS="vrs")$eff
pevs1<-cbind(pevs,eficiencia=b1)
result1<-pevs1[with(pevs1, order(-eficiencia)), ]
result1
write.csv(result1, file = paste(getwd(),"/tese-exemplo-dea.csv",sep=""))
###########
bootstrapSamples=1000
b2<-FEAR::boot.sw98(t(xSample),t(ySample),NREP=bootstrapSamples,RTS=1,ORIENTATION=2,OUTPUT.FARRELL = T,CI.TYPE=4,alpha=0.1)
tab2 <- round(rbind(dhat=b2$dhat,
                    dhat.m=rowMeans(b2$boot),
                    bias=b2$bias,
                    dhat.bc=b2$dhat.bc,
                    ci.low=b2$conf.int[,1],
                    ci.high=b2$conf.int[,2])
              ,10)
eficienciaSW98<-1/tab2[4,]
icInferiorSW98<-1/tab2[6,]
icSuperiorSW98<-1/tab2[5,]

result2<-cbind(pevs1,eficienciaSW98,icSuperiorSW98,icInferiorSW98)
result2
k<-result2[with(result2, order(-eficienciaSW98)), ]
k$index <- as.numeric(row.names(k))
kk<-k[order(k$index), ]
cbind(inf=kk$icInferiorSW98,med=kk$eficienciaSW98,sup=kk$icSuperiorSW98)

pevs2<-cbind(pevs,eficienciaSW98,icSuperiorSW98,icInferiorSW98)
result2<-pevs2[with(pevs2, order(-eficienciaSW98)), ]
write.csv(result2, file = paste(getwd(),"/tese-exemplo-dea-bootstrap.csv",sep=""))
