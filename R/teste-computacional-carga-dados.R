for (n in seq(from = 1, to = 10, by =1)) {
#######################
# n<-45
# ciclosEntrega<-1
# duracaoCiclo<-10
#
n<-60
ciclosEntrega<-2
duracaoCiclo<-15
juro<-0.003 # A SELIC em Dez/17 foi 0,54% = 0,0054
juroBeneficioTangivel<-0.003
rm(list=setdiff(ls(), c("n","duracaoCiclo","ciclosEntrega","juro", "juroBeneficioTangivel")))
#######################
source("~/projetosR/iplan/R/iplan.R")
loginfo(paste("############ INICIANDO RODADA CONTENDO ",n," FUNCIONALIDADES ###############",sep = ""))
inicio.time <- Sys.time()

##################################################
epico.projeto<-NULL
prazo<-NULL
avaliacao<-NULL
projeto<-NULL
escala<-c("Insignificante","Pequena","Moderada","Alta","Extrema")

for (j in 1:3) {
  epico.projeto<-c(epico.projeto,"F")
  prazo<-c(prazo,paste0("C",j))
  avaliacao<-c(avaliacao,sample(escala,1,T))
  projeto<-c(projeto,"")
}

for (i in 1:n) {
  for (j in 1:3) {
    epico.projeto<-c(epico.projeto,paste0("f",i))
    prazo<-c(prazo,paste0("C",j))
    avaliacao<-c(avaliacao,sample(escala,1,T))
    projeto<-c(projeto,"F")
  }
}

iplan_beneficios_intangiveis_avaliacao <- data.frame(epico.projeto,prazo,avaliacao,projeto)

write.csv(iplan_beneficios_intangiveis_avaliacao,
          file = "~/Doutorado/iplan-teste-computacional/iplan-beneficios-intangiveis-avaliacao.csv",
          quote = FALSE,
          row.names=FALSE)
###################################################
Origem<-NULL
Destino<-NULL
#-------------------------------
anterior<-"begin"
for (j in seq(1,5,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",j))
  anterior<-paste0("f",j)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (j in seq(6,10,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",j))
  anterior<-paste0("f",j)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (j in seq(11,15,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",j))
  anterior<-paste0("f",j)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (j in seq(16,20,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",j))
  anterior<-paste0("f",j)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (j in seq(21,25,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",j))
  anterior<-paste0("f",j)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (j in seq(26,30,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",j))
  anterior<-paste0("f",j)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (k in seq(31,35,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",k))
  anterior<-paste0("f",k)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (k in seq(36,40,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",k))
  anterior<-paste0("f",k)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (k in seq(41,45,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",k))
  anterior<-paste0("f",k)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (k in seq(46,50,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",k))
  anterior<-paste0("f",k)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (k in seq(51,55,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",k))
  anterior<-paste0("f",k)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")

anterior<-"begin"
for (k in seq(56,60,1)) {
  Origem<-c(Origem,anterior)
  Destino<-c(Destino,paste0("f",k))
  anterior<-paste0("f",k)
}
Origem<-c(Origem,anterior)
Destino<-c(Destino,"end")
#-------------------------------
# for (i in 1:n) {
#  Origem<-c(Origem,"begin")
#  Destino<-c(Destino,paste0("f",i))
# }
# for (i in 1:n) {
#  Origem<-c(Origem,paste0("f",i))
#  Destino<-c(Destino,"end")
# }
#-------------------------------

iplan_precedencias <- data.frame(Origem,Destino)

write.csv(iplan_precedencias,
          file = "~/Doutorado/iplan-teste-computacional/iplan-precedencias.csv",
          quote = FALSE,
          row.names=FALSE)
###################################################
id<-NULL
beneficioTangivel<-NULL

for (i in 1:n) {
  id<-c(id,paste0("f",i))
  beneficioTangivel<-c(beneficioTangivel,sample(5:200,1))
}

iplan_beneficios_tangiveis <- data.frame(id,beneficioTangivel)
#sum(iplan_beneficios_tangiveis$beneficioTangivel)

#write.csv(iplan_beneficios_tangiveis,
#          file = "~/Doutorado/iplan-teste-computacional/iplan-beneficios-tangiveis.csv",
#          quote = FALSE,
#          row.names=FALSE)
###################################################
id<-"begin"
duracao<-0
investimento<-0

soma<-0
for (i in 1:n) {
  id<-c(id,paste0("f",i))
  duracao<-c(duracao,sample(1:8,1,prob=c(0.05  ,  0.2  ,  0.4  ,  0.6  ,  0.4  ,  0.2  ,  0.05  ,  0.05  )))
  invest<-sample(1:50,1)
  soma<-soma+invest
  investimento<-c(investimento,invest)
}

id<-c(id,"end")
duracao<-c(duracao,"0")
investimento<-c(investimento,"0")

iplan_funcionalidades <- data.frame(id,duracao,investimento)

write.csv(iplan_funcionalidades,
          file = "~/Doutorado/iplan-teste-computacional/iplan-funcionalidades.csv",
          quote = FALSE,
          row.names=FALSE)
####################################################
id<-NULL
duracao<-NULL
for (i in 1:ciclosEntrega) {
  id<-c(id,paste0("CE",i))
  duracao<-c(duracao,duracaoCiclo)
}

iplan_ciclos_entrega <- data.frame(id,duracao)

write.csv(iplan_ciclos_entrega,
          file = "~/Doutorado/iplan-teste-computacional/iplan-ciclos-entrega.csv",
          quote = FALSE,
          row.names=FALSE)
####################################################
 capital<-soma
 iplan_portfolio <- data.frame(capital,juro,juroBeneficioTangivel)
 write.csv(iplan_portfolio,
           file = "~/Doutorado/iplan-teste-computacional/iplan-portfolio.csv",
           quote = FALSE,
           row.names=FALSE)
####################################################
getwd()
setwd("~/Doutorado/iplan-teste-computacional/")
getwd()
edges=read.csv("~/Doutorado/iplan-teste-computacional/iplan-precedencias.csv",comment.char="#")
vertices=read.csv("~/Doutorado/iplan-teste-computacional/iplan-funcionalidades.csv",comment.char="#")
capital=read.csv("~/Doutorado/iplan-teste-computacional/iplan-portfolio.csv",comment.char="#")
releases=read.csv("~/Doutorado/iplan-teste-computacional/iplan-ciclos-entrega.csv",comment.char="#")

#portfolio<-loadPortfolio(vertices = vertices, capital = capital, releases = releases, edges = edges)

#getRandomValidReleasePlan(portfolio )

l<-enumerateAllPEVs() #Esse método está gerando planos duplicados! Pode ser que tenha processamento desnecessário!
#l<-enumerateAllPEVsV2() #Essa é a versão para usar com o fatorial (toda as combinacoes possiveis de planos de entrega)

if(length(l)==0){
  loginfo(paste("Nao vamos calcular o DEA pois deu mais de 253.000 planos...",sep = ""))
}else{
  loginfo(paste("Calculando o DEA...",sep = ""))
  
  pevs<-dataFramePlanosDeEntregaValidos(l)
  #pevsSemDuplicados<-pevsDuplicados[!duplicated(pevsDuplicados[c("ciclos")]),]
  #nrow(pevsDuplicados)
  #nrow(pevsSemDuplicados)
  #pevs<-pevsSemDuplicados
  
  #pevs
  
  xSample<-cbind(pevs$investimento)
  ySample<-cbind(pevs$beneficiosTangiveis,pevs$beneficiosIntangiveis)
  #ySample<-cbind(pevs$beneficiosTangiveis)
  
  #b1<-1/Benchmarking::dea(xSample,ySample,ORIENTATION="out",RTS="vrs")$eff
  b1<-FEAR::dea(t(xSample),t(ySample),RTS=1,ORIENTATION=2)
  ####b1<-FEAR::boot.sw98(t(xSample),t(ySample),NREP=1,RTS=1,ORIENTATION=2,alpha=0.1,OUTPUT.FARRELL = F)
  
  
  pevs1<-cbind(pevs,eficiencia=b1)
  result1<-pevs1[with(pevs1, order(-eficiencia)), ]
  
  fim.time <- Sys.time()
  time.taken <- fim.time - inicio.time
  
  write.csv(result1, file = paste(getwd(),"/avaliacao-usando-dea-",n,"-funcionalidades-",inicio.time,".csv",sep=""))
  #result1[16365,]
  
  inicio.time
  fim.time
  loginfo(paste("############ O TEMPO para ",n," FUNCIONALIDADES (PLANOS ",nrow(pevs),") FOI ",time.taken," ###############",sep = ""))
}


}



###########################################################################
#   BOOSTRAP
###########################################################################
inicio.time <- Sys.time()
##########
#l2<-sampleValidReleasePlan(3000)
#pevsDuplicados<-dataFramePlanosDeEntregaValidos(l2)
#pevsSemDuplicados<-pevsDuplicados[!duplicated(pevsDuplicados[c("ciclos")]),]
#nrow(pevsDuplicados)
#nrow(pevsSemDuplicados)
#pevs2<-pevsSemDuplicados
##########
#pevsDuplicados<-NULL
#pevsSemDuplicados<-data.frame()
#while(nrow(pevsSemDuplicados)<100){
#  loginfo(paste("Amostra aleatoria está com ",nrow(pevsSemDuplicados)," planos de entrega",sep = ""))
#  l2<-sampleValidReleasePlan(50)
#  pev<-dataFramePlanosDeEntregaValidos(l2)
#  pevsDuplicados<-rbind(pevsDuplicados,pev)
#  pevsSemDuplicados<-pevsDuplicados[!duplicated(pevsDuplicados[c("ciclos")]),]
#}
#pevs2<-pevsSemDuplicados
##########
tamanhoDaAmostra<-2000
amostraIDs<-sample(seq(1,length(l),1),tamanhoDaAmostra)
pevs2<-pevs[amostraIDs,]
xSample2<-cbind(pevs2$investimento)
ySample2<-cbind(pevs2$beneficiosTangiveis,pevs2$beneficiosIntangiveis)

#xSample2<-cbind(pevs2$investimento)
#ySample2<-cbind(pevs2$beneficiosTangiveis,pevs2$beneficiosIntangiveis)

bootstrapSamples=1000
b2<-FEAR::boot.sw98(t(xSample2),t(ySample2),NREP=bootstrapSamples,RTS=1,ORIENTATION=2,OUTPUT.FARRELL = T,CI.TYPE=4,alpha=0.1)
loginfo(paste("Terminou o boot.sw98",sep = ""))
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

result2<-cbind(pevs2,eficienciaSW98,icSuperiorSW98,icInferiorSW98)
#result2
k<-result2[with(result2, order(-eficienciaSW98)), ]
k$index <- as.numeric(row.names(k))
kk<-k[order(k$index), ]
cbind(inf=kk$icInferiorSW98,med=kk$eficienciaSW98,sup=kk$icSuperiorSW98)

pevs2<-cbind(pevs2,eficienciaSW98,icSuperiorSW98,icInferiorSW98)
result2<-pevs2[with(pevs2, order(-eficienciaSW98)), ]
write.csv(result2, file = paste(getwd(),"/avaliacao-usando-dea-bootstrap.csv",sep=""))

fim.time <- Sys.time()
time.taken <- fim.time - inicio.time
#inicio.time
#fim.time
loginfo(paste("############ O TEMPO para calcular o Boostrap FOI ",time.taken," ###############",sep = ""))

}

###########################
precedencia=read.csv("~/Doutorado/iplan-teste-computacional/iplan-precedencias.csv",comment.char="#")
colnames(precedencia) <- c("predecessor", "id")
precedencia$predecessor[precedencia$predecessor=="begin"]<-NA
#precedencia$sucessor<-substring(precedencia$sucessor, 2)
tangiveis=read.csv(paste(getwd(),"/iplan-beneficios-tangiveis.csv",sep=""),comment.char="#")
tangiveis$beneficioTangivel<-tangiveis$beneficioTangivel*0.1
duracaoEinvestimento=head(vertices[-c(1),],-1)
duracaoEinvestimento$investimento<-duracaoEinvestimento$investimento*0.1
intangiveis=read.csv(paste(getwd(),"/AHP-resultado-intangiveis.csv",sep=""),comment.char="#")
colnames(intangiveis) <- c("id", "beneficioIntangivel")
intangiveis$beneficioIntangivel<-sapply(intangiveis$beneficioIntangivel*100, round, 2)
df<-merge(duracaoEinvestimento,tangiveis,by = "id")
df<-merge(df,intangiveis,by="id") 
df<-merge(df,precedencia,by="id") 
df$ordem<-substring(df$id, 2)
df[, c(7)] <- sapply(df[, c(7)], as.numeric)
df<-df[order(df$ordem),]
df$ordem<-NULL
df
write.csv2(df, file = paste(getwd(),"/dados-funcionalidades.csv",sep=""), row.names=FALSE, quote = FALSE,na="")

planos<-head(result1,21)
library(stringr)
p<-str_split_fixed(planos$ciclos, ".  CE2: ", 2)
ce1<-substring(p[,1],13)
ce2<-str_split_fixed(p[,2], "\\.", 2)
ce2<-ce2[,1]
ce<-data.frame(id=rownames(planos),um=ce1,dois=ce2)
write.csv2(ce, file = paste(getwd(),"/dados-planos.csv",sep=""), row.names=FALSE, quote = FALSE,na="")

