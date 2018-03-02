#iPlan.R - Programa principal do pacote R.
#Autor: Marcelo Carvalho Fernandes.
#Versão: 1.1.1.
#Data: 2 de Março de 2017.
#rm(list = ls())
library(igraph)
library(ahp)
library(data.tree)
library(ggplot2)
library(gridExtra)
library(logging)
library(gtools)

basicConfig()
addHandler(writeToFile, file="~/iplan.log")

setwd("~/Doutorado/iplan-teste-computacional/")
edges=read.csv(paste(getwd(),"/iplan-precedencias.csv",sep=""),comment.char="#")
vertices=read.csv(paste(getwd(),"/iplan-funcionalidades.csv",sep=""),comment.char="#")
capital=read.csv(paste(getwd(),"/iplan-portfolio.csv",sep=""),comment.char="#")
releases=read.csv(paste(getwd(),"/iplan-ciclos-entrega.csv",sep=""),comment.char="#")


getRandomValidReleasePlan <- function(p) {
  portfolio<-p
  portfolio$candidateEpics <- as.character(edges[edges$Origem=="begin",]$Destino)
  portfolio<-addEpicToRelease(portfolio, "begin", 1)
  portfolio$waitingListEpics <- list()
  countRelease <- 0
  for( ce in portfolio$releases){
    countRelease <- countRelease + 1
    loginfo(paste("      Gerando o ciclo de entrega ",countRelease,"...",sep = ""))
    while(length(portfolio$candidateEpics)>0){
      ep <- sample(portfolio$candidateEpics,1)[[1]]
      if(isEpicElegibleToStart(ep, portfolio)){
        if (isEpicElegibleToRelease(portfolio, ep,countRelease)){
          portfolio<-addEpicToRelease(portfolio, ep, countRelease)
        }else if (!isEpicInComplianceToPortfolioInvestmentCapital(portfolio, ep,countRelease)){
          portfolio$candidateEpics<-discardEpic(portfolio$candidateEpics, ep)
        }else if (!isEpicInComplianceToReleaseSchedule(portfolio, ep,countRelease)){
          if(countRelease<length(portfolio)) #desde que ainda haja um proximo "ce"
            portfolio<-flagEpicToNextRelease(portfolio, ep)
          else
            portfolio$candidateEpics<-discardEpic(portfolio$candidateEpics, ep)
        }
      }else{
        portfolio$candidateEpics<-discardEpic(portfolio$candidateEpics, ep)
      }
    } #while(length(portfolio$candidateEpics)>0){
    portfolio<-tryFlaggedEpicsInNextRelease(portfolio)
    portfolio<-createEpicPrecedenceInRelease(portfolio,countRelease)
  } #for( ce in portfolio)
  #browser()
  portfolio
}

createEpicPrecedenceInRelease<- function(portfolio,countRelease){
  if(length(portfolio$releases)==countRelease) # Na verdade aqui só deveriamos incluir o "end" se todos os predecessores dele estivessem no grafo da (ultima) release
    portfolio<-addEpicToRelease(portfolio, "end", countRelease)
  attributes<-graph_attr(portfolio$releases[[countRelease]])
  portfolio$releases[[countRelease]]<-induced_subgraph(portfolio$precedenceGraph,V(portfolio$releases[[countRelease]])$name)
  graph_attr(portfolio$releases[[countRelease]])<-attributes
  portfolio
}

isEpicElegibleToStart <- function(ep, portfolio) {
  antecessoresEP<-as.character(edges[edges$Destino==ep,]$Origem)
  intersecao<-intersect(portfolio$epicosEmRelease,antecessoresEP)
  (length(intersect(intersecao,antecessoresEP))==length(antecessoresEP))
}

isEpicElegibleToRelease <- function(portfolio, ep,countRelease) {
  (isEpicInComplianceToReleaseSchedule(portfolio, ep, countRelease) & isEpicInComplianceToPortfolioInvestmentCapital(portfolio, ep, countRelease))
}

isEpicInComplianceToReleaseSchedule <- function(portfolio, ep,countRelease) {
  (releases$duracao[countRelease] >= graph_attr(portfolio$releases[[countRelease]],"duracaoAtualEmSemanas")+vertices[vertices$id==ep,]$duracao)
}

isEpicInComplianceToPortfolioInvestmentCapital <- function(portfolio, ep,countRelease) {
  capital$capital >= vertices[vertices$id==ep,]$investimento
}

duracaoTotalPortfolio<-function(portfolio){
  soma<-0
  for(i in 1:length(portfolio$releases)){
    soma<-soma+graph_attr(portfolio$releases[[i]],"duracaoPlanejadaEmSemanas")
  }
  soma
}



addEpicToRelease <- function(portfolio, ep, countRelease,sampleMode=T) { #when sampleMode=F, enumerateAllPEVs
  if(sampleMode) portfolio$candidateEpics<-discardEpic(portfolio$candidateEpics, ep)
  ce <- portfolio$releases[[countRelease]] + vertices(ep)
  portfolio$epicosEmRelease<-c(portfolio$epicosEmRelease,ep)
  graph_attr(ce,"duracaoAtualEmSemanas") <- graph_attr(ce,"duracaoAtualEmSemanas")+vertices[vertices$id==ep,]$duracao #vertex_attr(portfolio$precedenceGraph, "duracao", ep)
  juro<-graph_attr(portfolio$precedenceGraph, "juro")
  graph_attr(portfolio$precedenceGraph,"capitalAtual") <- as.numeric(graph_attr(portfolio$precedenceGraph,"capitalAtual"))+vertices[vertices$id==ep,]$investimento
  graph_attr(portfolio$precedenceGraph,"capitalDisponivel")<-(graph_attr(portfolio$precedenceGraph,"capitalDisponivel")*(1+juro)^countRelease)-vertices[vertices$id==ep,]$investimento
  portfolio$releases[[countRelease]]<-ce
  if(sampleMode) portfolio$candidateEpics<-addNextEpicsAsCandidates(portfolio, ep)
  portfolio
}

discardEpic <- function(candidateEpics, ep) {
  if(length(candidateEpics)>1)
    candidateEpics <- candidateEpics[candidateEpics != ep]
  else
    candidateEpics <- list()
  candidateEpics
}

flagEpicToNextRelease <- function(portfolio, ep) {
  portfolio$waitingListEpics[length(portfolio$waitingListEpics)+1] <- ep
  portfolio$candidateEpics<-discardEpic(portfolio$candidateEpics,ep)
  portfolio
}

tryFlaggedEpicsInNextRelease <- function(portfolio) {
  candidateEpics <- unique(c(portfolio$candidateEpics,portfolio$waitingListEpics))
  portfolio$waitingListEpics <- list()
  portfolio$candidateEpics<-candidateEpics
  portfolio
}

addNextEpicsAsCandidates <- function(portfolio, ep) {
  candidateEpics<-list()
  sucessores<-as.character(edges[edges$Origem==ep,]$Destino)
  for (s in sucessores) {
    if(isEpicElegibleToStart(s,portfolio))
      candidateEpics<-c(candidateEpics,s)
  }
  unique(c(portfolio$candidateEpics,candidateEpics))
}

loadPortfolio <- function(vertices=read.csv(paste(getwd(),"/iplan-funcionalidades.csv",sep=""),comment.char="#"),
                          edges=read.csv(paste(getwd(),"/iplan-precedencias.csv",sep=""),comment.char="#"),
                          capital=read.csv(paste(getwd(),"/iplan-portfolio.csv",sep=""),comment.char="#"),
                          releases=read.csv(paste(getwd(),"/iplan-ciclos-entrega.csv",sep=""),comment.char="#")) {
  portfolio<-NULL
  g<-graph.data.frame(
    edges,
    directed = TRUE,
    vertices=vertices
  )
  graph_attr(g, "capitalDisponivel") <- as.numeric(capital[1,"capital"])
  graph_attr(g, "juro") <- as.numeric(capital[1,"juro"])
  graph_attr(g, "juroBeneficioTangivel") <- as.numeric(capital[1,"juroBeneficioTangivel"])
  graph_attr(g, "capitalAtual") <- 0
  createReleasesAsGraphs <- function(release){
    ce <- make_empty_graph(n = 0, directed = TRUE)
    graph_attr(ce, "nome") <- release["id"][[1]]
    graph_attr(ce, "duracaoAtualEmSemanas") <- 0
    graph_attr(ce, "duracaoPlanejadaEmSemanas") <- as.numeric(release["duracao"][[1]])
    ce
  }
  portfolio$precedenceGraph<-g
  portfolio$releases <- apply(releases,1,createReleasesAsGraphs)
  portfolio

}

sampleValidReleasePlan <- function(numberOfSamples=1){
  portfolio<-loadPortfolio()
  l <- list()
  for (i in 1:numberOfSamples) {
    loginfo(paste("Gerando a observacao ",i,"...",sep = ""))
    l[[length(l)+1]]<-getRandomValidReleasePlan(portfolio)
  }
  l
}

dataFramePlanosDeEntregaValidos<-function(l=sampleValidReleasePlan(1)){
  getStrCiclosEntrega<-function(pev){
    ces<-""
    for (i in 1:length(pev$releases)) {
      nomeCiclo<-pev$releases[[i]]$nome
      vertices<-paste(V(pev$releases[[i]])$name,collapse = ", ")
      vertices<-paste(vertices,". ", sep = "")
      ce<-paste(nomeCiclo,vertices,sep = ": ")
      ces<-paste(ces,ce)
    }
    gsub("^\\s+|\\s+$", "", ces)
  }

  ciclos<-unlist(lapply(l,getStrCiclosEntrega))

  getInvestimento<-function(pev){
    pev$precedenceGraph$capitalAtual
  }

  investimento<-unlist(lapply(l,getInvestimento))

  relevanciaIntangiveis<-calculaAHPabsoluto(beneficios="intangiveis")
  write.csv(relevanciaIntangiveis,
            file = "~/Doutorado/iplan-teste-computacional/AHP-resultado-intangiveis.csv",
            quote = FALSE,
            row.names=FALSE)
  getBeneficios<-function(pev,relevanciaBeneficios){
    vertices<-NULL
    for (i in 1:length(pev$releases)) {
      vertices<-c(vertices,V(pev$releases[[i]])$name)
    }
    epicos<-vertices[!vertices %in% c("begin","end")]
    x<-cbind(relevanciaBeneficios,epico=as.character(relevanciaBeneficios$epico.projeto))
    sum(subset(x,epico%in%epicos,select=relevanciaNormalizada))
  }
  getBeneficios2<-function(pev,relevanciaBeneficios){
    #pev<-l[[1]]
    #relevanciaBeneficios<-relevanciaIntangiveis
    soma<-0
    juro<-graph_attr(pev$precedenceGraph, "juroBeneficioTangivel")
    #convert de factor para string...
    x<-cbind(relevanciaBeneficios,epico=as.character(relevanciaBeneficios$epico.projeto))
    #loginfo(paste("==================== PARA UM PLANO ========================",sep = ""))
    for (countRelease in 1:length(pev$releases)) {
      #countRelease<-3
      vertices<-V(pev$releases[[countRelease]])$name
      epicos<-vertices[!vertices %in% c("begin","end")]
      if(length(epicos)==0){
        break
      }
      if(juro==0){
        soma<-sum(subset(x,epico%in%epicos,select=relevanciaNormalizada))
      }else{
        soma<-soma+sum(subset(x,epico%in%epicos,select=relevanciaNormalizada)*(1-juro)^countRelease)
      }
      #loginfo(paste("Epico ",epicos," no ciclo ",countRelease," sem juro ",sum(subset(x,epico%in%epicos,select=relevanciaNormalizada))," com juro ",sum(subset(x,epico%in%epicos,select=relevanciaNormalizada)*(1-juro)^countRelease),sep = ""))
      #loginfo(paste("SOMA=",soma,sep = ""))
    }
    soma
  }
  intang<-unlist(lapply(l,getBeneficios,relevanciaIntangiveis))

  tangiveis=read.csv(paste(getwd(),"/iplan-beneficios-tangiveis.csv",sep=""),comment.char="#")
  colnames(tangiveis)<-c("epico.projeto","relevanciaNormalizada")
  tang<-unlist(lapply(l,getBeneficios2,tangiveis))

  pevs<-data.frame(ciclos,investimento,beneficiosIntangiveis=intang,beneficiosTangiveis=tang)
  pevs[with(pevs, order(-investimento)), ]

}

calculaAHPabsoluto<-function(beneficios="intangiveis"){
  ############################################################################################
  arquivo<-paste("/iplan-beneficios-",beneficios,"-escala-avaliacao",sep = "")
  ahp <- Load(paste(getwd(),arquivo,".ahp",sep=""))
  Calculate(ahp)
  resultado<-Analyze(ahp)
  coluna<-ncol(resultado[1,])
  relevanciaEscala<-t(resultado[1,][,c(-1,-2,-coluna)])
  #hierarquia<-Visualize(ahp)
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),arquivo,".png",sep=""), height=40*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()
  ############################################################################################
  arquivo<-paste("/iplan-beneficios-",beneficios,"-criterios",sep = "")
  ahp <- Load(paste(getwd(),arquivo,".ahp",sep=""))
  Calculate(ahp)
  resultado<-Analyze(ahp)
  coluna<-ncol(resultado[1,])
  relevanciaPrazo<-t(resultado[1,][,c(-1,-2,-coluna)])
  #hierarquia<-Visualize(ahp)
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),arquivo,".png",sep=""), height=40*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()
  ############################################################################################
  names <- rownames(relevanciaPrazo)
  rownames(relevanciaPrazo) <- NULL
  relevanciaPrazo <- cbind(names,relevanciaPrazo)
  relevanciaPrazo<-data.frame(relevanciaPrazo)

  names <- rownames(relevanciaEscala)
  rownames(relevanciaEscala) <- NULL
  relevanciaEscala <- cbind(names,relevanciaEscala)
  relevanciaEscala<-data.frame(relevanciaEscala)
  ############################################################################################
  volumeInvestimento<-read.csv(paste(getwd(),"/iplan-beneficios-",beneficios,"-avaliacao.csv",sep=""),comment.char="#")

  avaliacoesApenasProjetos<-volumeInvestimento #subset(volumeInvestimento,projeto!="")

  x<-relevanciaEscala[match(avaliacoesApenasProjetos$avaliacao,relevanciaEscala$names),]
  avaliacoesApenasProjetos<-cbind(avaliacoesApenasProjetos,relevanciaEscala=x[,2])

  x<-relevanciaPrazo[match(avaliacoesApenasProjetos$prazo,relevanciaPrazo$names),]
  avaliacoesApenasProjetos<-cbind(avaliacoesApenasProjetos,relevanciaPrazo=x[,2])

  avaliacoesApenasProjetos<-cbind(avaliacoesApenasProjetos,
                                  relevanciaAvalicaoXPrazo=as.numeric(as.character(avaliacoesApenasProjetos$relevanciaEscala))*as.numeric(as.character(avaliacoesApenasProjetos$relevanciaPrazo)))

  relevancias<-aggregate(avaliacoesApenasProjetos$relevanciaAvalicaoXPrazo,
                         by=list(epico.projeto=avaliacoesApenasProjetos$epico.projeto,
                                 projeto=avaliacoesApenasProjetos$projeto),
                         FUN=sum)
  #Nommalizações
  consideraProjeto=F
  if(!consideraProjeto){
    relevanciaNormalizadaEpico<-data.frame(projeto=subset(relevancias,projeto!="",select = epico.projeto),
                                           relevanciaNormalizada=subset(relevancias,projeto!="")[,3]/sum(subset(relevancias,projeto!="")[,3]))
    relevanciaNormalizadaEpico
  }else{
    relevanciaNormalizadaProjetos<-data.frame(projeto=subset(relevancias,projeto=="",select = epico.projeto),
                                              relevanciaNormalizada=subset(relevancias,projeto=="")[,3]/sum(subset(relevancias,projeto=="")[,3]))
    projetos<-c(t(subset(relevancias,projeto=="",select = epico.projeto)))
    relevanciaEpicosNoPortfolio<-NULL
    for (p in projetos) {
      relevanciaNormalizadaEpico<-data.frame(projeto=subset(relevancias,projeto==p,select = epico.projeto),
                                             relevanciaNormalizada=subset(relevancias,projeto==p)[,3]/sum(subset(relevancias,projeto==p)[,3]))
      relevanciaDoProjeto<-subset(relevanciaNormalizadaProjetos,epico.projeto==p,select=relevanciaNormalizada)[1,1]
      relevanciaNormalizadaEpico$relevanciaNormalizada<-relevanciaNormalizadaEpico$relevanciaNormalizada*relevanciaDoProjeto
      relevanciaEpicosNoPortfolio<-rbind(relevanciaEpicosNoPortfolio,relevanciaNormalizadaEpico)
    }
    relevanciaEpicosNoPortfolio
  }
}


iplan<-function(enumerateAll=T,originalSamples=40,bootstrapSamples=1000,returnToScale=1,efficiencyOrientation=1){
  if(enumerateAll)
    l<-enumerateAllPEVs()
  else
    l<-sampleValidReleasePlan(originalSamples)
  pevs<-dataFramePlanosDeEntregaValidos(l)

  xSample<-cbind(pevs$investimento)
  ySample<-cbind(pevs$beneficiosTangiveis,pevs$beneficiosIntangiveis)

  b<-FEAR::boot.sw98(t(xSample),t(ySample),NREP=bootstrapSamples,RTS=returnToScale,ORIENTATION=efficiencyOrientation,alpha=0.1,OUTPUT.FARRELL = T)

  tab2 <- round(rbind(dhat=b$dhat,
                      dhat.m=rowMeans(b$boot),
                      bias=b$bias,
                      dhat.bc=b$dhat.bc,
                      ci.low=b$conf.int[,1],
                      ci.high=b$conf.int[,2])
                ,10)

  if(enumerateAll){
    eficienciaSW98<-1/tab2[1,]
  }else{
    eficienciaSW98<-1/tab2[4,]
  }
  icInferiorSW98<-1/tab2[6,]
  icSuperiorSW98<-1/tab2[5,]

  pevs<-cbind(pevs,eficienciaSW98,icSuperiorSW98,icInferiorSW98)
  pevs[with(pevs, order(-eficienciaSW98)), ]
}

enumerateAllPEVsV2<-function(){
  portfolio<-loadPortfolio()
  ciclos<-length(portfolio$releases)
  funcionalidades<-V(portfolio$precedenceGraph)$name
  funcionalidades<-funcionalidades[2:(length(funcionalidades)-1)]
  qtdCiclos<-length(portfolio$releases)
  allPEVs<-permutations(n=length(funcionalidades),r=qtdCiclos,v=funcionalidades)
  len<-nrow(allPEVs)
  loginfo(paste("Total de planos de entrega ",len,sep = ""))
  loginfo(paste(c("Construindo o array de planos para retona-lo como resultado e poder chamar iplan."), collapse = " "))
  start.time <- Sys.time()
  l <- list()
  for(i in 1:len){
    p<-portfolio
    countRelease<-1
    for(epico in allPEVs[i,]){
      p<-addEpicToRelease(p, epico, countRelease,sampleMode = F)
      countRelease<-countRelease+1
    }
    l[[length(l)+1]]<-p
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  loginfo(paste(c("Construcao concluida.", time.taken), collapse = " "))
  l
}

enumerateAllPEVs<-function(){
  loginfo("Enumeracao completa dos planos de entrega.")
  start.time <- Sys.time()
  portfolio<-loadPortfolio()
  portfolio2<-portfolio
  pevsFinais<-NULL
  pevs<-NULL
  duracao<-0
  investimento<-0
  planosUnique<-NULL
  capitalDisp<-graph_attr(portfolio$precedenceGraph, "capitalDisponivel")
  pevs[[1]]<-list(list("begin"),duracao,investimento,capitalDisp)
  pevsNaoVerificados<-list()
  pevsNaoVerificados[length(pevsNaoVerificados)+1]<-1
  gerouNovoPEV<-F
  for (qtdCiclos in 1:length(portfolio$releases)) {
    loginfo(paste("Enumerando o ciclo de entrega ",qtdCiclos,sep = ""))
    graph_attr(portfolio$releases[[qtdCiclos]],"duracaoAtualEmSemanas")<-0
    #qtdCiclos=2
    if(qtdCiclos>1){
      pevs<-pevs[pevsFinais]
      pevsNaoVerificados<-1:length(pevs)
      pevsFinais<-NULL
      for(i in 1:length(pevs)){
        pevs[[i]][[2]]<-0
      }
    }
    while(length(pevsNaoVerificados)>0){
      p<-pevs[[pevsNaoVerificados[[1]]]]
      portfolio$epicosEmRelease<-unlist(p[[1]])
      graph_attr(portfolio$releases[[qtdCiclos]],"duracaoAtualEmSemanas")<-p[[2]]
      graph_attr(portfolio$precedenceGraph,"capitalAtual")<-p[[3]]
      graph_attr(portfolio$precedenceGraph, "capitalDisponivel")<-p[[4]]
      candidatos<-NULL
      for (ep in portfolio$epicosEmRelease) {
        candidatos<-c(candidatos,as.character(edges[edges$Origem==ep,]$Destino))
      }
      remover<-intersect(candidatos,portfolio$epicosEmRelease)
      for(r in remover)
        candidatos<-unlist(discardEpic(as.list(candidatos),r))
      candidatos<-unique(candidatos)
      gerouNovoPEV<-F
      for(epico in candidatos){
        epicos<-list(c(unlist(p[[1]]),epico))
        epicosOrdenados<-paste(sort(unlist(epicos)), collapse=" ")
        if(epicosOrdenados %in% planosUnique) next
        if(isEpicElegibleToStart(epico,portfolio)){
          if(isEpicElegibleToRelease(portfolio,epico,qtdCiclos)){
            duracao<-p[[2]]+vertices[vertices$id==epico,]$duracao
            juro<-graph_attr(portfolio$precedenceGraph, "juro")
            capitalDisp<-(p[[4]]*(1+juro)^qtdCiclos)-(vertices[vertices$id==epico,]$investimento)
            investimento<-p[[3]]+vertices[vertices$id==epico,]$investimento
            pNovo<-list(epicos,duracao,investimento,capitalDisp)
            i<-length(pevs)+1
            pevs[[i]]<-pNovo
            planosUnique<-c(planosUnique,epicosOrdenados)
            pevsNaoVerificados[length(pevsNaoVerificados)+1]<-i
            gerouNovoPEV<-T
          }
        }
      }
      if(!gerouNovoPEV){ # se p nao gerou nenhum novo, ele é final
        if(length(pevsNaoVerificados)>0){
          pevsFinais<-c(pevsFinais,pevsNaoVerificados[[1]])
          loginfo(paste("Plano ",paste(unlist(p[[1]]),collapse = ",")," eh final dentre ",length(pevsFinais)," planos.",sep = ""))
        }else{
          #loginfo("Nao tem mais nenhum nao verificado nesse ciclo. Vamos para outro?")
        }
      }else{
        #loginfo(paste("Existem ",length(pevsNaoVerificados)," a verificar e ",length(pevsFinais)," finais apos a analise de ",paste(unlist(p[[1]]),collapse = ","),"...",sep = ""))  
      }
      pevsNaoVerificados<-pevsNaoVerificados[-1]
    }
  }
  allPEVs<-pevs[pevsFinais]
  len<-length(allPEVs)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  loginfo(paste(c("O tempo de processamento da geracao dos planos foi ",time.taken), collapse=" "))
  loginfo(paste(c("Construindo o array de planos para retona-lo como resultado e poder chamar iplan."), collapse = " "))
  start.time <- Sys.time()
  l <- list()
  loginfo(paste(c("Total de planos de entrega:",len), collapse=" "))
  if(len>253000){
    loginfo(paste("Abortei essa execucao pois tem mais de 253.000 planos", collapse=" "))
  }else{
    for(i in 1:len){
      p<-portfolio2 #recuperando o portfolio original
      countRelease<-1
      for(epico in allPEVs[[i]][[1]][[1]]){
        if(!isEpicElegibleToRelease(p,epico,countRelease)){
          if(!(epico=="begin"||epico=="end")){
            countRelease<-countRelease+1
          }
          #loginfo(paste(c("Somei 1 ao countRelease que agora é ",countRelease), collapse=" "))
        }
        p<-addEpicToRelease(p, epico, countRelease)
      }
      l[[length(l)+1]]<-p
    }
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  loginfo(paste(c("Construcao concluida.", time.taken), collapse = " "))
  l
}

######################################################
#iplan(enumerateAll=FALSE,
#      originalSamples=5,bootstrapSamples=1000,
#      returnToScale=1,efficiencyOrientation=1)

#getwd()

#setwd("/Users/mcf/")

#vrp<-getRandomValidReleasePlan()
#plot(vrp$precedenceGraph)
#plot(vrp$releases[[1]],vertex.size=70,layout=layout_as_tree)
#plot(vrp$releases[[2]],vertex.size=70,layout=layout_as_tree)
#plot(vrp$precedenceGraph,vertex.size=50,layout=layout_as_tree)

#l<-sampleValidReleasePlan(10)
#plot(l[[3]]$releases[[1]],vertex.size=70,layout=layout_as_tree)



#graph_attr(l[[2]]$releases[[2]], "duracaoAtualEmSemanas")
#graph_attr(l[[2]]$releases[[2]], "duracaoPlanejadaEmSemanas")
