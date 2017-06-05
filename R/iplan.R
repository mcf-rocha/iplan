#iPlan.R - Programa principal do pacote R.
#Autor: Marcelo Carvalho Fernandes.
#Versão: 1.1.1.
#Data: 2 de Março de 2017.

library(igraph)
library(ahp)
library(data.tree)
library(ggplot2)
library(gridExtra)


getRandomValidReleasePlan <- function(p) {
  portfolio<-p
  #2 - Incializar o conjunto candidateEpics (Épicos Candidatos, não iniciados) contendo os vizinhos de "begin"
  #portfolio$candidateEpics <- c("begin",attr(ego(portfolio$precedenceGraph, 1, nodes = "begin", mode=c("out"), mindist = 1)[[1]],"names"))

  portfolio$candidateEpics <- attr(ego(portfolio$precedenceGraph, 1, nodes = "begin", mode=c("out"), mindist = 1)[[1]],"names")
  portfolio<-addEpicToRelease(portfolio, "begin", 1)

  portfolio$waitingListEpics <- list()

  countRelease <- 0

  #3 - Enquanto for possível obter "ce", o próximo Ciclo de Entrega em portfolio
  for( ce in portfolio$releases){
    countRelease <- countRelease + 1
    #3.1 - Enquanto for possível obter "ep", ao acaso, a partir de candidateEpics
    while(length(portfolio$candidateEpics)>0){
      ep <- sample(portfolio$candidateEpics,1)[[1]]
      #3.1.1 - Se "ep" pode ser inciado (ele não possui antecessores em portfolio$candidateEpics)
      if(isEpicElegibleToStart(ep, portfolio)){
        #3.1.1.1 - Se ele não viola a duração planejada de "ce" e nem o capital total do portfolio
        if (isEpicElegibleToRelease(portfolio, ep,countRelease)){
          portfolio<-addEpicToRelease(portfolio, ep, countRelease)
        #3.1.1.2 - Se "ep" viola o capital total do portfolio
        }else if (!isEpicInComplianceToPortfolioInvestmentCapital(portfolio, ep)){
          #3.1.1.3.1 - Retiro "ep" de portfolio$candidateEpics
          portfolio$candidateEpics<-discardEpic(portfolio$candidateEpics, ep)
        #3.1.1.3 - Se "ep" viola a duração de "ce"
        }else if (!isEpicInComplianceToReleaseSchedule(portfolio, ep,countRelease)){
          #3.1.1.3.1 - Adiciono "ep" ao conjunto de EPA (Épicos Analisados)
          if(countRelease<length(portfolio)) #desde que ainda haja um proximo "ce"
            portfolio<-flagEpicToNextRelease(portfolio, ep)
          else #se for o ultimo "ce" vamos descartar
            portfolio$candidateEpics<-discardEpic(portfolio$candidateEpics, ep)
        }
      #3.1.2 - Caso constrário ("ep" não pode ser iniciado pois possui antecessores não inciados)
      }else{
        #3.1.2.1 - Retiro "ep" de portfolio$candidateEpics
        portfolio$candidateEpics<-discardEpic(portfolio$candidateEpics, ep)
      }
    } #while(length(portfolio$candidateEpics)>0){
    #3.2 - Copia os Épicos de EPA para portfolio$candidateEpics
    portfolio<-tryFlaggedEpicsInNextRelease(portfolio)
    portfolio<-createEpicPrecedenceInRelease(portfolio,countRelease)
  } #for( ce in portfolio)
  #browser()
  portfolio
}

createEpicPrecedenceInRelease<- function(portfolio,countRelease){
  if(length(portfolio$releases)==countRelease) # Na verdade aqui só deveriamos incluir o "end" se todos os predecessores dele estivessem no grafo da (ultima) release
    portfolio<-addEpicToRelease(portfolio, "end", countRelease)
    #portfolio$releases[[countRelease]]<-portfolio$releases[[countRelease]]+vertices("end")
  attributes<-graph_attr(portfolio$releases[[countRelease]])
  portfolio$releases[[countRelease]]<-induced_subgraph(portfolio$precedenceGraph,V(portfolio$releases[[countRelease]])$name)
  graph_attr(portfolio$releases[[countRelease]])<-attributes
  portfolio
}

isEpicElegibleToStart <- function(ep, portfolio) {
  antecessoresEP<-ego(portfolio$precedenceGraph, 1, nodes = ep, mode=c("in"), mindist = 1)[[1]]$name
  intersecao<-intersect(portfolio$epicosEmRelease,antecessoresEP)
  (length(intersect(intersecao,antecessoresEP))==length(antecessoresEP))
}

isEpicElegibleToRelease <- function(portfolio, ep,countRelease) {
  (isEpicInComplianceToReleaseSchedule(portfolio, ep, countRelease) & isEpicInComplianceToPortfolioInvestmentCapital(portfolio, ep))
}

isEpicInComplianceToReleaseSchedule <- function(portfolio, ep,countRelease) {
  graph_attr(portfolio$releases[[countRelease]],"duracaoPlanejadaEmSemanas") >= graph_attr(portfolio$releases[[countRelease]],"duracaoAtualEmSemanas")+vertex_attr(portfolio$precedenceGraph, "duracaoEmSemanas", ep)
}

isEpicInComplianceToPortfolioInvestmentCapital <- function(portfolio, ep) {
  graph_attr(portfolio$precedenceGraph, "capitalDisponivel") >= graph_attr(portfolio$precedenceGraph,"capitalAtual")+vertex_attr(portfolio$precedenceGraph, "investimento", ep)
}

duracaoTotalPortfolio<-function(portfolio){
  soma<-0
  for(i in 1:length(portfolio$releases)){
    soma<-soma+graph_attr(portfolio$releases[[i]],"duracaoPlanejadaEmSemanas")
  }
  soma
}



addEpicToRelease <- function(portfolio, ep, countRelease,sampleMode=T) {
  #3.1.1.1.1 - Retiro "ep" de candidateEpics
  if(sampleMode) portfolio$candidateEpics<-discardEpic(portfolio$candidateEpics, ep)
  #3.1.1.1.3 - Adiciono "ep" a "ce"
  ce <- portfolio$releases[[countRelease]] + vertices(ep)
  portfolio$epicosEmRelease<-c(portfolio$epicosEmRelease,ep)
  #3.1.1.1.4 - Desconto a duranção planejada e o capital
  graph_attr(ce,"duracaoAtualEmSemanas") <- graph_attr(ce,"duracaoAtualEmSemanas")+vertex_attr(portfolio$precedenceGraph, "duracaoEmSemanas", ep)
  graph_attr(portfolio$precedenceGraph,"capitalAtual") <- graph_attr(portfolio$precedenceGraph,"capitalAtual")+vertex_attr(portfolio$precedenceGraph, "investimento", ep)
  #portfolio$precedenceGraph<-g
  portfolio$releases[[countRelease]]<-ce
  #3.1.1.1.2 - Adiciono em candidateEpics os sucessores de "ep"    [VERIFICAR SE TEM, ANTES DE ADICIONAR?????]
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
  sucessores<-ego(portfolio$precedenceGraph, 1, nodes = ep, mode=c("out"), mindist = 1)[[1]]$name
  for (s in sucessores) {
    if(isEpicElegibleToStart(s,portfolio))
      candidateEpics<-c(candidateEpics,s)
  }
  unique(c(portfolio$candidateEpics,candidateEpics))
}

loadPortfolio <- function(vertices=read.csv(paste(getwd(),"/iplan-vertices.csv",sep=""),comment.char="#"),
                          edges=read.csv(paste(getwd(),"/iplan-edges.csv",sep=""),comment.char="#"),
                          capital=read.csv(paste(getwd(),"/iplan-portfolio.csv",sep=""),comment.char="#"),
                          releases=read.csv(paste(getwd(),"/iplan-releases.csv",sep=""),comment.char="#")) {
  portfolio<-NULL
  g<-graph.data.frame(
    edges,
    directed = TRUE,
    vertices=vertices
  )
  graph_attr(g, "capitalDisponivel") <- as.numeric(capital[1,"portfolioCapital"])
  graph_attr(g, "capitalAtual") <- 0
  createReleasesAsGraphs <- function(release){
    ce <- make_empty_graph(n = 0, directed = TRUE)
    graph_attr(ce, "nome") <- release["nome"][[1]]
    graph_attr(ce, "duracaoAtualEmSemanas") <- as.numeric(release["duracaoAtualEmSemanas"][[1]])
    graph_attr(ce, "duracaoPlanejadaEmSemanas") <- as.numeric(release["duracaoPlanejadaEmSemanas"][[1]])
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
    l[[length(l)+1]]<-getRandomValidReleasePlan(portfolio)
  }
  l
}

dataFramePlanosDeEntregaValidos<-function(l=sampleValidReleasePlan(1)){
  #l<-enumerateAllPEVs()
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

  getAHPBeneficiosIntangiveis<-function(pev,resultadoAHP){
    vertices<-NULL
    for (i in 1:length(pev$releases)) {
      vertices<-c(vertices,V(pev$releases[[i]])$name)
    }
    #vertices
    epicos<-vertices[!vertices %in% c("begin","end")]
    #resultadoAHP<-iplan::calculaAHP(sufixo="-intangibles")
    sum(resultadoAHP[epicos,1])
  }
  resultadoAHP<-iplan::calculaAHP(sufixo="-intangibles")
  beneficiosIntangiveis<-unlist(lapply(l,getAHPBeneficiosIntangiveis,resultadoAHP))

  ##############################
  relevanciaVOL<-iplan::calculaVOL()
  getVOL<-function(pev,relevanciaVOL){
    vertices<-NULL
    for (i in 1:length(pev$releases)) {
      vertices<-c(vertices,V(pev$releases[[i]])$name)
    }
    #vertices
    epicos<-vertices[!vertices %in% c("begin","end")]
    #convert de factor para string...
    x<-cbind(relevanciaVOL,epico=as.character(relevanciaVOL$epico.projeto))
    sum(subset(x,epico%in%epicos,select=relevanciaNormalizada))
  }
  vol<-unlist(lapply(l,getVOL,relevanciaVOL))
  ##############################

  pevs<-data.frame(ciclos,investimento,beneficiosIntangiveis,vol)
  pevs[with(pevs, order(-investimento)), ]

}

calculaAHP <- function(sufixo="") {
  ahp <- Load(paste(getwd(),"/iplan1",sufixo,".ahp",sep=""))
  Calculate(ahp)
  relevanciaProjetos<-t(Analyze(ahp)[1,][,3:6])
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),"/Projetos-hierarquia-AHP",sufixo,".png",sep=""), height=30*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()

  ahp <- Load(paste(getwd(),"/iplan2",sufixo,".ahp",sep=""))
  Calculate(ahp)
  resultado<-Analyze(ahp)
  coluna<-ncol(resultado[1,])
  relevanciaEpicosWPS<-t(resultado[1,][,c(-1,-2,-coluna)])*relevanciaProjetos["WPS",1]
  hierarquia<-Visualize(ahp)
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),"/WPS-hierarquia-AHP",sufixo,".png",sep=""), height=30*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()

  ahp <- Load(paste(getwd(),"/iplan3",sufixo,".ahp",sep=""))
  Calculate(ahp)
  resultado<-Analyze(ahp)
  coluna<-ncol(resultado[1,])
  relevanciaEpicosMVC<-t(resultado[1,][,c(-1,-2,-coluna)])*relevanciaProjetos["MVC",1]
  hierarquia<-Visualize(ahp)
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),"/MVC-hierarquia-AHP",sufixo,".png",sep=""), height=30*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()

  ahp <- Load(paste(getwd(),"/iplan4",sufixo,".ahp",sep=""))
  Calculate(ahp)
  resultado<-Analyze(ahp)
  coluna<-ncol(resultado[1,])
  relevanciaEpicosTFM<-t(resultado[1,][,c(-1,-2,-coluna)])*relevanciaProjetos["TFM",1]
  hierarquia<-Visualize(ahp)
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),"/TFM-hierarquia-AHP",sufixo,".png",sep=""), height=30*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()

  ahp <- Load(paste(getwd(),"/iplan5",sufixo,".ahp",sep=""))
  Calculate(ahp)
  resultado<-Analyze(ahp)
  coluna<-ncol(resultado[1,])
  relevanciaEpicosTLC<-t(resultado[1,][,c(-1,-2,-coluna)])*relevanciaProjetos["TLC",1]
  hierarquia<-Visualize(ahp)
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),"/TLC-hierarquia-AHP",sufixo,".png",sep=""), height=30*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()

  relavanciaEpicos<-rbind(relevanciaEpicosWPS,relevanciaEpicosMVC,relevanciaEpicosTFM,relevanciaEpicosTLC)
  #colSums(relavanciaEpicos)
  relavanciaEpicos
}


calculaVOL<-function(){
  ############################################################################################
  arquivo<-"/iplan-volume-investimento-escala-avaliacao"
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
  arquivo<-"/iplan-volume-investimento-prazo"
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
  volumeInvestimento<-read.csv(paste(getwd(),"/iplan-volume-investimento.csv",sep=""),comment.char="#")

  avaliacoesApenasProjetos<-volumeInvestimento #subset(volumeInvestimento,projeto!="")

  x<-relevanciaEscala[match(avaliacoesApenasProjetos$avaliacao,relevanciaEscala$names),]
  avaliacoesApenasProjetos<-cbind(avaliacoesApenasProjetos,relevanciaEscalaAvaliacao=x[,2])

  x<-relevanciaPrazo[match(avaliacoesApenasProjetos$prazo,relevanciaPrazo$names),]
  avaliacoesApenasProjetos<-cbind(avaliacoesApenasProjetos,relevanciaPrazo=x[,2])

  avaliacoesApenasProjetos<-cbind(avaliacoesApenasProjetos,
                                  relevanciaAvalicaoXPrazo=as.numeric(as.character(avaliacoesApenasProjetos$relevanciaEscalaAvaliacao))*as.numeric(as.character(avaliacoesApenasProjetos$relevanciaPrazo)))

  relevancias<-aggregate(avaliacoesApenasProjetos$relevanciaAvalicaoXPrazo,
                         by=list(epico.projeto=avaliacoesApenasProjetos$epico.projeto,
                                 projeto=avaliacoesApenasProjetos$projeto),
                         FUN=sum)
  #Nommalizações

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
  #sum(relevanciaEpicosNoPortfolio$relevanciaNormalizada)
}


execute<-function(enumerateAll=T,originalSamples=40,bootstrapSamples=1000,returnToScale=1,efficiencyOrientation=1){
  #############################################
  if(enumerateAll)
    l<-enumerateAllPEVs()
  else
    l<-sampleValidReleasePlan(originalSamples)
  pevs<-dataFramePlanosDeEntregaValidos(l)
  #############################################

  xSample<-cbind(pevs$investimento)
  ySample<-cbind(pevs$vol,pevs$beneficiosIntangiveis)

  b<-FEAR::boot.sw98(t(xSample),t(ySample),NREP=bootstrapSamples,RTS=returnToScale,ORIENTATION=efficiencyOrientation,alpha=0.1,OUTPUT.FARRELL = T)

  tab2 <- round(rbind(dhat=b$dhat,
                      dhat.m=rowMeans(b$boot),
                      bias=b$bias,
                      dhat.bc=b$dhat.bc,
                      ci.low=b$conf.int[,1],
                      ci.high=b$conf.int[,2])
                ,10)

  if(enumerateAll)
    eficienciaSW98<-1/tab2[1,]
  else
    eficienciaSW98<-1/tab2[4,]

  icInferiorSW98<-1/tab2[6,]
  icSuperiorSW98<-1/tab2[5,]

  pevs<-cbind(pevs,eficienciaSW98,icSuperiorSW98,icInferiorSW98)
  pevs[with(pevs, order(-eficienciaSW98)), ]
  #############################################
}

enumerateAllPEVs<-function(){
  start.time <- Sys.time()
  portfolio<-loadPortfolio()
  portfolio2<-portfolio
  pevsFinais<-NULL
  #seja pevs uma lista de grafos inicialmente vazia
  pevs<-NULL
  #adicionar à pevs um plano só com o nó begin
  duracao<-0
  investimento<-0
  pevs[[1]]<-list(list("begin"),duracao,investimento)
  pevsNaoVerificados<-list()
  pevsNaoVerificados[length(pevsNaoVerificados)+1]<-1
  #vou simular que p é todo no release 1 só para armazenar a duracao em algum lugar e poder reusar isEpicElegibleToRelease
  #graph_attr(portfolio$releases[[1]],"duracaoPlanejadaEmSemanas")<-duracaoTotalPortfolio(portfolio)
  for (qtdCiclos in 1:length(portfolio$releases)) {
    #qtdCiclos=2
    #TEM QUE COLOCAR TODOS OS PLANOS COMO NAO VERIFICADOS PARA SEREM VERIFICADOS NOVAMENTE
    if(qtdCiclos>1){
      pevs<-pevs[pevsFinais]
      pevsNaoVerificados<-1:length(pevs)
      pevsFinais<-NULL
      #pevsNaoVerificados<-pevsFinais
    }
    #enquanto houver um grafo g em pevs ainda não verificado
    primeiraVerificacaoDoCiclo<-T
    while(length(pevsNaoVerificados)>0){
      #pega o primeiro pevs nao verificado
      p<-pevs[[pevsNaoVerificados[[1]]]]
      #atualiza a lista de epicos de p que estão na release, ou seja, todos os epicos de p
      portfolio$epicosEmRelease<-unlist(p[[1]])
      #se adicionar epico à p não violar as restrições de prazo (do ciclo) e orcamento (do portfólio)
      graph_attr(portfolio$precedenceGraph,"capitalAtual")<-p[[3]]
      #vou simular que p é todo no release 1 só para armazenar a duracao em algum lugar e poder reusar isEpicElegibleToRelease
      graph_attr(portfolio$releases[[qtdCiclos]],"duracaoAtualEmSemanas")<-ifelse(primeiraVerificacaoDoCiclo,0,p[[2]])
      #obtém a lista c de nós candidatos a serem iniciados (todos seus precedentes estão em g)
      #ultimoEpico<-portfolio$epicosEmRelease[portfolio$epicosEmRelease]
      #####Tem que pegar os candidatos de todos os nós que estão no plano
      #candidatos<-adjacentes[portfolio$epicosEmRelease]
      candidatos<-NULL
      for (ep in portfolio$epicosEmRelease) {
        candidatos<-c(candidatos,attr(ego(portfolio$precedenceGraph, 1, nodes = ep, mode=c("out"), mindist = 1)[[1]],"names"))
      }
      #remove de candidatos os epicos que ja estao na release
      remover<-intersect(candidatos,portfolio$epicosEmRelease)
      for(r in remover)
        candidatos<-unlist(discardEpic(as.list(candidatos),r))
      #para cada noh candidato
      #epico<-"WPS1" # begin: "WPS1" "MVC1" "TFM1" "TFM3" "TLC1"
      gerouNovoPEV<-F
      for(epico in candidatos){
        #epico="WPS1" epico="WPS4"
        if(isEpicElegibleToStart(epico,portfolio)){
          if(isEpicElegibleToRelease(portfolio,epico,qtdCiclos)){
            #adiciona à pevs um novo plano composto por p+epico
            duracao<-p[[2]]+vertex_attr(portfolio$precedenceGraph, "duracaoEmSemanas", epico)
            investimento<-p[[3]]+vertex_attr(portfolio$precedenceGraph, "investimento", epico)
            pNovo<-list(list(c(portfolio$epicosEmRelease,epico)),duracao,investimento)
            i<-length(pevs)+1
            pevs[[i]]<-pNovo
            pevsNaoVerificados[length(pevsNaoVerificados)+1]<-i
            gerouNovoPEV<-T
          }
        }
      }
      # se p nao gerou nenhum novo, ele é final
      if(!gerouNovoPEV){
        pevsFinais<-c(pevsFinais,pevsNaoVerificados[[1]])
      }
      #p esta verificado, ou seja, sai da lista de nao verificados
      pevsNaoVerificados<-pevsNaoVerificados[-1]
      primeiraVerificacaoDoCiclo<-F
    }
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  allPEVs<-pevs[pevsFinais]
  l <- list()
  for(i in 1:length(allPEVs)){
    p<-portfolio2
    #i<-186
    countRelease<-1
    for(epico in allPEVs[[i]][[1]][[1]]){
      #epico<-"begin" epico<-"WPS1"  epico<-"MVC1"  epico<-"TFM3"
      if(!isEpicElegibleToRelease(p,epico,countRelease))
        countRelease<-countRelease+1
      p<-addEpicToRelease(p, epico, countRelease)
    }
    l[[length(l)+1]]<-p
  }
  l
}

######################################################


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
