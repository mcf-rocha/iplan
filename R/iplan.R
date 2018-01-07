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
  #loginfo("Gerando um plano de entrega valido....")
  portfolio<-p
  #2 - Incializar o conjunto candidateEpics (Épicos Candidatos, não iniciados) contendo os vizinhos de "begin"
  portfolio$candidateEpics <- as.character(edges[edges$Origem=="begin",]$Destino)
  portfolio<-addEpicToRelease(portfolio, "begin", 1)
  portfolio$waitingListEpics <- list()
  countRelease <- 0
  #3 - Enquanto for possível obter "ce", o próximo Ciclo de Entrega em portfolio
  for( ce in portfolio$releases){
    countRelease <- countRelease + 1
    loginfo(paste("      Gerando o ciclo de entrega ",countRelease,"...",sep = ""))
    #3.1 - Enquanto for possível obter "ep", ao acaso, a partir de candidateEpics
    while(length(portfolio$candidateEpics)>0){
      #loginfo(paste("Ainda existem ",length(portfolio$candidateEpics)," epicos candidatos.",sep = ""))
      ep <- sample(portfolio$candidateEpics,1)[[1]]
      #if(countRelease==1) loginfo(paste("Epico observado foi ",ep," em [",paste(portfolio$candidateEpics,collapse=","),"]",sep = ""))
      #3.1.1 - Se "ep" pode ser inciado (ele não possui antecessores em portfolio$candidateEpics)
      if(isEpicElegibleToStart(ep, portfolio)){
        #3.1.1.1 - Se ele não viola a duração planejada de "ce" e nem o capital total do portfolio
        if (isEpicElegibleToRelease(portfolio, ep,countRelease)){
          portfolio<-addEpicToRelease(portfolio, ep, countRelease)
        #3.1.1.2 - Se "ep" viola o capital total do portfolio
        }else if (!isEpicInComplianceToPortfolioInvestmentCapital(portfolio, ep,countRelease)){
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
  antecessoresEP<-as.character(edges[edges$Destino==ep,]$Origem)
  intersecao<-intersect(portfolio$epicosEmRelease,antecessoresEP)
  (length(intersect(intersecao,antecessoresEP))==length(antecessoresEP))
}

isEpicElegibleToRelease <- function(portfolio, ep,countRelease) {
  (isEpicInComplianceToReleaseSchedule(portfolio, ep, countRelease) & isEpicInComplianceToPortfolioInvestmentCapital(portfolio, ep, countRelease))
}

isEpicInComplianceToReleaseSchedule <- function(portfolio, ep,countRelease) {
  releases$duracao[countRelease] >= graph_attr(portfolio$releases[[countRelease]],"duracaoAtualEmSemanas")+vertices[vertices$id==ep,]$duracao
}

isEpicInComplianceToPortfolioInvestmentCapital <- function(portfolio, ep,countRelease) {
  capital$capital >= vertices[vertices$id==ep,]$investimento
  #juro<-graph_attr(portfolio$precedenceGraph, "juro")
  #if(juro==0)
  #  capital$capital >= graph_attr(portfolio$precedenceGraph,"capitalAtual")+vertices[vertices$id==ep,]$investimento
  #else # valor / (1+rate)^ciclo
  #  capital$capital >= graph_attr(portfolio$precedenceGraph,"capitalAtual") + ((vertices[vertices$id==ep,]$investimento)*(1+juro)^countRelease)
}

duracaoTotalPortfolio<-function(portfolio){
  soma<-0
  for(i in 1:length(portfolio$releases)){
    soma<-soma+graph_attr(portfolio$releases[[i]],"duracaoPlanejadaEmSemanas")
  }
  soma
}



addEpicToRelease <- function(portfolio, ep, countRelease,sampleMode=T) { #when sampleMode=F, enumerateAllPEVs
  #3.1.1.1.1 - Retiro "ep" de candidateEpics
  if(sampleMode) portfolio$candidateEpics<-discardEpic(portfolio$candidateEpics, ep)
  #3.1.1.1.3 - Adiciono "ep" a "ce"
  ce <- portfolio$releases[[countRelease]] + vertices(ep)
  portfolio$epicosEmRelease<-c(portfolio$epicosEmRelease,ep)
  #3.1.1.1.4 - Desconto a duranção planejada e o capital
  graph_attr(ce,"duracaoAtualEmSemanas") <- graph_attr(ce,"duracaoAtualEmSemanas")+vertices[vertices$id==ep,]$duracao #vertex_attr(portfolio$precedenceGraph, "duracao", ep)
  juro<-graph_attr(portfolio$precedenceGraph, "juro")
  graph_attr(portfolio$precedenceGraph,"capitalAtual") <- as.numeric(graph_attr(portfolio$precedenceGraph,"capitalAtual"))+vertices[vertices$id==ep,]$investimento
  graph_attr(portfolio$precedenceGraph,"capitalDisponivel")<-(graph_attr(portfolio$precedenceGraph,"capitalDisponivel")*(1+juro)^countRelease)-vertices[vertices$id==ep,]$investimento
  #if(juro==0)
  #  graph_attr(portfolio$precedenceGraph,"capitalAtual") <- as.numeric(graph_attr(portfolio$precedenceGraph,"capitalAtual"))+vertices[vertices$id==ep,]$investimento #vertex_attr(portfolio$precedenceGraph, "investimento", ep)
  #else
  #  graph_attr(portfolio$precedenceGraph,"capitalAtual") <- as.numeric(graph_attr(portfolio$precedenceGraph,"capitalAtual"))+((vertices[vertices$id==ep,]$investimento)*(1+juro)^countRelease)
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
  #sucessores<-ego(portfolio$precedenceGraph, 1, nodes = ep, mode=c("out"), mindist = 1)[[1]]$name
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

  #versao antiga quando eu usada o AHP relativo
  #getAHPBeneficiosIntangiveis<-function(pev,resultadoAHP){
  #  vertices<-NULL
  #  for (i in 1:length(pev$releases)) {
  #    vertices<-c(vertices,V(pev$releases[[i]])$name)
  #  }
  #  #vertices
  #  epicos<-vertices[!vertices %in% c("begin","end")]
  #  #resultadoAHP<-iplan::calculaAHP(sufixo="-intangibles")
  #  sum(resultadoAHP[epicos,1])
  #}
  #resultadoAHP<-calculaAHP(sufixo="-intangibles")
  #beneficiosIntangiveis<-unlist(lapply(l,getAHPBeneficiosIntangiveis,resultadoAHP))

  ##############################
  #relevanciaVOL<-calculaVOL()
  relevanciaIntangiveis<-calculaAHPabsoluto(beneficios="intangiveis")
  getBeneficios<-function(pev,relevanciaBeneficios){
    vertices<-NULL
    for (i in 1:length(pev$releases)) {
      vertices<-c(vertices,V(pev$releases[[i]])$name)
    }
    #vertices
    epicos<-vertices[!vertices %in% c("begin","end")]
    #convert de factor para string...
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
  ##############################

  tangiveis=read.csv(paste(getwd(),"/iplan-beneficios-tangiveis.csv",sep=""),comment.char="#")
  colnames(tangiveis)<-c("epico.projeto","relevanciaNormalizada")
  tang<-unlist(lapply(l,getBeneficios2,tangiveis))

  ##############################
  ##relevanciaVOL<-calculaVOL()
  #versao antiga quando usava AHP
  #relevanciaTangiveis<-calculaAHPabsoluto(beneficios="tangiveis")
  #tang<-unlist(lapply(l,getBeneficios,relevanciaTangiveis))
  ##############################

  pevs<-data.frame(ciclos,investimento,beneficiosIntangiveis=intang,beneficiosTangiveis=tang)
  pevs[with(pevs, order(-investimento)), ]

}
#Deprecated
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
  relevanciaEpicosWPS<-t(resultado[1,][,c(-1,-2,-coluna)])*relevanciaProjetos["CNC",1]
  hierarquia<-Visualize(ahp)
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),"/WPS-hierarquia-AHP",sufixo,".png",sep=""), height=30*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()

  ahp <- Load(paste(getwd(),"/iplan3",sufixo,".ahp",sep=""))
  Calculate(ahp)
  resultado<-Analyze(ahp)
  coluna<-ncol(resultado[1,])
  relevanciaEpicosMVC<-t(resultado[1,][,c(-1,-2,-coluna)])*relevanciaProjetos["TSI",1]
  hierarquia<-Visualize(ahp)
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),"/MVC-hierarquia-AHP",sufixo,".png",sep=""), height=30*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()

  ahp <- Load(paste(getwd(),"/iplan4",sufixo,".ahp",sep=""))
  Calculate(ahp)
  resultado<-Analyze(ahp)
  coluna<-ncol(resultado[1,])
  relevanciaEpicosTFM<-t(resultado[1,][,c(-1,-2,-coluna)])*relevanciaProjetos["SAI",1]
  hierarquia<-Visualize(ahp)
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),"/TFM-hierarquia-AHP",sufixo,".png",sep=""), height=30*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()

  ahp <- Load(paste(getwd(),"/iplan5",sufixo,".ahp",sep=""))
  Calculate(ahp)
  resultado<-Analyze(ahp)
  coluna<-ncol(resultado[1,])
  relevanciaEpicosTLC<-t(resultado[1,][,c(-1,-2,-coluna)])*relevanciaProjetos["HTV",1]
  hierarquia<-Visualize(ahp)
  tabela<-AnalyzeTable(ahp)
  png(paste(getwd(),"/TLC-hierarquia-AHP",sufixo,".png",sep=""), height=30*nrow(tabela), width=100*ncol(tabela))
  grid.table(tabela)
  dev.off()

  relavanciaEpicos<-rbind(relevanciaEpicosWPS,relevanciaEpicosMVC,relevanciaEpicosTFM,relevanciaEpicosTLC)
  #colSums(relavanciaEpicos)
  relavanciaEpicos
}


#Deprecated
calculaVOL<-function(){
  ############################################################################################
  arquivo<-"/iplan-beneficios-tangiveis-escala-avaliacao"
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
  arquivo<-"/iplan-beneficios-tangiveis-criterios"
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
  volumeInvestimento<-read.csv(paste(getwd(),"/iplan-beneficios-tangiveis-avaliacao.csv",sep=""),comment.char="#")

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


  ##sum(relevanciaEpicosNoPortfolio$relevanciaNormalizada)
}


iplan<-function(enumerateAll=T,originalSamples=40,bootstrapSamples=1000,returnToScale=1,efficiencyOrientation=1){
  #############################################
  if(enumerateAll)
    l<-enumerateAllPEVs()
  else
    l<-sampleValidReleasePlan(originalSamples)
  pevs<-dataFramePlanosDeEntregaValidos(l)
  #############################################

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
  #############################################
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
  capitalDisp<-graph_attr(portfolio$precedenceGraph, "capitalDisponivel")
  pevs[[1]]<-list(list("begin"),duracao,investimento,capitalDisp)
  pevsNaoVerificados<-list()
  pevsNaoVerificados[length(pevsNaoVerificados)+1]<-1
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
      
      #loginfo(paste("Analisando o plano ",p[[1]]," que ao final sera considerado verificado.",sep = ""))
      portfolio$epicosEmRelease<-unlist(p[[1]])
      graph_attr(portfolio$releases[[qtdCiclos]],"duracaoAtualEmSemanas")<-p[[2]]
      graph_attr(portfolio$precedenceGraph,"capitalAtual")<-p[[3]]
      graph_attr(portfolio$precedenceGraph, "capitalDisponivel")<-p[[4]]
      candidatos<-NULL
      for (ep in portfolio$epicosEmRelease) {
        #ep="f107"
        candidatos<-c(candidatos,as.character(edges[edges$Origem==ep,]$Destino))
      }
      remover<-intersect(candidatos,portfolio$epicosEmRelease)
      for(r in remover)
        candidatos<-unlist(discardEpic(as.list(candidatos),r))
      candidatos<-unique(candidatos)
      gerouNovoPEV<-F
      for(epico in candidatos){
        #epico="f2" epico="f6"
        if(isEpicElegibleToStart(epico,portfolio)){
          if(isEpicElegibleToRelease(portfolio,epico,qtdCiclos)){
            duracao<-p[[2]]+vertices[vertices$id==epico,]$duracao
            juro<-graph_attr(portfolio$precedenceGraph, "juro")
            capitalDisp<-(p[[4]]*(1+juro)^qtdCiclos)-(vertices[vertices$id==epico,]$investimento)
            #loginfo(paste(c("Juro mudou de",(vertices[vertices$id==epico,]$investimento),"para",((vertices[vertices$id==epico,]$investimento)*(1+juro)^qtdCiclos),"."), collapse = " "))
            investimento<-p[[3]]+vertices[vertices$id==epico,]$investimento
            #loginfo(paste(c("Juro mudou de",(vertices[vertices$id==epico,]$investimento),"para",((vertices[vertices$id==epico,]$investimento)*(1+juro)^qtdCiclos),"."), collapse = " "))
            pNovo<-list(list(c(portfolio$epicosEmRelease,epico)),duracao,investimento,capitalDisp)
            i<-length(pevs)+1
            pevs[[i]]<-pNovo
            pevsNaoVerificados[length(pevsNaoVerificados)+1]<-i
            gerouNovoPEV<-T
            #loginfo(paste(c("Plano ",i,": ",c(portfolio$epicosEmRelease,epico),". adicionado."), collapse = " "))
            #loginfo(paste(c("Agora existem ",length(pevsNaoVerificados)," planos nao verificados. "), collapse = " "))
          }
        }
      }
      if(!gerouNovoPEV){ # se p nao gerou nenhum novo, ele é final
        if(length(pevsNaoVerificados)>0){
          pevsFinais<-c(pevsFinais,pevsNaoVerificados[[1]])
          #loginfo(paste(c("Plano ",pevsNaoVerificados[[1]]," (",portfolio$epicosEmRelease,") nao gerou nenhum novo plano, eh final."), collapse=" "))
        }else{
          #loginfo("Nao tem mais nenhum nao verificado nesse ciclo. Vamos para outro?")
        }
      }
      pevsNaoVerificados<-pevsNaoVerificados[-1]
    }
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  loginfo(paste(c("O tempo de processamento foi ",time.taken), collapse=" "))
  loginfo(paste(c("Construindo o array de planos para retona-lo como resultado e poder chamar iplan."), collapse = " "))
  start.time <- Sys.time()
  allPEVs<-pevs[pevsFinais]
  l <- list()
  len<-length(allPEVs)
  loginfo(paste(c("Total de planos de entrega:",len), collapse=" "))
  for(i in 1:len){
    p<-portfolio2 #recuperando o portfolio original
    #i<-186
    countRelease<-1
    for(epico in allPEVs[[i]][[1]][[1]]){
      #epico<-"begin" epico<-"WPS1"  epico<-"MVC1"  epico<-"TFM3"
      #loginfo(paste(c("Estou no countRelease ",countRelease), collapse=" "))
      if(!isEpicElegibleToRelease(p,epico,countRelease)){
        if(!(epico=="begin"||epico=="end")){
          countRelease<-countRelease+1
        }
        #loginfo(paste(c("Somei 1 ao countRelease que agora é ",countRelease), collapse=" "))
      }
      #loginfo(paste(c("Estou no countRelease ",countRelease), collapse=" "))
      p<-addEpicToRelease(p, epico, countRelease)
    }
    l[[length(l)+1]]<-p
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
