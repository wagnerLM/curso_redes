### Redes frequentistas

# Personalidade
#carregando o pacote
library(qgraph)
# preparando os dados
BFI_sub<-read.csv("https://raw.githubusercontent.com/wagnerLM/curso_redes/master/BFI_sub",sep = ";")
View(BFI_sub)
BFI_grupos <- rep(c('A','C','E','N','O'),each=5)
BFI_groups
BFI_names <- scan("https://raw.githubusercontent.com/wagnerLM/curso_redes/master/BFI_names",what = "character", sep = "\n")
BFI_names

# representacao grafica e correlacoes bivariadas
qgraph(cor_auto(BFI_sub),layout="circle")
qgraph(cor_auto(BFI_sub),layout="spring")
library(psych)
cor.plot(cor_auto(BFI_sub))
par(mfrow=c(1,1))

# redes de correlacoes parciais
BFI_g <- qgraph(cor_auto(BFI_sub), layout = "groups", 
                      graph = "glasso", sampleSize = nrow(bfi),
                      nodeNames = BFI_names, groups = BFI_grupos, legend.cex = 0.25, 
                      cut = 0.1, maximum = 1, minimum = 0, esize = 20,
                      vsize = 5,threshold=T)
BFI_g <- qgraph(cor_auto(BFI_sub), layout = "spring", 
                graph = "glasso", sampleSize = nrow(bfi),
                nodeNames = BFI_names, groups = BFI_grupos, legend.cex = 0.25, 
                cut = 0.1, maximum = 1, minimum = 0, esize = 20,
                vsize = 5,threshold=T)
cor.plot(getWmat(BFI_g))
par(mfrow=c(1,1))

# Depressao
# preparando os dados
dass_d<-read.csv("https://raw.githubusercontent.com/wagnerLM/curso_redes/master/dass_d",sep=";")
View(dass_d)
dass_d_names<-scan("https://raw.githubusercontent.com/wagnerLM/curso_redes/master/dass_d_names",what = "character",sep="\n")
dass_d_names

# rede das correlacoes parciais
dass_d_g<-qgraph(cor_auto(dass_d),layout="spring",graph="glasso",sampleSize=nrow(dass_d),nodeNames=dass_d_names,labels=colnames(dass_d),threshold=T,legend.cex=0.4)
View(getWmat(dass_d_g))

# flow chart: quais os preditores de "nao ter iniciativa"
flow(dass_d_g,"DASS5")

# pathways: qual a influencia da "falta de sentido" em "nao ter iniciativa"
pathways(dass_d_g,"DASS21","DASS5")

# DASS21 completa (comorbidade e sintomas de alerta)
# preparando os dados
dass21<-read.csv("https://raw.githubusercontent.com/wagnerLM/curso_redes/master/dass21",sep=";")
View(dass21)
dass21_names<-scan("https://raw.githubusercontent.com/wagnerLM/curso_redes/master/dass21_names",what = "character",sep="\n")
dass21_names

#grafo de correlacoes parciais
dass_g<-qgraph(cor_auto(dass21),layout="spring",graph="glasso",sampleSize=nrow(dass21),nodeNames=dass21_names,labels=colnames(dass21),threshold = TRUE,legend.cex=0.4)

# medidas de centralidade (forca, proximidade e conectividade; considerando o valor absoluto dos vertices)
centralityPlot(dass_g,labels = dass21_names)
# influencia esperada (soma dos vertices, considerando o sinal)
centralityPlot(dass_g,include = "ExpectedInfluence",orderBy = "ExpectedInfluence",labels = dass21_names)

# analise de comunidades (agrupamentos dentro de uma rede)
# pacote igraph
library(igraph)

# estimando o número de comunidades
walktrap.community(as.igraph(dass_g))
dass_com<-walktrap.community(as.igraph(dass_g))

# rede com comunidades
qgraph(dass_g,groups=as.factor(dass_com$membership),palette="pastel")

# Predicao autismo
autism<-read.csv("https://raw.githubusercontent.com/wagnerLM/curso_redes/master/autism",sep= ";")
autism_names<-scan("https://raw.githubusercontent.com/wagnerLM/curso_redes/master/autism_names",what = "character",sep="\n")

autism_g<-qgraph(cor_auto(autism),layout="spring",sampleSize=nrow(autism),nodeNames=autism_names,labels=colnames(autism),edge.labels=T,tuning=0.5,legend.cex=0.3)

flow(autism_g,"IiS")
flow(autism_g,"SatTreat")
