setwd("D:/Dati/vito.tranquillo/Desktop/Rprojects/text mining elisabetta")
options(stringsAsFactors = FALSE)
library(tm)
library(ggplot2)
library(ggthemes)
library(igraph)
library(qdap)
rm(list = ls())


df<-read.csv('oct_delta.csv')
#tweets<-data.frame(ID=seq(1:nrow(df)),text=df$text)

tweets<-data.frame(doc_id=seq(1:nrow(df)),text=df$text)

tryTolower<-function(x){
  y=NA
  try_error=tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

custom.stopwords<-c(stopwords('english'), 'lol', 'smh', 'delta', 'amp')

clean.corpus<-function(corpus){
  corpus<-tm_map(corpus, content_transformer(tryTolower))
  corpus<-tm_map(corpus, removeWords, custom.stopwords)
  corpus<-tm_map(corpus, removePunctuation)
  corpus<-tm_map(corpus, stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  return(corpus)
}


corpus <- VCorpus(DataframeSource(tweets))
corpus<-clean.corpus(corpus)
corpus[[103]][1]
corpus[[103]][2]


tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm.tweets.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.tweets.m)
freq.df<-data.frame(word=
                      names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]


freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)
        
associations<-findAssocs(tdm,'apologies', 0.11)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=apologies), data=associations, size=5)+
  theme_gdocs()+geom_text(aes(x=apologies, label=apologies),
                          colour="darkred", hjust=-.25, size=8)+
  theme(text=element_text(size=20),
        axis.title.y = element_blank())

refund<-tweets[grep("refund", tweets$text, ignore.case = T),]
corpus <- VCorpus(DataframeSource(refund))
corpus<-clean.corpus(corpus)
refund.tdm<-TermDocumentMatrix(corpus, control = list(weighting=weightTf))


refund.m<-as.matrix(refund.tdm)
refund.adj<-refund.m %*% t(refund.m)
refund.adj<-graph.adjacency(refund.adj, weighted = TRUE, mode="undirected", diag = T)
refund.adj<-simplify(refund.adj)

plot.igraph(refund.adj, vertex.shape="none",
            vertex.label.font=2, vertex.label.color="darkred", vertex.label.cex=.7, edge.color="gray85")
title(main="xxxxx")


word_network_plot(refund$text)

tdm2<-removeSparseTerms(tdm, sparse=0.975)

hc<-hclust(dist(tdm2, method = "euclidean"), method="complete")
plot(hc, yaxt='n', main="title")


dend.change<-function(n){
  if(is.leaf(n))
{
a<-attributes(n)
labCol<-labelColors[clusMember[which(
  names(clusMember)==a$label)]]
attr(n, "nodePar") <-c(a$nodePar, lab.col=labCol)
}
n
}

hcd<-as.dendrogram(hc)
clusMember<-cutree(hc, 4)
labelColors<-c('darkgrey', 'darkred', 'black', '#bada55')
clusDendro<-dendrapply(hcd, dend.change)
plot(clusDendro, main='title', type='triangle', yaxt='n')

hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)


hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)



wordcloud(freq.df$word, freq.df$frequency, max.words=300, colors=c('black','darkred'))


tryTolower<-function(x){
  y=NA
  try_error=tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}
custom.stopwords<-c(stopwords('english'), 'sorry', 'amazon', 'delta', 'amp')
clean.vec<-function(text.vec){
  text.vec<-tryTolower(text.vec)
  text.vec<-removeWords(text.vec, custom.stopwords)
  text.vec<-removePunctuation(text.vec)
  text.vec<-stripWhitespace(text.vec)
  text.vec<-removeNumbers(text.vec)
  return(text.vec)

}

###############OPEN NPL##################

options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

library(gridExtra)
library(ggmap)
library(ggthemes)
library(NPL)
library(openNLP)
library("openNLPmodels.en")
library(pbapply)
library(stringr)
library(rvest)
library(doBy)
library(tm)
library(cshapes)

temp<-list.files(pattern = '*.txt')

for(i in 1:length(temp)) assign(temp[i],
readLines(temp[i]))

all.emails<-pblapply(temp, get)

