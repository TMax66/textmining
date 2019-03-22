#setwd("~/Desktop/text analysis")
#setwd("D:/Dati/vito.tranquillo/Desktop/Rprojects/text mining elisabetta")
library(tm)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(igraph)
library(qdap)
library(dendextend)
library(circlize)
library(Rgraphviz)
library(graph)
#library(tidyverse)
library(tidytext)
library(topicmodels)
library(dplyr)
library(tidyr)
#library(data.table)
rm(list=ls())

tryTolower<-function(x){
  y=NA
  try_error=tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

clean.corpus<-function(corpus){
  corpus<-tm_map(corpus, content_transformer(tryTolower))
  corpus<-tm_map(corpus, removeWords, custom.stopwords)
  corpus<-tm_map(corpus, removePunctuation)
  corpus<-tm_map(corpus, stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  return(corpus)
}
df<-read.csv('dati.csv', header = T, sep = ";", stringsAsFactors=FALSE)



##################Q10###########
q10<-data.frame(doc_id=seq(1:nrow(df)),text=df$Q10)
custom.stopwords<-c(stopwords('english'), "horses",  "horse", "min", "exits", "etc", "condition", "conditions",
                    "good","appropriate","available","fresh","adequate","presence", "access","enough")
corpus <- VCorpus(DataframeSource(q10))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleanliness", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleaning", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleansing", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "mangers", replacement = "manger")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "litter", replacement="bedding")
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.q10.m<-as.matrix(tdm)
dim(tdm.q10.m)


#####count-based evaluation#####
term.freq<-rowSums(tdm.q10.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

ggplot(freq.df, aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

#####word clouds#########
set.seed(500)
wordcloud(freq.df$word, freq.df$frequency, max.words=100, colors=c('black','darkred'))

###########NETWORK#############

freq.term<-findFreqTerms(tdm, lowfreq = 10)

plot(tdm, term=freq.term, corThreshold = 0.2,weighting=T)










# tdm.q10.m #matrice
# 
# 
# 
# #adjacency matrix
# tdm.q10.m[tdm.q10.m>=1] <- 1
# tdm2<-tdm.q10.m%*%t(tdm.q10.m)
# 
# tdm2[5:10,5:10]
# tdm.g <- graph.adjacency(tdm2, weighted=TRUE, mode="undirected")
# tdm.g<-simplify(tdm.g)
# 
# V(tdm.g)$label <- V(tdm.g)$name
# V(tdm.g)$degree <- degree(tdm.g)
# 
# 
# layout1 <- layout.fruchterman.reingold(tdm.g)
# 
# plot(tdm.g, layout=layout1, vertex.size=20, 
#      vertex.label.color="darkred")


######associations############
associations<-findAssocs(tdm,'floors', 0.2)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=floors), data=associations, size=1)+
  theme_gdocs()+geom_text(aes(x=clean, label=clean),
                          colour="darkred", hjust=-.25, size=3)+
  theme(text=element_text(size=8),
        axis.title.y = element_blank())




X["nome",]
esempio
X<-matrix(runif(100), ncol=10)
>dimnames(X)<-list(righe=LETTERS[1:10], colonne=LETTERS[1:10])
X
X["B",]








tdm2<-removeSparseTerms(tdm, sparse=0.975)

hc<-hclust(dist(tdm2, method = "euclidean"), method="complete")
plot(hc, yaxt="n",main="", hang=0.5, cex=0.6)
rect.hclust(hc,k=6)


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
clusMember<-cutree(hc, 6)
labelColors<-c('darkgrey', 'darkred', 'black', '#bada55', "lightblue", "red")
clusDendro<-dendrapply(hcd, dend.change)
plot(clusDendro, main='title', type='triangle', yaxt='n')

hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)




############################################Q11#######################################
q11<-data.frame(doc_id=seq(1:nrow(df)),text=df$Q11)
custom.stopwords<-c(stopwords('english'), "horses",  "horse", "min", "exits", "etc", "condition", "conditions",
                    "good","adequate","enough","feeding","food","feed")
corpus <- VCorpus(DataframeSource(q11))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleanliness", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleaning", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleansing", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "mangers", replacement = "manger")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "body", replacement = "score")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "score", replacement = "body_score")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "forage", replacement = "roughage")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "hay", replacement = "roughage")
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.q11.m<-as.matrix(tdm)
dim(tdm.q11.m)


#####count-based evaluation#####
term.freq<-rowSums(tdm.q11.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)
#####word clouds#########
set.seed(500)
wordcloud(freq.df$word, freq.df$frequency, max.words=200, colors=c('black','darkred'))

###########NETWORK#############

freq.term<-findFreqTerms(tdm, lowfreq = 10)

plot(tdm, term=freq.term, corThreshold = 0.2,weighting=T)

######associations############
associations<-findAssocs(tdm,'roughage', 0.2)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=roughage), data=associations, size=1)+
  theme_gdocs()+geom_text(aes(x=roughage, label=roughage),
                          colour="darkred", hjust=-.25, size=3)+
  theme(text=element_text(size=8),
        axis.title.y = element_blank())






tdm2<-removeSparseTerms(tdm, sparse=0.975)

hc<-hclust(dist(tdm2, method = "euclidean"), method="complete")
plot(hc, yaxt="n",main="", hang=0.5, cex=0.6)
rect.hclust(hc,k=6)


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
clusMember<-cutree(hc, 6)
labelColors<-c('darkgrey', 'darkred', 'black', '#bada55', "lightblue", "red")
clusDendro<-dendrapply(hcd, dend.change)
plot(clusDendro, main='title', type='triangle', yaxt='n')

hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)



#############################################Q12#######################################
q12<-data.frame(doc_id=seq(1:nrow(df)),text=df$Q12)
custom.stopwords<-c(stopwords('english'), "horses",  "horse", "min", "exits", "etc", "condition", "conditions",
                    "look", "alert","healthy","shiny","bright","signs","feet","good")
corpus <- VCorpus(DataframeSource(q12))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleanliness", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleaning", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleansing", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "mangers", replacement = "manger")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "body", replacement = "score")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "score", replacement = "body_score")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "hair", replacement = "coat")
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.q12.m<-as.matrix(tdm)
dim(tdm.q12.m)


#####count-based evaluation#####
term.freq<-rowSums(tdm.q12.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)
#####word clouds#########
set.seed(500)
wordcloud(freq.df$word, freq.df$frequency, max.words=100, colors=c('black','darkred'))

###########NETWORK#############

freq.term<-findFreqTerms(tdm, lowfreq = 10)

plot(tdm, term=freq.term, corThreshold = 0.2,weighting=T)

######associations############
associations<-findAssocs(tdm,'coat', 0.2)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=coat), data=associations, size=1)+
  theme_gdocs()+geom_text(aes(x=coat, label=coat),
                          colour="darkred", hjust=-.25, size=3)+
  theme(text=element_text(size=8),
        axis.title.y = element_blank())






tdm2<-removeSparseTerms(tdm, sparse=0.975)

hc<-hclust(dist(tdm2, method = "euclidean"), method="complete")
plot(hc, yaxt="n",main="", hang=0.5, cex=0.6)
rect.hclust(hc,k=6)


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
clusMember<-cutree(hc, 6)
labelColors<-c('darkgrey', 'darkred', 'black', '#bada55', "lightblue", "red")
clusDendro<-dendrapply(hcd, dend.change)
plot(clusDendro, main='title', type='triangle', yaxt='n')

hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)



############################################Q13#######################################
q13<-data.frame(doc_id=seq(1:nrow(df)),text=df$Q13)
custom.stopwords<-c(stopwords('english'), "horses",  "horse", "min", "exits", "etc", "condition", "conditions",
                    "environment", "around","animals","behaviour", "normal")
corpus <- VCorpus(DataframeSource(q13))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleanliness", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleaning", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleansing", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "mangers", replacement = "manger")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "herd", replacement = "group")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "social", replacement = "interaction")

tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.q13.m<-as.matrix(tdm)
dim(tdm.q13.m)


#####count-based evaluation#####
term.freq<-rowSums(tdm.q13.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)
#####word clouds#########
set.seed(500)
wordcloud(freq.df$word, freq.df$frequency, max.words=100, colors=c('black','darkred'))

###########NETWORK#############

freq.term<-findFreqTerms(tdm, lowfreq = 10)

plot(tdm, term=freq.term, corThreshold = 0.2,weighting=T)

######associations############
associations<-findAssocs(tdm,'group', 0.2)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=group), data=associations, size=1)+
  theme_gdocs()+geom_text(aes(x=group, label=group),
                          colour="darkred", hjust=-.25, size=3)+
  theme(text=element_text(size=8),
        axis.title.y = element_blank())






tdm2<-removeSparseTerms(tdm, sparse=0.975)

hc<-hclust(dist(tdm2, method = "euclidean"), method="complete")
plot(hc, yaxt="n",main="", hang=0.5, cex=0.6)
rect.hclust(hc,k=6)


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
clusMember<-cutree(hc, 6)
labelColors<-c('darkgrey', 'darkred', 'black', '#bada55', "lightblue", "red")
clusDendro<-dendrapply(hcd, dend.change)
plot(clusDendro, main='title', type='triangle', yaxt='n')

hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)


############################################Q14#######################################
q14<-data.frame(doc_id=seq(1:nrow(df)),text=df$Q14)
custom.stopwords<-c(stopwords('english'), "horses",  "horse", "min", "exits", "etc", "condition", "conditions",
                    "back","people","herd","behaviours","abnormal","animal","presence","herd","excessive","behaviour")
corpus <- VCorpus(DataframeSource(q14))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleanliness", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleaning", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleansing", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "mangers", replacement = "manger")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "stereotypical", replacement = "stereotypies")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "dance", replacement = "weaving")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "bear", replacement = "weaving")
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.q14.m<-as.matrix(tdm)
dim(tdm.q14.m)


#####count-based evaluation#####
term.freq<-rowSums(tdm.q14.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)
#####word clouds#########

wordcloud(freq.df$word, freq.df$frequency, max.words=100, colors=c('black','darkred'))

###########NETWORK#############

freq.term<-findFreqTerms(tdm, lowfreq = 10)

plot(tdm, term=freq.term, corThreshold = 0.2,weighting=T)

######associations############
associations<-findAssocs(tdm,'weaving', 0.2)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=weaving), data=associations, size=1)+
  theme_gdocs()+geom_text(aes(x=weaving, label=weaving),
                          colour="darkred", hjust=-.25, size=3)+
  theme(text=element_text(size=8),
        axis.title.y = element_blank())






tdm2<-removeSparseTerms(tdm, sparse=0.975)

hc<-hclust(dist(tdm2, method = "euclidean"), method="complete")
plot(hc, yaxt="n",main="", hang=0.5, cex=0.6)
rect.hclust(hc,k=6)


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
clusMember<-cutree(hc, 6)
labelColors<-c('darkgrey', 'darkred', 'black', '#bada55', "lightblue", "red")
clusDendro<-dendrapply(hcd, dend.change)
plot(clusDendro, main='title', type='triangle', yaxt='n')

hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)







##################TIDY#######################

######bi-gram Q10#####
data("stop_words")
q10<-data_frame(doc_id=seq(1:nrow(df)),text=df$Q10)

my_stop_words<-rbind(c("horses",  "horse", "min", "exits", "etc", "condition", "conditions",
                       "good","appropriate","available","fresh","adequate","presence", "access","enough"), stop_words)

q10bgr<-q10 %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)

q10bgr %>%
  count(ngram, sort = TRUE)

q10bsep<-q10bgr %>%
separate(ngram, c("word1", "word2"), sep = " ")

q10bsepf <- q10bsep %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>%
  count(word1, word2, sort = TRUE)
q10bsepf<-na.omit(q10bsepf)


bigram_graph <- q10bsepf %>%
  filter(n>1) %>%
  graph_from_data_frame()



library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


###bi-gram Q11###
data("stop_words")
q11<-data_frame(doc_id=seq(1:nrow(df)),text=df$Q11)

my_stop_words<-rbind(c("horses",  "horse", "box", "min",
                       "animals", "etc"), stop_words)

q11bgr<-q11 %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)

q11bgr %>%
  count(ngram, sort = TRUE)

q11bsep<-q11bgr %>%
  separate(ngram, c("word1", "word2"), sep = " ")

q11bsepf <- q11bsep %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>%
  count(word1, word2, sort = TRUE)
q11bsepf<-na.omit(q11bsepf)


bigram_graph <- q11bsepf %>%
  filter(n>1) %>%
  graph_from_data_frame()



library(ggraph)
set.seed(2017)

# ggraph(bigram_graph, layout = "fr") +
#   geom_edge_link() +
#   geom_node_point() +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


###bi-gram Q12###
data("stop_words")
q12<-data_frame(doc_id=seq(1:nrow(df)),text=df$Q12)

my_stop_words<-rbind(c("horses",  "horse", "box", "min",
                       "animals", "etc"), stop_words)

q12bgr<-q12 %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)

q12bgr %>%
  count(ngram, sort = TRUE)

q12bsep<-q12bgr %>%
  separate(ngram, c("word1", "word2"), sep = " ")

q12bsepf <- q12bsep %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>%
  count(word1, word2, sort = TRUE)
q12bsepf<-na.omit(q12bsepf)


bigram_graph <- q12bsepf %>%
  filter(n>1) %>%
  graph_from_data_frame()



library(ggraph)
set.seed(2017)

# ggraph(bigram_graph, layout = "fr") +
#   geom_edge_link() +
#   geom_node_point() +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

###Q13-bigram###
data("stop_words")
q13<-data_frame(doc_id=seq(1:nrow(df)),text=df$Q13)

my_stop_words<-rbind(c("horses",  "horse", "box", "min",
                       "animals", "etc"), stop_words)

q13bgr<-q13%>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)

q13bgr %>%
  count(ngram, sort = TRUE)

q13bsep<-q13bgr %>%
  separate(ngram, c("word1", "word2"), sep = " ")

q13bsepf <- q13bsep %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>%
  count(word1, word2, sort = TRUE)
q13bsepf<-na.omit(q13bsepf)


bigram_graph <- q13bsepf %>%
  filter(n>1) %>%
  graph_from_data_frame()



library(ggraph)
set.seed(2017)

# ggraph(bigram_graph, layout = "fr") +
#   geom_edge_link() +
#   geom_node_point() +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


###Q14-bigram###
data("stop_words")
q14<-data_frame(doc_id=seq(1:nrow(df)),text=df$Q14)

my_stop_words<-rbind(c("horses",  "horse", "box", "min",
                       "animals", "etc"), stop_words)

q14bgr<-q14 %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)

q14bgr %>%
  count(ngram, sort = TRUE)

q14bsep<-q14bgr %>%
  separate(ngram, c("word1", "word2"), sep = " ")

q14bsepf <- q14bsep %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>%
  count(word1, word2, sort = TRUE)
q14bsepf<-na.omit(q14bsepf)


bigram_graph <- q14bsepf %>%
  filter(n>1) %>%
  graph_from_data_frame()



library(ggraph)
set.seed(2017)

# ggraph(bigram_graph, layout = "fr") +
#   geom_edge_link() +
#   geom_node_point() +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


























###############TOPIC MODELING###############

dtm<-DocumentTermMatrix(corpus, control=list(weighting=weightTf))

rowTotals <- apply(dtm , 1, sum) 
dtm<-dtm.new<- dtm[rowTotals> 0, ] 


q10lda <- LDA(dtm, k = 2, control = list(seed = 1234))

q10topics <- tidy(q10lda, matrix = "beta")




ap_top_terms <- q10topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  mutate(topic = paste0("topic", topic)) %>%
  ggplot(aes(term, beta)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()




beta_spread <- q10topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>% 
  group_by(log_ratio < 0) %>%
  top_n(15, abs(log_ratio)) %>%
  ungroup() %>% 
  mutate(word = reorder(term, log_ratio))  
  



ggplot(aes(word, log_ratio))+
  geom_col()

  # arrange(topic, -log_ratio) %>% 
  # mutate(term = reorder(term, log_ratio)) %>%
 # ggplot(aes(term, log_ratio)) +
  #geom_col() +
  #coord_flip()

beta_spread %>%
  group_by(log_ratio < 0) %>%
  top_n(15, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(word = reorder(term, log_ratio)) %>%
  ggplot(aes(word, log_ratio)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio") 
  

