#setwd("~/Desktop/text analysis")
setwd("D:/Dati/vito.tranquillo/Desktop/Rprojects/text mining elisabetta")
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
library(tidyverse)
library(tidytext)
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



############################################Q10#######################################à
q10<-data.frame(doc_id=seq(1:nrow(df)),text=df$Q10)


custom.stopwords<-c(stopwords('english'), "horses",  "horse", "box", "min",
                    "manger", "mangers", "exits", "etc")

#custom.stopwords<-c(stopwords('english'))

corpus <- VCorpus(DataframeSource(q10))
corpus<-clean.corpus(corpus)

corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleanliness", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleaning", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleansing", replacement = "clean")
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.975)
freq.terms<-findFreqTerms(tdm, lowfreq = 15)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 15)
df <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)





tdm.q10.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.q10.m)
freq.df<-data.frame(word=
                      names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)






tdm<-removeSparseTerms(tdm,  sparse=0.975)
associations<-findAssocs(tdm,'presence', 0.1)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=clean), data=associations, size=3)+
  theme_gdocs()+geom_text(aes(x=clean, label=clean),
                          colour="darkred", hjust=-.25, size=4)+
  theme(text=element_text(size=15),
        axis.title.y = element_blank())




clean<-q10[grep("clean", q10$text, ignore.case = T),]
corpus <- VCorpus(DataframeSource(clean[1:3,]))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleanliness", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleaning", replacement = "clean")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "cleansing", replacement = "clean")
clean.tdm<-TermDocumentMatrix(corpus, control = list(weighting=weightTf))

f.m<-as.matrix(clean.tdm)
f.adj<-f.m %*% t(f.m)
f.adj<-graph.adjacency(f.adj, weighted = TRUE, mode="undirected", diag = T)
f.adj<-simplify(f.adj)



plot.igraph(f.adj, vertex.shape="none",
            vertex.label.font=2, vertex.label.color="darkred", vertex.label.cex=.7, edge.color="gray85")
title(main="xxxxx")



word_network_plot(clean$text[1:5])


word_associate(clean$text, match.string = c('clean'),
               stopwords = Top25Words, network.plot = T, cloud.colors = c('gray85', 'darkred'))


tdm2<-removeSparseTerms(tdm, sparse=0.975)

hc<-hclust(dist(tdm2, method = "euclidean"), method="complete")
plot(hc, yaxt='n', main="title")
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


set.seed(500)
wordcloud(freq.df$word, freq.df$frequency, max.words=100, colors=c('black','darkred'))

############################################Q11#######################################à
q11<-data.frame(doc_id=seq(1:nrow(df)),text=df$Q10)


custom.stopwords<-c(stopwords('english'), "horses",  "horse", "box", "feed", "food", "feeding")

#custom.stopwords<-c(stopwords('english'))

corpus <- VCorpus(DataframeSource(q11))
corpus<-clean.corpus(corpus)

tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm.q11.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.q11.m)
freq.df<-data.frame(word=
                      names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


associations<-findAssocs(tdm,'hay', 0.25)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=hay), data=associations, size=3)+
  theme_gdocs()+geom_text(aes(x=hay, label=hay),
                          colour="darkred", hjust=-.25, size=4)+
  theme(text=element_text(size=15),
        axis.title.y = element_blank())




hay<-q11[grep("hay", q11$text, ignore.case = T),]
corpus <- VCorpus(DataframeSource(hay[1:3,]))
corpus<-clean.corpus(corpus)
clean.tdm<-TermDocumentMatrix(corpus, control = list(weighting=weightTf))

f.m<-as.matrix(hay.tdm)
f.adj<-f.m %*% t(f.m)
f.adj<-graph.adjacency(f.adj, weighted = TRUE, mode="undirected", diag = T)
f.adj<-simplify(f.adj)



plot.igraph(f.adj, vertex.shape="none",
            vertex.label.font=2, vertex.label.color="darkred", vertex.label.cex=.7, edge.color="gray85")
title(main="xxxxx")



word_network_plot(hay$text[1:5])


word_associate(clean$text, match.string = c('hay'),
               stopwords = Top25Words, network.plot = T, cloud.colors = c('gray85', 'darkred'))


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
clusMember<-cutree(hc, 6)
labelColors<-c('darkgrey', 'darkred', 'black', '#bada55', "lightblue", "red")
clusDendro<-dendrapply(hcd, dend.change)
plot(clusDendro, main='title', type='triangle', yaxt='n')

hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)


set.seed(500)
wordcloud(freq.df$word, freq.df$frequency, max.words=100, colors=c('black','darkred'))

############################################Q12#######################################à
q12<-data.frame(doc_id=seq(1:nrow(df)),text=df$Q12)


custom.stopwords<-c(stopwords('english'), "horses",  "horse", "box", "feed", "food", "feeding", "etc", "condition")

#custom.stopwords<-c(stopwords('english'))

corpus <- VCorpus(DataframeSource(q12))
corpus<-clean.corpus(corpus)

tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm.q12.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.q12.m)
freq.df<-data.frame(word=
                      names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


associations<-findAssocs(tdm,'coat', 0.25)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=coat), data=associations, size=3)+
  theme_gdocs()+geom_text(aes(x=coat, label=coat),
                          colour="darkred", hjust=-.25, size=4)+
  theme(text=element_text(size=15),
        axis.title.y = element_blank())




coat<-q12[grep("coat", q12$text, ignore.case = T),]
corpus <- VCorpus(DataframeSource(coat[1:3,]))
corpus<-clean.corpus(corpus)
clean.tdm<-TermDocumentMatrix(corpus, control = list(weighting=weightTf))

f.m<-as.matrix(coat.tdm)
f.adj<-f.m %*% t(f.m)
f.adj<-graph.adjacency(f.adj, weighted = TRUE, mode="undirected", diag = T)
f.adj<-simplify(f.adj)



plot.igraph(f.adj, vertex.shape="none",
            vertex.label.font=2, vertex.label.color="darkred", vertex.label.cex=.7, edge.color="gray85")
title(main="xxxxx")



word_network_plot(coat$text[1:5])


word_associate(clean$text, match.string = c('hay'),
               stopwords = Top25Words, network.plot = T, cloud.colors = c('gray85', 'darkred'))


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
clusMember<-cutree(hc, 6)
labelColors<-c('darkgrey', 'darkred', 'black', '#bada55', "lightblue", "red")
clusDendro<-dendrapply(hcd, dend.change)
plot(clusDendro, main='title', type='triangle', yaxt='n')

hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)


set.seed(500)
wordcloud(freq.df$word, freq.df$frequency, max.words=100, colors=c('black','darkred'))

############################################Q13#######################################à
q13<-data.frame(doc_id=seq(1:nrow(df)),text=df$Q13)


custom.stopwords<-c(stopwords('english'), "horses",  "horse", "normal", "behaviour", "herd", "group")

#custom.stopwords<-c(stopwords('english'))

corpus <- VCorpus(DataframeSource(q13))
corpus<-clean.corpus(corpus)

tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm.q13.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.q13.m)
freq.df<-data.frame(word=
                      names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


associations<-findAssocs(tdm,'grazing', 0.25)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=grazing), data=associations, size=3)+
  theme_gdocs()+geom_text(aes(x=grazing, label=grazing),
                          colour="darkred", hjust=-.25, size=4)+
  theme(text=element_text(size=15),
        axis.title.y = element_blank())


grazing<-q13[grep("coat", q13$text, ignore.case = T),]
corpus <- VCorpus(DataframeSource(coat[1:3,]))
corpus<-clean.corpus(corpus)
clean.tdm<-TermDocumentMatrix(corpus, control = list(weighting=weightTf))

f.m<-as.matrix(grazing.tdm)
f.adj<-f.m %*% t(f.m)
f.adj<-graph.adjacency(f.adj, weighted = TRUE, mode="undirected", diag = T)
f.adj<-simplify(f.adj)



plot.igraph(f.adj, vertex.shape="none",
            vertex.label.font=2, vertex.label.color="darkred", vertex.label.cex=.7, edge.color="gray85")
title(main="xxxxx")



word_network_plot(grazing$text[1:5])


word_associate(grazing$text, match.string = c('grazing'),
               stopwords = Top25Words, network.plot = T, cloud.colors = c('gray85', 'darkred'))


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
clusMember<-cutree(hc, 6)
labelColors<-c('darkgrey', 'darkred', 'black', '#bada55', "lightblue", "red")
clusDendro<-dendrapply(hcd, dend.change)
plot(clusDendro, main='title', type='triangle', yaxt='n')

hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)


set.seed(500)
wordcloud(freq.df$word, freq.df$frequency, max.words=100, colors=c('black','darkred'))

############################################Q14#######################################à
q14<-data.frame(doc_id=seq(1:nrow(df)),text=df$Q14)


custom.stopwords<-c(stopwords('english'),  "abnormal" , "horses",  "horse", "animal", "behaviour", "herd", "group", "presence", "behaviours", "excessive")

#custom.stopwords<-c(stopwords('english'))

corpus <- VCorpus(DataframeSource(q14))
corpus<-clean.corpus(corpus)

tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm.q14.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.q14.m)
freq.df<-data.frame(word=
                      names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


associations<-findAssocs(tdm,'weaving', 0.25)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=weaving), data=associations, size=3)+
  theme_gdocs()+geom_text(aes(x=weaving, label=weaving),
                          colour="darkred", hjust=-.25, size=4)+
  theme(text=element_text(size=15),
        axis.title.y = element_blank())


weaving<-q14[grep("coat", q14$text, ignore.case = T),]
corpus <- VCorpus(DataframeSource(coat[1:3,]))
corpus<-clean.corpus(corpus)
weaving.tdm<-TermDocumentMatrix(corpus, control = list(weighting=weightTf))

f.m<-as.matrix(weaving.tdm)
f.adj<-f.m %*% t(f.m)
f.adj<-graph.adjacency(f.adj, weighted = TRUE, mode="undirected", diag = T)
f.adj<-simplify(f.adj)



plot.igraph(f.adj, vertex.shape="none",
            vertex.label.font=2, vertex.label.color="darkred", vertex.label.cex=.7, edge.color="gray85")
title(main="xxxxx")



word_network_plot(weaving$text[1:5])


word_associate(weaving$text, match.string = c('weaving'),
               stopwords = Top25Words, network.plot = T, cloud.colors = c('gray85', 'darkred'))


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
clusMember<-cutree(hc, 6)
labelColors<-c('darkgrey', 'darkred', 'black', '#bada55', "lightblue", "red")
clusDendro<-dendrapply(hcd, dend.change)
plot(clusDendro, main='title', type='triangle', yaxt='n')

hcd<-color_labels(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
hcd<-color_branches(hcd, 4, col=c('#bada55', 'darkgrey', 'darkred', 'black'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)


set.seed(500)
wordcloud(freq.df$word, freq.df$frequency, max.words=30, colors=c('black','darkred'))










###################TIDY#######################
data("stop_words")
q10<-data_frame(doc_id=seq(1:nrow(df)),text=df$Q10)
my_stop_words<-rbind(c("horses",  "horse", "box", "min",
                       "manger", "mangers", "exits", "etc"), stop_words)

q10<-q10 %>%
  unnest_tokens(word, text)

q10 <-q10 %>% 
  anti_join(stop_words)
  
q10 %>% 
  count(word, sort = TRUE) 

library(widyr)

pairwise_count(word, sort = TRUE)





q10bgr<-q10 %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)

q10bgr %>% 
  count(ngram, sort = TRUE)

q10bsep<-q10bgr %>% 
separate(ngram, c("word1", "word2"), sep = " ")

q10bsepf <- q10bsep %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>% 
  count(word1, word2, sort = TRUE)# %>% 
  #unite(ngram, word1, word2, sep = " ")
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

my_stop_words<-rbind(c("horses",  "horse", "box", "min",
                       "manger", "mangers", "exits", "etc"), stop_words)




