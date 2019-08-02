library(MASS)
library(ISLR)
library(dplyr)
library(GGally)
library(ggplot2)
library(forecast)
library(readr)
library(naniar)

library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(biclust)
library(cluster)
library(igraph)
library(fpc)

#Read file
Seattledf <- read_csv("seattle.csv")
colnames(Seattledf)
str(Seattledf)

#Select predictors
selectedcln <- which(colnames(Seattledf) %in% c("id", "name","summary","space","description","neighborhood_overview",
                                             "notes","transit","host_about","host_verifications","amenities",
                                             "neighbourhood_group_cleansed"))
QueenAnnedf <- Seattledf[,selectedcln]
View(QueenAnnedf)

#Filter data with assigned neighbourhood "Queen Anne"
colnames(QueenAnnedf)
QueenAnnedf = dplyr::filter(QueenAnnedf, neighbourhood_group_cleansed == "Queen Anne")

#Text mining analysis
QueenAnnecorpus <- Corpus(VectorSource(QueenAnnedf [2:9]))
QueenAnnecorpus <- tm_map(QueenAnnecorpus, removePunctuation)
QueenAnnecorpus <- tm_map(QueenAnnecorpus, removeNumbers)
QueenAnnecorpus <- tm_map(QueenAnnecorpus, tolower)
QueenAnnecorpus <- tm_map(QueenAnnecorpus, removeWords, stopwords('english'))
QueenAnnecorpus <- tm_map(QueenAnnecorpus, removeWords, c("seattle","room", "anne","queen","bed","bedroom",
                                                          "neighborhood","apart","home","live","will","one",
                                                          "kitchen","just","live","house","apartment","floor",
                                                          "can","also","floor"))
QueenAnnecorpus <- tm_map(QueenAnnecorpus, stemDocument)
#QueenAnnecorpus <- tm_map(QueenAnnecorpus, PlainTextDocument)
View(QueenAnnecorpus)

tdm1<- DocumentTermMatrix(QueenAnnecorpus, control = list(weighting = weightTf, stopwords = FALSE))
tdm1 <- removeSparseTerms(tdm1, .99)
freq <- colSums(as.matrix(tdm1)) 
df <- data.frame(word=names(freq), freq=freq)
dffreq <- subset(df, freq>=350)

ggplot(aes(x=word, y=freq), data = dffreq)+
  geom_bar(stat = "identity")+
  coord_flip()

pal2 <- brewer.pal(8,"Set2")
wordcloud(QueenAnnecorpus, min.freq=10,
          max.words=100, random.order=F, rot.per=.3, colors=pal2)

