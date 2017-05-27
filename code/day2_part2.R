####  Arthur Spirling
####  UTokyo, June 3--4, 2017
####  TEXT-AS-DATA
####  DAY 2, PART 2

rm(list=ls())

setwd("C:/Users/arthur spirling/Dropbox/Japan_Text_Class/data/")


#################
#Getting Started#
#################

#install quanteda and readtext
library(quanteda)
library(readtext)

#let's grab the UK manifestos and create a corpus
manifestos <- readtext("UK_manifestos/*.txt", docvarsfrom=c("filenames"))
manifestos_corpus <- corpus(manifestos) 
docnames(manifestos_corpus) <- manifestos$doc_id

#and let's grab SOTU too
sotu <- readtext("sotu/*.txt",docvarsfrom=c("filenames"))
sotu_corpus <- corpus(sotu)
docnames(sotu_corpus) <- sotu$doc_id



#we'll also need ldadtuning and topicmodels, later
library(topicmodels)
library(ldatuning)


########################
# PRINCIPAL COMPONENTS #
########################
#make a dfm
DFM<- dfm(manifestos_corpus, remove_punct=TRUE) 
#do basic psa - note we have to transpose the matrix
basic_pca<-prcomp(t(as.matrix(DFM))) 

#looks like first PC does most of the lifting
x11()
screeplot(basic_pca)

#let's grab that first PCA and plot it
first.pca <- basic_pca$rotation[,1]
x11()

#make sure points are colored intutiively
#grab party names
ps <- gsub("[0-9].*", "", rownames(DFM))
cols <- ps #make color vector
cols[cols=="Con"] <- 'blue'
cols[cols=="Lib"] <- 'gold'
cols[cols=='Lab'] <- 'red'

#let's plot them over time
dates <- as.numeric( gsub('[^[:digit:]]','',docnames(manifestos_corpus)) )
plot(dates, first.pca, col=cols, pch=16, cex=2)

#some overtime variation...
#...but doesn't seem to be a v good ideological measurement!

##############
# CLUSTERING #
##############

#patterns we see in pca, we often see in clustering...
dist.matrix <- dist(DFM)
x11()
plot( hclust(dist.matrix) )
#looks fairly time series-y
# Con1992 look like an outlier for some reason

############
# WORDFISH #
############
#we'll use Lab83 as example of left manifesto
# and Con83 as example of right manifesto
#...otherwise use standard defaults
wordfish.model <- textmodel_wordfish(DFM, dir=c(42,19))

#let's take a look at 1d model results
x11()
textplot_scale1d(wordfish.model)
#hmm, looks a time series effect

#maybe this is an artifact of Liberal manifestos
#let's redo, just using post war Lab and Con ones
DFM2 <- DFM[c(8:23, 31:46),]
wordfish.model2 <- textmodel_wordfish(DFM2, dir=c(28,12))
x11()
textplot_scale1d(wordfish.model2)
#hmm, still looks very time series dependent
#(note that quanteda will drop a bunch of terms not in new DFM)

# let's look at the word fixed effects
# recall that words with high fixed effects are used a lot
# ... but need not be informative
fixed.effects <- wordfish.model2@psi
names(fixed.effects) <- wordfish.model2@features
sort(fixed.effects, decreasing=T)[1:15]

#let's look at word weights (betas)
weights <- wordfish.model2@beta
names(weights) <- wordfish.model2@features
#let's get the biggest and smallest ones
print(head(sort(weights), 10))
print(tail(sort(weights), 10))
#not particularly informative!  might want to get rid of numbers!

#'eiffel tower' plot
x11()
plot(weights, fixed.effects)


######################
# TOPIC MODELS: LDA  #
######################

#we'll just work with the Labour and Conservative post-war data
#we'll be a little more aggressive about preprocessing -- get rid of stopwords too
DFM3 <- dfm(DFM2, remove=c(stopwords()))

#let's start with basic 10 topic model, using LDA
 # (takes 2 or 3 minutes)
lda.model <- LDA(DFM3, k=10)

#let's get the estimated topic mix of each document...
topicProbabilities <- as.data.frame(lda.model@gamma) 
#not that rowSums(topicProbabilities) is as expected

#now, let's grab the terms and see what topic they were assigned to
termassignments <- as.data.frame(t(posterior(lda.model)$terms))
#for example, what topic(s) would we expect to find words in
x11()
par(mfrow=c(1,2))
barplot(as.numeric(termassignments["taxation",])) #everywhere
barplot(as.numeric(termassignments["1919",])) #one place

#let's try to label the topics
topTerms <- terms(lda.model, 6)
#hmm, may be helpful, may be not

#let's find out how many topics is optimal for our model (just check 2 to 15)
# this take a while -- 8 minutes or so
how.many.topics <- FindTopicsNumber(DFM3,  topics = seq(from = 2, to = 15, by = 1))


#plot the result (larger number is better in this metric)
x11()
FindTopicsNumber_plot(how.many.topics)


