####  Arthur Spirling
####  UTokyo, June 3--4, 2017
####  TEXT-AS-DATA
####  DAY 1, PART 1

rm(list=ls())

setwd("C:/Users/arthur spirling/Dropbox/Japan_Text_Class/data/")


#################
#Getting Started#
#################

#install quanteda if don't already have it
#install.packages("quanteda")
library(quanteda)

#and let's grab readtext
#install.packages("readtext")
library(readtext)


#install lsa if don't have it
#install.packages("lsa")
#we load lsa, just because it has a nice cosine function
# (write your own if you like!)
library(lsa)


#let's grab the UK manifestos
# the argument docvarsfrom= allows us to use the filenames as document ids 
manifestos <- readtext("UK_manifestos/*.txt", docvarsfrom=c("filenames"))

# We need to turn the text files into a 'corpus' so that we can do more 
# interesting things with them
manifestos_corpus <- corpus(manifestos)
docnames(manifestos_corpus) <- manifestos$doc_id

# let's get a summary of this corpus
summary(manifestos_corpus)

#let's inspect the Labour 1983 manifesto
texts(manifestos_corpus)[manifestos_corpus$documents$docvar1=="Lab1983"]
#hmm, quite a bit of annoying mark up.  We'll deal with that later...

#For now: how long was the 'longest suicide note in history'? (in sentences)
summary(corpus_subset(manifestos_corpus, docvar1=="Lab1983"), verbose=F )$Sentence

#what about the Tory manifesto that election?
summary(corpus_subset(manifestos_corpus, docvar1=="Con1983"), verbose=F )$Sentence

#are manifestos getting longer or shorter over time?
#well, let's get the number of sentences for each manifesto
num_sentences <- summary(manifestos_corpus, verbose=F)$Sentence

#need to grab the dates of the manifestos. We could have added that via docvars()<-
#but let's just do a bit of regex, so you've seen it at work
dates <- as.numeric( gsub('[^[:digit:]]','', summary(manifestos_corpus, v=F)$docvar1) )

dev.new() #just to force a new plot window (-- I think this works on a mac too?)
plot(dates, num_sentences, pch=16)
#looks like they're generally getting heftier.

#hmm, how about SOTU speeches?
sotu <- readtext("sotu/*.txt",docvarsfrom=c("filenames"))
sotu_corpus <- corpus(sotu)
docnames(sotu_corpus) <- sotu$doc_id
#summary(sotu_corpus)

#############################
#Vector Space Representation#
#############################

#let's make a document feature matrix
#AKA a document term matrix

#first, let's look at the options
?dfm 
# hmm, quite a lot of different stuff we could do!

#to see this stuff at work, let's grab the Labour 1983 manifesto and 
# work on it directly for a moment
Lab_1983 <- corpus_subset(manifestos_corpus, docvar1=="Lab1983")

#### TOKENS

#one of the things dfm does is 'tokenize'
# ?tokenize
#for now, let's use that directly just to see how it works.
#So, 
tokens_all <- tokenize(Lab_1983) #this defaults to the /word/ level
#how many  are there?
length(tokens_all[[1]]) #~25.5k
#what about _unique_ tokens (basically the 'types' in this context)
length(unique(tokens_all[[1]])) #~3.6k


#let's not bother with punctuation: not very helpful generally
tokens_nopunc <- tokenize(Lab_1983, remove_punct=TRUE)
#can check
length(tokens_nopunc[[1]]) #~22.5k



#and maybe we could remove numbers too: probably don't care about them either
tokens_nopunc_nonum <- tokenize(Lab_1983, remove_punct=TRUE, remove_numbers=FALSE)
#can check
length(tokens_nopunc_nonum[[1]]) #~22.4k

#what if we wanted bigrams (only) instead of unigrams?
tokens_bigrams <- tokenize(Lab_1983, ngrams= 2)

#### STEMS

#let's stem the document (uses Porter)
stemmed <- tokens_wordstem(tokens_all) #can inspect directly, if desired.
# to see what's happened in practice, let's compare 'family' and 'families'
# So, compare
tokens_all[[1]][grep("fam",tokens_all[[1]])]
# to
stemmed[[1]][grep("fam",stemmed[[1]])]

#### STOPWORDS

#what are they?
stopwords("english")
#and for that matter
stopwords("german")
# etc
# we'll use these in a minute...

##### MAKING A VECTOR
Lab1983_vector <- dfm(Lab_1983, stem=TRUE, remove=stopwords("english"), remove_punct=TRUE, remove_numbers=TRUE  )
#can take a look at it in longform via (well, first 10 terms at least)
as.matrix(Lab1983_vector, colnames=colnames(Lab1983_vector))[1, 1:10]

#alright, let's make a dfm of our whole collection
DTM <- dfm(manifestos_corpus, stem=T, remove=stopwords("english"),
           remove_punct=TRUE)
#what proportion of it is zeros?
sum(DTM==0)/length(DTM) #so, q a bit!

#we can use different weights if we like.  e.g. tfidf
DTM_tfidf <- tfidf(DTM)
#interesting to compare
topfeatures(DTM)
#with
topfeatures(DTM_tfidf)
#hmm, what happened here?

