####  Arthur Spirling
####  UTokyo, June 3--4, 2017
####  TEXT-AS-DATA
####  DAY 1, PART 2

rm(list=ls())

setwd("C:/Users/as9934/Dropbox/Japan_Text_Class/data/")


#################
#Getting Started#
#################

#install quanteda and readtext
library(quanteda)
library(readtext)
#install lsa if don't have it
#install.packages("lsa")
#we load lsa, just because it has a nice cosine function
# (write your own if you like!)
library(lsa)

#we'll also look at the burstiness of some terms if there is time
library(bursts)


#let's grab the UK manifestos and create a corpus
manifestos <- readtext("UK_manifestos/*.txt", docvarsfrom=c("filenames"))
manifestos_corpus <- corpus(manifestos)

#and let's grab SOTU too
sotu_corpus <- corpus(readtext("sotu/*.txt",docvarsfrom=c("filenames")) )

#############################
#Vector Space Representation#
#############################
#make a DFM
DTM <- dfm(manifestos_corpus, stem=T, remove=stopwords("english"))


#######################
# PROPERTIES OF TEXTS #
#######################

## ZIPF
#what are the most common terms used in the manifestos?
topfeatures(DTM)

#how are they distributed?
x11()
plot(topfeatures(DTM, n= ncol(DTM)))
#not very clear: let's look at log-log
x11()
par(pty="s") #do square plot -- easier to see relationship
plot(log(topfeatures(DTM, n= ncol(DTM))), log(1:ncol(DTM)), pch=16)

## HEAPS LAW
ntypes <- summary(manifestos_corpus, verbose=F)$Types
ntokens <- summary(manifestos_corpus, verbose=F)$Tokens

x11()
par(mfrow=c(2,1))
plot(ntokens, ntypes)
plot(log(ntokens), log(ntypes))

#reset plot style
par(pty="m")


#######################
# SIMILARITY MEASURES #
#######################


#example from lecture
doc1 <- c(5,4,3)
doc2 <- c(50,40,30)
doc3 <- c(3,3,4)

doc.mat <- rbind(doc1,doc2,doc3)

#euclidean distance
dist(doc.mat)

#cosine distance
# (note we are using LSA here -- we'll use quanteda in a minute)
cosine(doc1, doc2) #compare to euclidean

#how close were the Tory and Labour 1983 manifestos?
textstat_simil(DTM[c("Lab1983.txt","Con1983.txt"),], method='cosine')

#what about 1997?
textstat_simil(DTM[c("Lab1997.txt","Con1997.txt"),], method='cosine')
#NB: not exactly the same as lecture calcs, bec stemmed in slightly 
# different way -- but pretty close

#for completeness, let's look at Jaccard
textstat_simil(DTM[c("Lab1997.txt","Con1997.txt"),], method='jaccard')
#which should be a bit larger than
textstat_simil(DTM[c("Lab1983.txt","Con1983.txt"),], method='jaccard')

#########################
# Key Words in Context  #
#########################

kwic(manifestos_corpus, "socialism")
kwic(manifestos_corpus, "poll tax" )
kwic(manifestos_corpus,"community charge")

#####################
# LEXICAL DIVERSITY #
#####################

#take a look at the type-token ratio for the manifestos
types <- summary(manifestos_corpus, verbose=F)$Types
tokens <- summary(manifestos_corpus, verbose=F)$Tokens
TTR <- types/tokens
#let's plot them over time
dates <- as.numeric( gsub('[^[:digit:]]','',docnames(manifestos_corpus)) )
x11()
plot(dates, TTR, pch=16)
#seem to be getting less diverse!

#hmm, maybe this overstates things?
# take a look at Guiraud's Root measure
ntypes_G  <-  ntype(manifestos_corpus) 
ntokens_G <-  ntoken(manifestos_corpus) 

R <- ntypes_G/sqrt(ntokens_G)

x11()
plot(dates, R)
#looks p different!

###############
# READABILITY #
###############

FRE_manifestos <- textstat_readability(manifestos_corpus, measure='Flesch')
FRE_sotu <- textstat_readability(sotu_corpus, measure='Flesch')

x11()
plot(dates, FRE_manifestos, pch=16, ylim=c(20,80), xlim=c(1918, 2015))
#no obvious pattern (?)
#What about compared to SOTU?
years_sotu <- as.numeric(gsub(".txt","",gsub("su","",names(FRE_sotu))))#v inefficient
lines(years_sotu, FRE_sotu, col="red", lwd=2)

#what about other measures?
DC_manifestos <-textstat_readability(manifestos_corpus, measure="Dale.Chall")
cor(FRE_manifestos, DC_manifestos) #look similar in practice

#weird things can happen --
sentance <- "These include capital expenditures by the Rural Electrification 
Administration and expenditures for resource development by other 
organizational units in the Department of Agriculture 
which are also mentioned above under 'agricultural programs.' "

textstat_readability(sentance, measure='Flesch')
#hmm...

#################
# BOOTSTRAPPING #
#################

#probably more efficient ways to do this, but here's one idea
# First, write a function that bootstraps at the sentance level within a doc
boot_doc <- function(document=manifestos_corpus[1],nboot=200){
  #tokenize to the sent level
  toked_doc <- tokenize(document, what='sentence')  
  #make it into a corpus
  sent_corpus<-corpus(unlist(toked_doc))
  
  #set up a vector to take mean of bootstrap stats
  means <- c()
  
  #sample the sentences (with replacement)
  #do this nboot times
  for(i in 1:nboot){
    samp_corpus <- sent_corpus[sample(1: nrow(summary(sent_corpus, v=F)), replace=T)]
    #apply FRE to each of those, take mean
    FRE_mean <- mean(textstat_readability(samp_corpus, measure='Flesch' ))
    means <- c(means, FRE_mean)
    mean_FRE <- mean(means)
    FRE_lower <- quantile(means, c(0.025))
    FRE_upper <- quantile(means, c(0.975))
    cat("done",i,"of",nboot,"resamples\n")
  }
  c(FRE_lower, mean_FRE, FRE_upper)
}

#so, for example, 
boot_Lib1918 <- boot_doc(manifestos_corpus[47], nboot=500) 
#gives the mean and basic CI for the first Lib manifesto

#whereas
boot_Lib1997 <- boot_doc(manifestos_corpus[68], nboot=500)
#gives that info for the Libs in 1997

#NB: Libs in 1997 have much more variable sentence lengths, but
# the manifesto is longer, so we are generally more certain

######################
# BASIC STYLOMETRICS #
######################

#some texts by Austen
austen <- corpus(readtext("austen_texts/*.txt", docvarsfrom=c("filenames")) )
#some texts by Dickens
dickens <- corpus(readtext("dickens_texts/*.txt",  docvarsfrom=c("filenames")) )

#a mystery text
mystery <- corpus(readtext("mystery/*.txt" ))

#let's look at some key function words
# and make the DTMs with those in mind
# (from Peng and Hengartner)
func.words <- c('the', 'may', 'which', 'not', 'be', 'upon')


austen.dfm <- dfm(austen, select=func.words)
dickens.dfm <- dfm(dickens, select=func.words)
mystery.dfm <- dfm(mystery, select=func.words)

#then, inspect (takes means)
apply( austen.dfm/rowSums(as.matrix(austen.dfm)), 2, mean) 
#vs 
apply(dickens.dfm/rowSums(as.matrix(dickens.dfm)), 2, mean)
#vs
mystery.dfm/rowSums(as.matrix(mystery.dfm)) 
##--> who looks like most plausible author?


##############
# BURSTINESS #
##############
treaties <- readtext("treaties/*.txt", docvarsfrom=c("filenames"))
treaties_corpus <- corpus(treaties)

#grab the treaty dates
cases <- read.csv("treaties/universecases.csv")
date <- as.Date(as.character(cases$Date[1:365]), "%m-%d-%Y")
#put them on corpus
docvars(treaties_corpus)$Date <- date

DTM <- dfm(treaties_corpus)

#write a function to look at burstiness of given word
#this is a repurposing of some guts of kleinberg()
bursty<-function(word="sioux"){
  word.vec <- DTM[,which(colnames(DTM) == word)]
  word.times <- c(0,which(as.vector(word.vec)>0))
  kl <- kleinberg(word.times, gamma=.5)
  kl$start <- date[kl$start+1]
  kl$end <- date[kl$end]
  max_level <- max(kl$level)
  plot(c(kl$start[1], kl$end[1]), c(1,max_level), 
       type = "n", xlab = "Time", ylab = "Level", bty = "n", 
       xlim = c(kl$start[1], kl$end[1]), ylim = c(1, max_level), 
       yaxt = "n")
  axis(2, at = 1:max_level)
  arrows(kl$start, kl$level, kl$end, kl$level, code = 3, angle = 90, 
         length = 0.05)
  
  print(kl)
  #note deviation from standard defaults bec don't have that much data
}
