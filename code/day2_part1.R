####  Arthur Spirling
####  UTokyo, June 3--4, 2017
####  TEXT-AS-DATA
####  DAY 2, PART 1

rm(list=ls())

setwd("C:/Users/as9934/Dropbox/Japan_Text_Class/data/")


#################
#Getting Started#
#################

#install quanteda and readtext
library(quanteda)
library(readtext)
#let's grab the UK manifestos and create a corpus
manifestos <- readtext("UK_manifestos/*.txt", docvarsfrom=c("filenames"))
manifestos_corpus <- corpus(manifestos)
#and let's grab SOTU too
sotu_corpus <- corpus(readtext("sotu/*.txt",docvarsfrom=c("filenames")) )


########################
####### DICTIONARIES  ##
########################


##############################
# LAVER AND GARRY DICTIONARY #
##############################

#load the dictionary
LG_dictionary <- dictionary(file="dictionaries/LaverGarry.cat", format = "wordstat")

#make a dfm based on the dictionary
DTM_LGdict <- dfm(manifestos_corpus, dictionary=LG_dictionary)

#let's look at the 1983 manifestos specifically (Con, Lab)
DTM_dict_1983 <- DTM_LGdict[c("Con1983.txt","Lab1983.txt"),]
#who talks more about ethnic groups and women?
# who talks more about 'conservative' values?

################################
# Regressive Imagery dictionary#
#primordial/conceptual thinking#
################################
RID_dictionary <- dictionary(file="dictionaries/RID.cat",format = "wordstat")

#make a dfm based on the dictionary
DTM_RIDdict <- dfm(manifestos_corpus, dictionary=RID_dictionary)

#has politics become more aggressive over time?
aggro <-  DTM_RIDdict[,"EMOTIONS.AGGRESSION" ]
dates <- as.numeric( gsub('[^[:digit:]]','',docnames(manifestos_corpus)) )

#let's color the points 'correctly'
#grab party names
ps <- gsub("[0-9].*", "", rownames(aggro))
cols <- ps #make color vector
cols[cols=="Con"] <- 'blue'
cols[cols=="Lib"] <- 'gold'
cols[cols=='Lab'] <- 'red'

#plot
x11()
plot(dates, aggro, col='black', bg=cols, pch=22, cex=1.5)
low <- lowess(aggro~dates, f=2/3)
lines(low$x, low$y, col="black", lwd=2)
#Cold War effect? more polarized politics?


##############################
# sENTIMENT USING HU and LIU #
##############################
pos<-read.table("dictionaries/positive-words.txt", as.is=T)
neg <-read.table("dictionaries/negative-words.txt", as.is=T)

#function just to do simply arithmatic
sentiment<-function(words=c("really great good stuff bad")){
  require(quanteda)
  tok <- tokenize(words)
  pos.count <- sum(tok[[1]]%in%pos[,1])
  cat("\n positive words:",tok[[1]][which(tok[[1]]%in%pos[,1])],"\n")
  
  neg.count <- sum(tok[[1]]%in%neg[,1])
  cat("\n negative words:",tok[[1]][which(tok[[1]]%in%neg[,1])],"\n\n")
  out <- (pos.count - neg.count)/(pos.count+neg.count)
  out
}

#review examples
movie <-  "Director and co-screenwriter Adam McKay (Step Brothers) bungles a great 
opportunity to savage the architects of the 2008 financial crisis in The Big Short, 
wasting an A-list ensemble cast in the process. Steve Carell, Brad Pitt, Christian 
Bale and Ryan Gosling play various tenuously related members of the finance industry,
men who made made a killing by betting against the housing market, which at that point
had superficially swelled to record highs. All of the elements are in place for a 
lacerating satire, but almost every aesthetic choice in the film is bad, 
from the U-Turn-era Oliver Stone visuals to Carell's sketch-comedy performance
to the cheeky cutaways where Selena Gomez and Anthony Bourdain explain complex 
financial concepts. After a brutal opening half, it finally settles into a groove, 
and there's a queasy charge in watching a credit-drunk America walking towards that 
cliff's edge, but not enough to save the film."

yelp<- "this guy mat the owner is a scam do not use him you will regret doing business 
with this company I'm going to court he is a scam customers please beware he 
will destroy your floors he is nothing by a liar he robs customers, 
and promises you everything if you want s--- then go with him if you like nice work 
find another he is A SCAM LIAR BULL----ER,"

wolfshirt <-"This item has wolves on it which makes it intrinsically sweet and worth
5 stars by itself, but once I tried it on, that's when the magic happened. 
After checking to ensure that the shirt would properly cover my girth, 
I walked from my trailer to Wal-mart with the shirt on and was immediately 
approached by women. The women knew from the wolves on my shirt that I, 
like a wolf, am a mysterious loner who knows how to 'howl at the moon' 
from time to time (if you catch my drift!). The women that approached
me wanted to know if I would be their boyfriend and/or give them money for 
something they called mehth. I told them no, because they didn't have enough teeth,
and frankly a man with a wolf-shirt shouldn't settle for the first thing that comes
to him."

################
# NAIVE BAYES ##
################

#?textmodel_NB
email1 <- "money inherit prince"
email2 <- "prince inherit amount"
email3 <- "inherit plan money"
email4 <- "cost amount amazon"
email5 <-  "prince william news"
newemail <- "prince prince money"

trainingset<- dfm(c(email1, email2, email3, email4, email5, newemail))

trainingclass <- factor(c("spam", "spam", "ham", "ham", "ham" , NA), ordered = T)
## replicate example from lecture
NB_model <- textmodel_NB(trainingset, trainingclass, smooth=0, prior=c("docfreq"))
predict(NB_model, newdata = trainingset[6, ])

##problem of sparsity: suppose new email includes term never seen in spam
newemail2 <- "plan inherit prince"
trainingset2<- dfm(c(email1, email2, email3, email4, email5, newemail2))
trainingclass2 <- factor(c("spam", "spam", "ham", "ham", "ham" , NA), ordered = T)
NB_model2 <- textmodel_NB(trainingset2, trainingclass2, smooth=0, prior=c("docfreq"))
predict(NB_model2, newdata = trainingset2[6, ])
##oh dear... can 'correct' this by adding smooth=1
NB_model3 <- textmodel_NB(trainingset2, trainingclass2, smooth=1, prior=c("docfreq"))
predict(NB_model3, newdata = trainingset2[6, ])

################
# WORD SCORES  #
################

#use the 1983 and 1987 manifestos and left-right training cases
#--> see if we can predict Labour 1997 manifesto
dfm<- dfm(manifestos_corpus, verbose=FALSE)
dfm_5 <- dfm[c("Con1983.txt", "Con1987.txt", "Lab1983.txt", "Lab1987.txt", "Lab1997.txt")]

#rescale the scores, and provide a prediction for Lab1997...
predicted_97 <- predict( textmodel_wordscores(dfm_5, y=c(1, 1, -1,-1,  NA), smooth=1), rescaling="lbg" )



