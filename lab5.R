

install.packages("dplyr")
install.packages("readtext")
install.packages("quanteda")
library(quanteda)
library(dplyr)
library(readtext)

blogs<-readtext::readtext("blogs.txt")

c1<-corpus(blogs)
c1_test<-c1

#summary(c1)


#mayusculas a minusculas (Tarda un monton, no lo corran)
#tolower(c1_test)


#Eliminacion de cosas
c1_test<-tokens(c1_test, remove_numbers = TRUE, remove_punct = TRUE, remove_separators = TRUE)


# make a dfm, removing stopwords and applying stemming
dfm1_test <- dfm(c1_test,remove = stopwords("english"),stem = TRUE, remove_punct = TRUE)


dfm1<-dfm(c1_test)
topfeatures(dfm1_test, 20) # 20 most frequent words


########################################################################################################################


blg<- file("blogs.txt", open="rb")
blDat<- readLines(blg, encoding="latin1")
close(blg)

nws<- file("news.txt", open="rb")
nwsDat<- readLines(nws, encoding="latin1")
close(nws)

twts<- file("twitter.txt", open="rb")
twtsDat<- readLines(twts, encoding="latin1")
close(twts)

####### WORD COUNT #########
blgWordCnt<- sum((nchar(blDat) - nchar(gsub(' ','',blDat))) + 1)
blgLinesCnt<-NROW(blDat)

nWsWordCnt<- sum((nchar(nwsDat) - nchar(gsub(' ','',nwsDat))) + 1)
nwsLinesCnt<-NROW(nwsDat)

twtWordCnt<- sum((nchar(twtsDat) - nchar(gsub(' ','',twtsDat))) + 1)
twtLinesCnt<-NROW(twtsDat)

docProperties<-matrix(c(blgWordCnt, nWsWordCnt, twtWordCnt, blgLinesCnt, nwsLinesCnt, twtLinesCnt), nrow=3, ncol=2)
rownames(docProperties)<- c("Blogs","News","Twitter")
colnames(docProperties)<- c("# of words","# of lines")
barplot(docProperties[,1], main="Numero de Palabras",
        xlab="data", col=c("orange","red", "lightblue"),
        log="y", beside=TRUE)

############################ PREPARACION DE LOS DATOS #################################

# 10% DE LOS DATOS 
blogSamp<-sample(blDat, blgLinesCnt* 0.1)
newsSamp<-sample(nwsDat,nwsLinesCnt * 0.1)
twtsSamp<-sample(twtsDat,twtLinesCnt * 0.1)

##Combine samples to work with

combSamp<- c(blogSamp, newsSamp, twtsSamp)

# remove words with non-ASCII characters - 

# convert string to vector of words
spSamp<- unlist(strsplit(combSamp, split=", "))

# find indices of words with non-ASCII characters
nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))

# subset original vector of words to exclude words with non-ACCII characters
ascVec<- spSamp[ - nonAscIDX]

# convert vector back to string
ascSamp<- paste(ascVec, collapse = ", ")
#remove numbers and punctuation

clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)

######## CORPUS (TM) #########
library(tm)
SampCrps<- Corpus(VectorSource(clnSamp))

# mayusculas a minisculas
SampCrps<- tm_map(SampCrps, tolower)

# eliminar signos de puntuacion
SampCrps<- tm_map(SampCrps, removePunctuation)

# eliminar numeros
SampCrps<- tm_map(SampCrps, removeNumbers)

# PATRONES A ELIMINAR
# URLs
urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
SampCrps<-tm_map(SampCrps, urlPat)

# Emails
emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
SampCrps<- tm_map(SampCrps, emlPat)

# Twitter RTs, via, etc
tt<-function(x) gsub("RT |via", "", x)
SampCrps<- tm_map(SampCrps, tt)

# Usuarios de Twitter 
tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
SampCrps<- tm_map(SampCrps, tun)

#White Space
SampCrps<- tm_map(SampCrps, stripWhitespace)

#Stop words
SampCrps<-tm_map(SampCrps, removeWords, stopwords("english"))

#### N-GRAMAS ####
install.packages("stylo")
library(stylo)

myCrps<- txt.to.words(SampCrps)

# se crean data frames para el unigrama, digrmama y trigrama

tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
#tbldiGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 2)))
#tbltriGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 3)))

stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]


top20unig<-stblUnigrm[1:20,]
colnames(top20unig)<-c("UniGram","Frequency")

library(ggplot2)

ggplot (top20unig, aes(x = reorder(UniGram, - Frequency), y= Frequency )) + 
  geom_bar( stat = "Identity" , fill = "lightblue" ) +  
  geom_text( aes (label = Frequency ) , vjust = - 0.20, size = 3 ) +
  xlab( "UniGramas" ) +
  ylab( "Frequencia" ) +
  theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) )

