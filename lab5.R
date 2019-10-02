install.packages("dplyr")
install.packages("readtext")
install.packages("quanteda")
library(quanteda)
library(dplyr)
library(readtext)

#Análisis exploratorio a los datos, utilizando blogs.txt como muestra

blogs<-readtext::readtext("blogs.txt")

#Creamos un corpus de los datos 
c1<-corpus(blogs)
c1_test<-c1


#Utilizamos la funcion para tokenizar nuestro corpues, removemos números, signos de puntuación, separamos por palabra, removemos por ejemplo self-love a "self" y "love" 
c1_test<-tokens(c1_test, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, verbose = TRUE)


# creación de un document-feature matrix (dft) para eliminar stopword en ingles y dejar palabras raíz
dfm1_test <- dfm(c1_test, tolower = TRUE, remove = stopwords("english"), stem = FALSE, verbose = TRUE)

#Las 20 palabras mas frecuentes 
topfeatures(dfm1_test, 20) 


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


#----------------------- Unigramas ----------------------------

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

#-------------------------- digramas ---------------------------

tbldiGrm<-data.frame(table(parse.corpus(myCrps, ngram.size = 2)))
#tbltriGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 3)))

stbldigrm<-tbldiGrm[order(tbldiGrm$Freq, decreasing = TRUE),]


top20dig<-stbldigrm[1:20,]
colnames(top20dig)<-c("DiGram","Frequency")


ggplot (top20dig, aes(x = reorder(DiGram, - Frequency), y= Frequency )) + 
  geom_bar( stat = "Identity" , fill = "lightblue" ) +  
  geom_text( aes (label = Frequency ) , vjust = - 0.20, size = 3 ) +
  xlab( "DiGramas" ) +
  ylab( "Frequencia" ) +
  theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) )

#------------------- trigramas ------------------------------
tbltriGrm<-data.frame(table(parse.corpus(myCrps, ngram.size = 3)))

stbltrigrm<-tbltriGrm[order(tbltriGrm$Freq, decreasing = TRUE),]


top20trig<-stbltrigrm[1:20,]
colnames(top20trig)<-c("TriGram","Freq")


ggplot (top20trig, aes(x = reorder(TriGram, - Freq), y= Freq )) + 
  geom_bar( stat = "Identity" , fill = "lightblue" ) +  
  geom_text( aes (label = Freq ) , vjust = - 0.20, size = 3 ) +
  xlab( "DiGramas" ) +
  ylab( "Frequencia" ) +
  theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) )


########## predicciones ###########



#Implementacion de  algoritmo kneser-Ney por John O. Bonsak en April, 2016
KNprep <- function(dt, n) {
  # Takes a data.table coming from ngramprep() and adds (Mod.) Kneser-Ney-discounting of the freqs
  #
  # Args: 
  #    dt: a data.table as ngramprep() serves it, requires Terms, Nextword and Freq as minimum cols
  #    n:  the ngram level (one dt per ngram level 1-4) 
  # Returns:
  #    dt: The prepared data table now also suitable for Modified Kneser-Ney prediction
  #        as described by Chen & Goodman (1999)
  
  col <- colnames(dt)
  col <- col[1:n]
  
  
  # Y = N_c / (N_c + 2N_(c+1)), where N_c is the count of ngrams with count==c (for Kneser-Ney)
  #     Note: This is a simplified calculation of Y using only the two lowest kept freqs.
  c1 <- min(dt$Freq) # The lowest available Freq
  c2 <- min(subset(dt, Freq>c1)$Freq) # And the second lowest
  Y <- nrow(dt[Freq == c1]) / (nrow(dt[Freq == c1]) + 2 * nrow(dt[Freq == c2])) # 1:0.50 2:0.56 3+:0.62  
  
  # D = Discounting parameter different for freq==1, freq==2 and freq>=3
  #     Ref Goodman and Chen (1999)
  dt[, D := 0]
  dt[Freq == 1]$D <- 1 - 2 * Y * (nrow(dt[Freq == 2]) / nrow(dt[Freq == 1]))
  dt[Freq == 2]$D <- 2 - 3 * Y * (nrow(dt[Freq == 3]) / nrow(dt[Freq == 2]))
  dt[Freq > 2]$D  <- 3 - 4 * Y * (nrow(dt[Freq == 4]) / nrow(dt[Freq == 3]))
  
  # Nom = First nominator in P_KN formula ( max{c(w_i-1, w_i)-D, 0} )
  dt <- dt[, Nom := pmax(Freq-D, 0)]
  
  # Denom = Denominator is the count of the preceding word(s) 
  if(n==1) {
    dt <- dt[, Denom := sum(Freq)]
  } else if (n==2) {
    dt <- dt[, .(w, Freq, D, Nom, Denom = sum(Freq)), by = w1]
  } else if (n==3) {
    dt <- dt[, .(w, Freq, D, Nom, Denom = sum(Freq)), by = list(w2, w1)]
  } else if (n==4) {
    dt <- dt[, .(w, Freq, D, Nom, Denom = sum(Freq)), by = list(w3, w2, w1)]
  }
  
  
  # NN = number of word types that follows w_i-1 in the training data
  if(n==1) {
    dt <- dt[, .(w, Freq, D, Nom, Denom, NN = length(w))]
  } else if (n==2) {
    dt <- dt[, .(w, Freq, D, Nom, Denom, NN = length(w)), by=w1]
  } else if (n==3) {
    dt <- dt[, .(w, Freq, D, Nom, Denom, NN = length(w)), by=list(w2, w1)]
  } else if (n==4) {
    dt <- dt[, .(w, Freq, D, Nom, Denom, NN = length(w)), by=list(w3, w2, w1)]
  }
  
  
  # L  = Lambda, normalizing constant, the probability mass we've discounted
  dt[, L := (D / Freq) * NN]
  
  # N = The number of different ngrams this nextword completes in training set 
  #     (c(w_(i-1)) for Kneser-Ney). Used in P_continutation (PC)
  if(n==1) {
    dt <- dt[, .(w, Freq, D, Nom, Denom, NN, L, .N)]
  } else if (n==2) {
    dt <- dt[, .(w1, Freq, D, Nom, Denom, NN, L, .N), by=w]
  } else if (n==3) {
    dt <- dt[, .(w2, w1, Freq, D, Nom, Denom, NN, L, .N), by=w]
  } else if (n==4) {
    dt <- dt[, .(w3, w2, w1, Freq, D, Nom, Denom, NN, L, .N), by=w]
  }
  
  
  # PC = P_continuation
  dt[, PC := N / nrow(dt)] # Count of this novel continuation div. by number of unique grams
  
  # Prob_KN - Estimated KN probability
  dt[, P_KN := (Nom/Denom) + ((D/Denom) * NN) * PC]
}

s_count<-nrow(myCrps)
clean_sample <- sample(myCrps, 10* 0.01)
KNprep(top20trig, 2)

