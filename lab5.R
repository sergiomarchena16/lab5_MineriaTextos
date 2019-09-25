

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
