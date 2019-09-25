

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


dfm1<-dfm(c1)
topfeatures(dfm1, 20) # 20 most frequent words
