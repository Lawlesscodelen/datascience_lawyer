#Códigos de várias ramas del derecho: https://www.boe.es/biblioteca_juridica/index.php?tipo=C
#########Wordcloud##################
#####Instalamos y cargamos las librerías que nos hacen falta
install.packages("tm")
install.packages("wordcloud")
install.packages("pdftools")
install.packages("dplyr")
library(tm)
library(wordcloud)
library(pdftools)
library(dplyr)


#Cargamos el documento donde vamos a hacer el wordcloud 
dt = pdf_text("C:/Users/eleni/downloads/CodigoPenal.pdf", opw = "" , upw = "")

#Pasamos el texto entero a Corpus para que sea leíble.
docs <- Corpus(VectorSource(dt))


#Empezamos a limpiar: quitamos números, puntuaciones y espaciados extras
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
         
# Convertimos el texto todo a minúscula 
docs <- tm_map(docs, content_transformer(tolower))

# Quitamos palabras frecuentes del lenguaje de español 
docs <- tm_map(docs, removeWords, stopwords("spanish"))

#Si queremos quitar palabaras adicionales:
#docs <- tm_map(docs, removeWords, c("artículo","penal","código"))

#Convertimos el texto en una matriz con la frecuencia de las palabras
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
r <- sort(rowSums(m),decreasing=TRUE)
f <- data.frame(word = names(r),freq=r)

#Vemos las 10 primeras palabras 
head(f, 10)

#Generamos el wordcloud o la nube de palabras
set.seed(1234)
wordcloud(words = f$word, freq = f$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.20, 
          colors=brewer.pal(8, "Dark2"))


#Vemos la frecuencia de palabras 
findFreqTerms(dtm, lowfreq = 0)


