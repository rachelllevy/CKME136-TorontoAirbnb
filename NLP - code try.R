#Install Packages

install.packages("tm")
install.packages("text2vec")
install.packages("SnowballC")
install.packages("slam")
library(tm)
library(text2vec)
library(SnowballC)
library(slam)

#Create Corpus

char_variables <- c("Name","Description","Host Verifications","Amenities","Geolocation","Features","Host Neighbourhood")

Corpus_desc <- Corpus(VectorSource(cleaned_data$Description))
print(Corpus_desc)
inspect(Corpus_desc[1:3])

N.docs <- length(Corpus_desc)


#Clean Corpus

Corpus_desc_clean <- tm_map(Corpus_desc, tolower)
Corpus_desc_clean <- tm_map(Corpus_desc_clean, removeNumbers)
Corpus_desc_clean <- tm_map(Corpus_desc_clean, removePunctuation)
Corpus_desc_clean <- tm_map(Corpus_desc_clean, removeWords, stopwords())
Corpus_desc_clean <- tm_map(Corpus_desc_clean, stripWhitespace)
Corpus_desc_clean <- tm_map(Corpus_desc_clean, stemDocument)


#Create a TDM applying TF-IDF weighting instead of term frequency

tdm = TermDocumentMatrix(Corpus_desc_clean,
                         control = list(weighting = weightTfIdf))

#Reduce sparsity of TDM

tdm2 <- removeSparseTerms(tdm, 0.7)

#Inspect subset of TDM

inspect(tdm2[1:10,1:10])

#Analyze how frequently terms appear by summing contents of all rows

freq = rowSums(as.matrix(tdm2))
head(freq)

#See the ten most frequent terms

tail(sort(freq), n = 10)

# toronto     park  bedroom     walk   subway    close    minut    apart     room    place 
#134.2890 134.3740 135.1740 139.0573 142.8792 151.4949 157.2093 162.0840 168.1166 233.8662

