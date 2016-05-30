
test <- read.csv('test.csv',header = TRUE)
install.packages('tm')
library(tm)   

test$product_title <-  gsub("<.*?>", "", test$product_title) 
test$product_title <-  gsub("&nbsp;", " ", test$product_title)
Corpus = Corpus(VectorSource(test$product_title)) 
Corpus <- tm_map(Corpus, removePunctuation)  
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))   
test_title_dataframe<-data.frame(text=unlist(sapply(Corpus, `[`, "content")), stringsAsFactors=F)
test$product_title=test_title_dataframe$text
test$product_title=tolower(test$product_title)

test$product_description <-  gsub("<.*?>", "", test$product_description) 
test$product_description <-  gsub("&nbsp;", " ", test$product_description)
Corpus = Corpus(VectorSource(test$product_description)) 
Corpus <- tm_map(Corpus, removePunctuation)  
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))   
test_description_dataframe<-data.frame(text=unlist(sapply(Corpus, `[`, "content")), stringsAsFactors=F)
test$product_description=test_description_dataframe$text
test$product_description=tolower(test$product_description)

test$query <-  gsub("<.*?>", "", test$query) 
test$query <-  gsub("&nbsp;", " ", test$query)
Corpus = Corpus(VectorSource(test$query)) 
Corpus <- tm_map(Corpus, removePunctuation)  
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))   
test_query_dataframe<-data.frame(text=unlist(sapply(Corpus, `[`, "content")), stringsAsFactors=F)
test$query=test_query_dataframe$text
test$query=tolower(test$query)

#install.packages('quanteda')
#library(quanteda) 

 
install.packages('stringdist')
library(stringdist)
#cosim
for (i in 1:length(test$query)){
  if(test$product_title[i]!=""){
    sim<-stringsim(a = test$query[i], b=test$product_title[i], method = "cosine", useBytes = FALSE, q = 2)
    test$sim_query_title[i]<-sim
  }else{
    test$sim_query_title[i]=0}
}
for (i in 1:length(test$query)){
  if(test$product_description[i]!="")
  { 
    sim<-stringsim(a = test$query[i], b=test$product_description[i], method = "cosine", useBytes = FALSE, q = 2)
    test$sim_query_description[i]<-sim
  }else{
    test$sim_query_description[i]=0}
}


#qgram
for (i in 1:length(test$query)){
  if(test$product_title[i]!="")
  {
    sim<-stringsim(a = test$query[i], b=test$product_title[i], method = "qgram", useBytes = FALSE, q = 4)
    test$gram_query_title[i]<-sim
  }else{
    test$gram_query_title[i]=0}
}
for (i in 1:length(test$query)){
  if(test$product_description[i]!="")
  { 
    sim<-stringsim(a = test$query[i], b=test$product_description[i], method = "qgram", useBytes = FALSE, q = 4)
    test$gram_query_description[i]<-sim
  }else{
    test$gram_query_description[i]=0}
}

install.packages('RecordLinkage')
library(RecordLinkage)

#levinshtein similarity feature between query to product_title
for (z in 1:length(test$query)){ 
  if(test$product_title[z]!="")
  {
    lv_sim <- levenshteinSim(test$query[z],test$product_title[z])
    test$simlv_query_title[z] <- lv_sim
    lv_distance <- levenshteinDist(test$query[z],test$product_title[z])
    test$dislv_query_title[z] <- lv_distance
  }
  else
  {
    test$simlv_query_title[z] = 0
    test$dislv_query_title[z] = 0
  }
} 

#levinshtein similarity and distance feature between query to product_description
for (z in 1:length(test$query)){ 
  if(test$product_description[z]!="")
  {
    lv_sim <- levenshteinSim(test$query[z],test$product_description[z])
    test$simlv_query_description[z] <- lv_sim
    lv_distance <- levenshteinDist(test$query[z],test$product_description[z])
    test$dislv_query_description[z] <- lv_distance
  }
  else
  {
    test$simlv_query_description[z] = 0
    test$dislv_query_description[z]= 0
  }
} 

#jaccard similarity feature between query to title
for (z in 1:length(test$query)){ 
  if(test$product_title[z]!="")
  {
    jac_sim <- stringsim(test$query[z],test$product_title[z],method='jaccard', q=4)
    test$simjac_query_title[z] <- jac_sim
  }
  else
  {
    test$simjac_query_title[z] = 0
  }
} 


#jaccard similarity feature between query to description
for (z in 1:length(test$query)){ 
  if(test$product_description[z]!="")
  {
    jac_sim <- stringsim(test$query[z],test$product_description[z],method='jaccard', q=4)
    test$simjac_query_description[z] <- jac_sim
  }
  else
  {
    test$simjac_query_description[z] = 0
  }
} 
install.packages('dplyr')
library (dplyr)

test_data_features<-select(test,gram_query_description, gram_query_title, sim_query_description, sim_query_title,
                   simlv_query_description, dislv_query_description, simlv_query_title, dislv_query_title,
                   simjac_query_description, simjac_query_title)


