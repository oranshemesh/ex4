setwd("C:\\data_ofrit\\4\\")
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies=TRUE)   
install.packages('stringdist')
install.packages('stringdist')
install.packages('dplyr')
install.packages('randomForest')
install.packages('party')
install.packages('caret')
install.packages('Metrics')
install.packages('RecordLinkage')
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source") 
library(stringdist)
library(stringdist)
library (dplyr)
library(randomForest)
library(party)
library(caret)
library(Metrics)
library(RecordLinkage)
library(tm)   

train <- read.csv("train.csv", header=TRUE)
train$product_title <-  gsub("<.*?>", "", train$product_title) 
train$product_title <-  gsub("&nbsp;", " ", train$product_title)
Corpus = Corpus(VectorSource(train$product_title)) 
Corpus <- tm_map(Corpus, removePunctuation)  
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))   
train_title_dataframe<-data.frame(text=unlist(sapply(Corpus, `[`, "content")), stringsAsFactors=F)
train$product_title=train_title_dataframe$text
train$product_title=tolower(train$product_title)

train$product_description <-  gsub("<.*?>", "", train$product_description) 
train$product_description <-  gsub("&nbsp;", " ", train$product_description)
Corpus = Corpus(VectorSource(train$product_description)) 
Corpus <- tm_map(Corpus, removePunctuation)  
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))   
train_description_dataframe<-data.frame(text=unlist(sapply(Corpus, `[`, "content")), stringsAsFactors=F)
train$product_description=train_description_dataframe$text
train$product_description=tolower(train$product_description)

train$query <-  gsub("<.*?>", "", train$query) 
train$query <-  gsub("&nbsp;", " ", train$query)
Corpus = Corpus(VectorSource(train$query)) 
Corpus <- tm_map(Corpus, removePunctuation)  
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))   
train_query_dataframe<-data.frame(text=unlist(sapply(Corpus, `[`, "content")), stringsAsFactors=F)
train$query=train_query_dataframe$text
train$query=tolower(train$query)



#cosim
for (i in 1:length(train$query)){
  if(train$product_title[i]!=""){
    sim<-stringsim(a = train$query[i], b=train$product_title[i], method = "cosine", useBytes = FALSE, q = 2)
    train$sim_query_title[i]<-sim
  }else{
    train$sim_query_title[i]=0}
}
for (i in 1:length(train$query)){
  if(train$product_description[i]!="")
  { 
    sim<-stringsim(a = train$query[i], b=train$product_description[i], method = "cosine", useBytes = FALSE, q = 2)
    train$sim_query_description[i]<-sim
  }else{
    train$sim_query_description[i]=0}
}

R.Version()
#qgram
for (i in 1:length(train$query)){
  if(train$product_title[i]!="")
  {
    sim<-stringsim(a = train$query[i], b=train$product_title[i], method = "qgram", useBytes = FALSE, q = 4)
    train$gram_query_title[i]<-sim
  }else{
    train$gram_query_title[i]=0}
}
for (i in 1:length(train$query)){
  if(train$product_description[i]!="")
  { 
    sim<-stringsim(a = train$query[i], b=train$product_description[i], method = "qgram", useBytes = FALSE, q = 4)
    train$gram_query_description[i]<-sim
  }else{
    train$gram_query_description[i]=0}
}

#levinshtein similarity feature between query to product_title
for (z in 1:length(train$query)){ 
  if(train$product_title[z]!="")
  {
    lv_sim <- levenshteinSim(train$query[z],train$product_title[z])
    train$simlv_query_title[z] <- lv_sim
    lv_distance <- levenshteinDist(train$query[z],train$product_title[z])
    train$dislv_query_title[z] <- lv_distance
  }
  else
  {
    train$simlv_query_title[z] = 0
    train$dislv_query_title[z] = 0
  }
} 

#levinshtein similarity and distance feature between query to product_description
for (z in 1:length(train$query)){ 
  if(train$product_description[z]!="")
  {
    lv_sim <- levenshteinSim(train$query[z],train$product_description[z])
    train$simlv_query_description[z] <- lv_sim
    lv_distance <- levenshteinDist(train$query[z],train$product_description[z])
    train$dislv_query_description[z] <- lv_distance
  }
  else
  {
    train$simlv_query_description[z] = 0
    train$dislv_query_description[z]= 0
  }
} 

#jaccard similarity feature between query to title
for (z in 1:length(train$query)){ 
  if(train$product_title[z]!="")
  {
    jac_sim <- stringsim(train$query[z],train$product_title[z],method='jaccard', q=4)
    train$simjac_query_title[z] <- jac_sim
  }
  else
  {
    train$simjac_query_title[z] = 0
  }
} 


#jaccard similarity feature between query to description
for (z in 1:length(train$query)){ 
  if(train$product_description[z]!="")
  {
    jac_sim <- stringsim(train$query[z],train$product_description[z],method='jaccard', q=4)
    train$simjac_query_description[z] <- jac_sim
  }
  else
  {
    train$simjac_query_description[z] = 0
  }
} 


train_data_features<-select(train,gram_query_description, gram_query_title, sim_query_description, sim_query_title,
                   simlv_query_description, dislv_query_description, simlv_query_title, dislv_query_title, 
                   simjac_query_description, simjac_query_title, median_relevance)



#train_data_features<-select(train,gram_query_description, gram_query_title, sim_query_description, sim_query_title, median_relevance)
#test_data_features<-select(test,gram_query_description, gram_query_title, sim_query_description, sim_query_title)




fit <- randomForest(median_relevance~., data=train_data_features, importance=TRUE, ntree=2000)
summary(fit)
predictions <- predict(fit, test_data_features)
submit_data <- read.csv("test.csv", header=TRUE)
submit_data <- select(submit_data,id)
submit_data["prediction"] <- predictions
write.csv(submit_data, file = "Submission1.csv")



train_data_features$median_relevance=as.factor(train_data_features$median_relevance)
model2 <- train(median_relevance ~., data = train_data_features, method = "rpart", trControl = trainControl(classProbs = F))
pred <- predict(model2, newdata = test_data_features)
submit_data <- read.csv("test.csv", header=TRUE)
submit_data<- select(submit_data,id)
submit_data["prediction"] <- predict
write.csv(submit_data, file = "Submission.csv")
