#"Crowdflower Search Results Relevance" competition

כחלק מקורס יישום שיטות לניתוח נתונים, לקחנו חלק בתחרות לדירוג רלוונטיות תוצאות חיפוש, בה צריך לכתוב אלגוריתם, החוזה רלוונטיות התוצאות החוזרות עבור שאילתות חיפוש. 
בעזרת סט האימון שקבלנו (המכיל פרטי שאילתת חיפוש, כותרת וטקסט המתאר את המוצר שחזר כתוצאת חיפוש) למדנו להעריך עד כמה המוצר רלוונטי לחיפוש.

הקוד מורכב ממספר חלקים: 

### 1. התקנת החבילות
`````
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
`````
### 2. טעינת המידע
קריאת קבצי נתונים. נשים לב כי עבור כל רשומה, קיים המידע הבא: פרטי שאילתת חיפוש, כותרת וטקסט המתאר את המוצר שחזר כתוצאת חיפוש.
`````
train <- read.csv("train.csv", header=TRUE)
test <- read.csv('test.csv',header = TRUE)
`````

### 3. ניקוי הנתונים וסידור הקלט.
תהליך זה הורכב ממספר שלבים
א. ניקוי הנתונים על ידי הסרת תגיות html  ו- &nbsp
ב. הכנסת הנתונים לקורפוס
ג. הסרת פיסוק, הסרת מילים נפוצות ושינוי טקסט לאותיות קטנות
מצורף דוגמא של הקוד המבצע זאת, אך תהליך זה בוצע עבור אימון ו-בדיקה ובכל אחד מהם, עבור פרטי שאילתת חיפוש, כותרת וטקסט המתאר את המוצר שחזר כתוצאת חיפוש
`````
#Clean data- remove html tags and &nbsp
test$product_title <-  gsub("<.*?>", "", test$product_title) 
test$product_title <-  gsub("&nbsp;", " ", test$product_title)

#enter data into corpus
Corpus = Corpus(VectorSource(test$product_title)) 
#remove punctuation, stopwords, and lower the letter
Corpus <- tm_map(Corpus, removePunctuation)  
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))   
test_title_dataframe<-data.frame(text=unlist(sapply(Corpus, `[`, "content")), stringsAsFactors=F)
test$product_title=test_title_dataframe$text
test$product_title=tolower(test$product_title)
`````

### 4. יצירת תכונות
הוספנו את התכונות הבאות, תוך בחינת קשר בין כותרת מוצר לתיאור מוצר:
א. cosim 
ב. qgram
ג. levinshtein
ד. jaccard
`````
#cosim feature between query to product_title and product_description
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

#qgram feature between query to product_title and product_description
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


#levinshtein feature between query to product_title and product_description
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

#jaccard feature between query to product_title and product_description
for (z in 1:length(test$query)){ 
  if(test$product_title[z]!="")
  {
    jaccard_sim <- stringsim(test$query[z],test$product_title[z],method='jaccard', q=4)
    test$simjaccard_query_title[z] <- jaccard_sim
  }
  else
  {
    test$simjaccard_query_title[z] = 0
  }
} 
for (z in 1:length(test$query)){ 
  if(test$product_description[z]!="")
  {
    jaccard_sim <- stringsim(test$query[z],test$product_description[z],method='jaccard', q=4)
    test$simjaccard_query_description[z] <- jaccard_sim
  }
  else
  {
    test$simjaccard_query_description[z] = 0
  }
} 
`````
### 5. סינון עמודות לא רלוונטיות
`````
test_data_features<-select(test,gram_query_description, gram_query_title, sim_query_description, sim_query_title,
                   simlv_query_description, dislv_query_description, simlv_query_title, dislv_query_title,
                   simjaccard_query_description, simjaccard_query_title)
`````

### 6. חזוי תוצאות עבור הבדיקה על ידי אלגוריתם random Forest
`````
fit <- randomForest(median_relevance~., data=train_data_features, importance=TRUE, ntree=2000)
summary(fit)
predictions <- predict(fit, test_data_features)
submit_data <- read.csv("test.csv", header=TRUE)
submit_data <- select(submit_data,id)
submit_data["prediction"] <- predictions
write.csv(submit_data, file = "Submission1.csv")
`````
