require(twitteR)
require(tm)
require(wordcloud)

api_key <- "6DH6eDvGWnLnnRfL10y3Lb6hM"
api_secret <- "CI8Jtn2JR56I0VpW6DqCJwYd8k0H0AjmdugM4N69Ijy4qub4KH"
access_token <- ""
access_token_secret <- ""
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

sport<-searchTwitter("basketball OR volleyball OR hockey OR football OR handball OR skate OR skating OR judo OR hiking OR hike OR racquetball OR rock climbing OR soccer OR aerobics OR surfing OR swimming OR swim OR tennis OR soccer OR baseball OR skiing OR sky OR gymnastics OR dance OR dancing OR biking OR bike OR run OR running OR badminton OR fitness OR golf", n=5000, lang = "en" , resultType = "recent")
sport_text<-sapply(sport,function(x) x$getText())
str(sport_text)
#convert tweets to corpus out of vector
sport_corpus<- Corpus(VectorSource(sport_text))
sport_corpus
inspect(sport_corpus[10])
#the tm_map() function is used to remove unnecessary
#clean_up
sport_clean<- tm_map(sport_corpus,removePunctuation)
sport_clean<- tm_map(sport_clean, content_transformer(tolower))
sport_clean<- tm_map(sport_clean, removeNumbers)
sport_clean<- tm_map(sport_clean, removeWords, stopwords("en"))
#removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
#sport_clean<- tm_map(sport_clean, removeURL)
sport_clean<- tm_map(sport_clean,stripWhitespace)
sport_clean<- tm_map(sport_clean, removeWords, c("amp","the","team","play","follow","sport","The","just",'the',"favorite","sports"))
#Generate the Word cloud
wordcloud(sport_clean, random.order = F, scale = c(3, 0.5), max.words = 100, col=rainbow(50))
#The frequency table of words
# Building a term-document matrix,Document matrix is a table containing the frequency of the words. Column names are words and row names are documents. 
dtm <- TermDocumentMatrix(sport_clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

toFootball <- content_transformer(function (x , pattern ) gsub(pattern, "football", x))
sport_clean <- tm_map(sport_clean, toFootball, "soccer+Soccer")

#we want to find words that occur at least four times
findFreqTerms(dtm, lowfreq = 4)
#analyze the association between frequent terms (i.e., terms which correlate) using findAssocs() function. The R code below identifies which words are associated with 
findAssocs(dtm, terms = "gym", corlimit = 0.3)

mytdm <- TermDocumentMatrix(sport_clean, control=list(wordLengths=c(1,Inf)))
findFreqTerms(mytdm,lowfreq = 4)
termFrequency <- rowSums(as.matrix(mytdm))
termFrequency <- subset(termFrequency, termFrequency>10)

#The frequency of the first 10 frequent words are plotted 
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
require(ggplot2)
p <- ggplot(subset(d, freq=10), aes(word, freq)) 
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

dtmss <- removeSparseTerms(d, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss) 

library(cluster)   
d <- dist(t(d), method="euclidian")   
fit <- hclust(d=d, method="ward")   
fit  
library(SnowballC) 
sport_clean <- tm_map(sport_clean, stemDocument)   # *Removing common word endings* (e.g., "ing", "es")   
sport_clean <- tm_map(sport_clean, stripWhitespace)   # *Stripping whitespace   
sport_clean <- tm_map(sport_clean, PlainTextDocument) # *This is the end of the preprocessing stage.*  
