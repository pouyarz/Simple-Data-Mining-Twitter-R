require(twitteR)
require(tm)
require(wordcloud)
require(ggplot2)

api_key <- ""                   #Input your info
api_secret <- ""                #Input your info
access_token <- ""              #Input your info
access_token_secret <- ""       #Input your info

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

sport<-searchTwitter("basketball OR volleyball OR hockey OR football OR handball OR skate OR skating OR judo OR hiking OR hike OR racquetball OR rock climbing OR soccer OR aerobics OR surfing OR swimming OR swim OR tennis OR soccer OR baseball OR skiing OR sky OR gymnastics OR dance OR dancing OR biking OR bike OR run OR running OR badminton OR fitness OR golf", n=5000, lang = "en" , resultType = "recent")

sport_text<-sapply(sport,function(x) x$getText())

sport_corpus<- Corpus(VectorSource(sport_text))

sport_clean<- tm_map(sport_corpus,removePunctuation)
sport_clean<- tm_map(sport_clean, content_transformer(tolower))
sport_clean<- tm_map(sport_clean, removeNumbers)
sport_clean<- tm_map(sport_clean, removeWords, stopwords("english"))
#removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
#sport_clean<- tm_map(sport_clean, removeURL)
sport_clean<- tm_map(sport_clean,stripWhitespace)
sport_clean<- tm_map(sport_clean, removeWords, c("amp","the","team","play","follow","sport","The","just",'the',"favorite","sports"))

wordcloud(sport_clean, random.order = F, scale = c(3, 0.5), max.words = 500, col=rainbow(50))

dtm <- TermDocumentMatrix(sport_clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

toFootball <- content_transformer(function (x , pattern ) gsub(pattern, "football", x))
sport_clean <- tm_map(sport_clean, toFootball, "soccer")

library(SnowballC) 
sport_clean <- tm_map(sport_clean, stemDocument) 
sport_clean <- tm_map(sport_clean, PlainTextDocument)

findFreqTerms(dtm, lowfreq = 100)

findAssocs(dtm, terms = "gym", corlimit = 0.3)


barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

p <- ggplot(subset(d, freq=4), aes(word, freq)) 
p <- p + geom_bar(stat="identity")   
p


