install.packages("twitteR")
require(twitteR) # provides access to twitter data
#text mining, clean-up
install.packages("tm")
require(tm) # provides fundtion for text mining
# The most used keywords stand out better in a word cloud. Word clouds are a potent communication tool. They are easy to understand, to be shared and are impactful
install.packages("wordcloud")
require(wordcloud) # visualizes result with cloud 
api_key <- "6DH6eDvGWnLnnRfL10y3Lb6hM"
api_secret <- "CI8Jtn2JR56I0VpW6DqCJwYd8k0H0AjmdugM4N69Ijy4qub4KH"
access_token <- ""
access_token_secret <- ""
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
getTrends(woeid = 23424977)
sport<-searchTwitter("basketball OR volleyball OR judo OR hockey OR football OR marathon OR handball OR skate OR skating OR judo OR jiuJitsu OR jujitsu OR  OR lacrosse OR hiking OR hike OR racquetball OR rapelling OR rock climbing OR soccer OR aerobics OR surfing OR swimming OR swim", n=742, lang = "en" , resultType = "recent")
#shows type
class(sport)
#conver to charector vector(just taking text)
sport_text<-sapply(sport,function(x) x$getText())
str(sport_text)
#convert tweets to corpus out of vector
sport_corpus<- Corpus(VectorSource(sport_text))
sport_corpus
inspect(sport_corpus[10])
#clean_up
sport_clean<- tm_map(sport_corpus,removePunctuation)
sport_clean<- tm_map(sport_clean, content_transformer(tolower))
sport_clean<- tm_map(sport_clean, removeNumbers)
#c("","")
sport_clean<- tm_map(sport_clean, removeWords, stopwords("en"))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
sport_clean<- tm_map(sport_clean, removeURL)
sport_clean<- tm_map(sport_clean, removeWords, c("you","can","something","sport","like","field"))
sport_clean<- tm_map(sport_clean,stripWhitespace)

# Stemming
sport_clean <- tm_map(sport_clean, stemCompletion, dictionary=sport_clean)

# Replacing
# word cloud
wordcloud(sport_clean)
# randon=f(in center), scale(max $ min font size)
wordcloud(sport_clean, random.order = F, scale = c(3, 0.5), max.words = 100, col=rainbow(50))

# Building a term-document matrix
dtm <- TermDocumentMatrix(sport_clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
