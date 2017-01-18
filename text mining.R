# Load
library("tm")            # for text mining
library("SnowballC")     # for text stemming
library("wordcloud")     # word-cloud generator 
library("RColorBrewer")  # color palettes

# load the text
# Read the text file
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

# Load the data as a corpus
# The text is loaded using Corpus() function from text mining (tm) package. Corpus is a list of a document (in our case, we only have one document).
docs <- Corpus(VectorSource(text))
# VectorSource() function creates a corpus of character vectors
# The content of the document can be inspected as follow :
  
inspect(docs)

# Text transformation
# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing "/", "@" and "|" with space

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#Cleaning the text

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming ==> package 'SnowballC'
docs <- tm_map(docs, stemDocument)

# Build a term-document matrix
# Document matrix is a table containing the frequency of the words. Column names are words and row names are documents, from text mining package
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)




