#
# Fit an LDA topic model to data using tm and lda packages
#
# Herb Susmann
# September 2014
#

library(tm)
library(lda)
library(plyr)

# Settings
documents.file <- "documents.csv"
num.topics <- 10

# Read in documents
# Expects a CSV with a column "contents" which has the contents of each document, one per row
documents <- read.csv(documents.file, stringsAsFactors=FALSE)

# Clean dataset
corpus <- Corpus(VectorSource(documents$contents))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument, language="english", mc.cores=1)

# Convert to character vector for input into LDA
lines <- sapply(1:length(corpus), function(i) corpus[[i]]$content)

docs <- lexicalize(lines)

# Fit topic model
result <- lda.collapsed.gibbs.sampler(docs$documents, 10, docs$vocab, 25, 0.1, 0.1, compute.log.likelihood=TRUE)

# Find the top words associated with each topic
top.words <- top.topic.words(result$topics, 10, by.score=TRUE)

cat("Top words for each topic")
print(top.words)


top.documents <- top.topic.documents(result$document_sums, num.documents = 10)
top.documents.contents <- t(aaply(top.documents, 2, function(col) documents$contents[col]))

cat("Top documents for each topic")
print(top.documents)

