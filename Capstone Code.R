##  Libraries
library(LaF)
library(tm)
library(SnowballC)
library(RWeka)

##  Set the seed
set.seed(2020)

##  Save the first 10000 lines of data in the environment
sBlogs   <- sample_lines('./final/en_US/en_US.blogs.txt',   10000)
sNews    <- sample_lines('./final/en_US/en_US.news.txt',    10000)
sTwitter <- sample_lines('./final/en_US/en_US.twitter.txt', 10000)

profane  <-read.table("bad-words.txt", header=FALSE, sep="\n", strip.white=TRUE)

##  Concat all three vectors into Sample
sData    <- c(sBlogs, sNews, sTwitter)
rm(sBlogs, sNews, sTwitter)

sData    <- iconv(sData, 'latin1', 'ascii', sub = '')

gc()
##  Cleaning up the data
sData <- gsub("http[[:alnum:]]*",'', sData)
sData <- gsub('http\\S+\\s*', '', sData)     ## Remove URLs
sData <- gsub('\\b+RT', '', sData)           ## Remove RT
sData <- gsub('#\\S+', '', sData)            ## Remove Hashtags
sData <- gsub('@\\S+', '', sData)            ## Remove Mentions
sData <- gsub('[[:cntrl:]]', '', sData)      ## Remove Controls and special characters
sData <- gsub("\\d", '', sData)              ## Remove Controls and special characters
sData <- gsub('[[:punct:]]', '', sData)      ## Remove Punctuations
sData <- gsub("\r", "", sData)               ## remove carriage return
sData <- gsub("-", "", sData)                ## remove hypens
sData <- gsub("^[[:space:]]*","",sData)      ## Remove leading whitespaces
sData <- gsub("[[:space:]]*$","",sData)      ## Remove trailing whitespaces
sData <- gsub(' +',' ',sData)                ## Remove esDatatra whitespaces

sCorp <- VCorpus(VectorSource(sData))

sCorp <- tm_map(sCorp, PlainTextDocument)
sCorp <- tm_map(sCorp, content_transformer(tolower))
sCorp <- tm_map(sCorp, removeNumbers)
sCorp <- tm_map(sCorp, removeWords, profane[,1])

rm(profane)

gc()
##  Creating Ngrams

### Unigram
dtm <- DocumentTermMatrix(sCorp)
dtm <- removeSparseTerms(dtm, 0.998)
mat <- as.matrix(dtm)
rm(dtm)

freq <- sort(colSums(mat), decreasing = T)
rm(mat)
word <- names(freq)
unigram <- data.frame('word' = word, 'freq' = freq)
rm(word)

gc()
### Bigram
bigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min=2, max=2))}
dtm <- DocumentTermMatrix(sCorp, control = list(tokenize = bigramTokenizer))
dtm <- removeSparseTerms(dtm, 0.999)
mat <- as.matrix(dtm)
rm(dtm)

freq    <- sort(colSums(mat), decreasing = T)
rm(mat)
words   <- names(freq)
split   <- strsplit(words, ' ')
bigram  <- data.frame('word1' = sapply(split, function (x) {x[1]}), 
                      'word2' = sapply(split, function (x) {x[2]}), 
                      'freq'  = freq)

### Trigram
trigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}
dtm <- DocumentTermMatrix(sCorp, control = list(tokenize = trigramTokenizer))
dtm <- removeSparseTerms(dtm, 0.9996)
mat <- as.matrix(dtm)
rm(dtm)

freq    <- sort(colSums(mat), decreasing = T)
rm(mat)
words   <- names(freq)
split   <- strsplit(words, ' ')
trigram <- data.frame('word1' = sapply(split, function (x) {x[1]}), 
                       'word2' = sapply(split, function (x) {x[2]}), 
                       'word3' = sapply(split, function (x) {x[3]}), 
                       'freq'  = freq)

### Quadgram
quadgramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 4, max = 4))}
dtm <- DocumentTermMatrix(sCorp, control = list(tokenize = quadgramTokenizer))
dtm <- removeSparseTerms(dtm, 0.99985)
mat <- as.matrix(dtm)
rm(dtm)

freq     <- sort(colSums(mat), decreasing = T)
rm(mat)
words    <- names(freq)
split    <- strsplit(words, ' ')
quadgram <- data.frame('word1' = sapply(split, function (x) {x[1]}), 
                       'word2' = sapply(split, function (x) {x[2]}), 
                       'word3' = sapply(split, function (x) {x[3]}), 
                       'word4' = sapply(split, function (x) {x[4]}), 
                       'freq'  = freq)

rm(split, words, sData, sCorp, freq)
gc()
##  Save the Ngrams
rownames(unigram)   <- c()
rownames(bigram)    <- c()
rownames(trigram)   <- c()
rownames(quadgram)  <- c()


saveRDS(unigram,  file = 'unigram.rds')
saveRDS(bigram,   file = 'bigram.rds')
saveRDS(trigram,  file = 'trigram.rds')
saveRDS(quadgram, file = 'quadgram.rds')
