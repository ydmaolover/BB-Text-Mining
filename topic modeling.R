
library(topicmodels) #LDA
library(tidytext) #tokens
library(tidyverse) #cleaning, visualization
library(readxl) #input
library(tm) #dtm
library(reshape2)
library(RColorBrewer)
library(wordcloud)
library(broom)
#######################################################test_code

#Overall LDA
#data inport&clean
visits_2017 <- read_excel("visits_2017.xlsx")

df_remove_na <- filter(visits_2017, is.na(visits_2017$VisitNotes) == FALSE)


stop_words <- stopwords("SMART")

notes <- gsub("'", "", df_remove_na$VisitNotes)  # remove apostrophes
notes <- gsub("[[:punct:]]", " ", notes)  # replace punctuation with space
notes <- gsub("[[:cntrl:]]", " ", notes)  # replace control characters with space
notes <- gsub("^[[:space:]]+", "", notes) # remove whitespace at beginning of documents
notes <- gsub("[[:space:]]+$", "", notes) # remove whitespace at end of documents
notes <- tolower(notes)  # force to lowercase


#tokenize on space and output as a list:
note.list <- strsplit(notes, "[[:space:]]+")


# compute the table of terms:
note.table <- table(unlist(note.list))
note.table <- sort(note.table, decreasing = TRUE)


# remove terms that are stop words or occur fewer than 5 times:
del <- names(note.table) %in% stop_words | note.table < 5
note.table <- note.table[!del]
vocab <- names(note.table)


# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(note.list, get.terms)


# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (6736)
W <- length(vocab)  # number of terms in the vocab (1576)
doc.length <- sapply(documents, function(x) sum(x[2, ]))
N <- sum(doc.length)  # total number of tokens in the data (114,106)
term.frequency <- as.integer(note.table)


# MCMC and model tuning parameters:
K <- 5
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(2017)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 1.5 minutes



theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))


VisitNotes <- list(phi = phi,
                   theta = theta,
                   doc.length = doc.length,
                   vocab = vocab,
                   term.frequency = term.frequency)


library(LDAvis)
# create the JSON object to feed the visualization:
json <- createJSON(phi = VisitNotes$phi, 
                   theta = VisitNotes$theta, 
                   doc.length = VisitNotes$doc.length, 
                   vocab = VisitNotes$vocab, 
                   term.frequency = VisitNotes$term.frequency)

library(servr)
serVis(json, out.dir = 'vis', open.browser = TRUE)



##Word Cloud

top.topic.words(fit$topics, num.words = 20, by.score = TRUE)


a <- as.data.frame(fit$topics)
set.seed(1234)
wordcloud(words = colnames(a), freq = a[1,], min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



wordcloud(words = colnames(a), freq = a[3,], min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = colnames(a), freq = a[4,], min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



wordcloud(words = colnames(a), freq = a[2,], min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = colnames(a), freq = a[5,], min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


