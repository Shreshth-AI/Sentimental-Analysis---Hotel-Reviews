

#### ---------------------- [ Part : 2 - Hotel Reviews ] ------------------ ####
# Load Hotel Data with customer online reviews for hotels and their corresponding ratings
hotel_data <- read.csv("HotelsData.csv")

# Observe the hotel data
head(hotel_data)
tail(hotel_data)

# Set seed to Roll No. generating a random number to be used further
set.seed(788)



# Installing required Packages and Loading Library
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("tidytext")
library(tidytext)
install.packages("textcat")
library(textcat)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("textdata")
library(textdata)
install.packages("tm")
library(tm)
install.packages('lda')
library(lda)
install.packages("topicmodels")
library(topicmodels)
install.packages("reshape2")
library(reshape2)

# Test set sample of 2000 observations
sample <- sample_n(hotel_data, 2000)

# Mean of Review Score of the Test Set
sample %>% summarize(stars_mean = mean(Score))
# Arrange in Descending Order
sample %>% arrange(desc(Score))

# Detect Languages in Reviews Available
# Store language in other column to analyse
sample$Language <- textcat(sample$Review)

# Store and count the number of languages in the reviews
language_counts <- sample %>%
  group_by(Language) %>%
  summarise(count = n())

# Print the output
print(language_counts)

# Store filtered sample after deleting other languages from the sample
Fsample <- sample[grepl("english", sample$Language, ignore.case = TRUE), ]
count(Fsample)
# Store and count the number of languages in the reviews of filtered Sample
language_counts2 <- Fsample %>%
  group_by(Language) %>%
  summarise(count = n())

# Print the output
print(language_counts2)

# Remove the language column from the Filtered Sample
Fsample <- subset(Fsample, select = c("Score", "Review"))

# Mean of Review Score of the Test Set
Fsample %>% summarize(stars_mean = mean(Score))
# Arrange in Descending Order
Fsample %>% arrange(desc(Score))

# Number of words and frequency in the test set and arranging in descending order
tidy_sample <- Fsample %>% unnest_tokens(word, Review)
tidy_sample %>% count(word) %>% arrange(desc(n))
# Reorder the tokenize words for the graph plot
word_count <- tidy_sample %>% count(word) %>% filter(n>200) %>% 
  mutate(word =fct_reorder(word,n))
# Plotting the word count filtered
ggplot(word_count, aes(x=word, y=n)) + geom_col() + coord_flip() + ggtitle("Review Word Counts")
wordcloud(words = word_count$word,
          freq = word_count$n,
          rot.per=0.15,
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))

# Removing stop words from test set by anti joining common words and frequency
tidy_sample2 <- Fsample %>% unnest_tokens(word, Review) %>% anti_join(stop_words)
tidy_sample2 %>% count(word) %>% arrange(desc(n))

# Storing word count in a variable and Plotting the words
# Reorder the graph plot
word_count2 <- tidy_sample2 %>% count(word) %>% filter(n>200) %>% 
  mutate(word =fct_reorder(word,n))
#Plotting the word count filtered
ggplot(word_count2, aes(x=word, y=n)) + geom_col() + coord_flip() + ggtitle("Review Word Counts")
wordcloud(words = word_count2$word,
          freq = word_count2$n,
          rot.per=0.15,
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))

# Removing unnecessary words using tribble fucntion
custom_stop_words <- tribble(
  ~word, ~lexicon,
  "1",   "CUSTOM",
  "2",   "CUSTOM",
  "3",   "CUSTOM",
  "4",   "CUSTOM",
  "5",   "CUSTOM",
)

# Bind with preset custom word
stop_words2 <-stop_words %>% bind_rows(custom_stop_words)
# Removing other stop words from test set by anti joining common words and frequency
tidy_sample3 <- Fsample %>% unnest_tokens(word, Review) %>% anti_join(stop_words2)
# Storing arranged word count for plotting 
word_count3 <- tidy_sample3 %>% count(word) %>% filter(n>300) %>% 
  mutate(word = fct_reorder(word,n))
# Plot Again
ggplot(word_count3, aes(x=word, y=n)) + geom_col() + coord_flip() + ggtitle("Review Word Counts")


# Word Cloud
wordcloud(words = word_count3$word,
          freq = word_count3$n,
          rot.per=0.15,
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))

# Sentiment Analysis for our data set using Bing
sentiment_review <- tidy_sample3 %>% inner_join(get_sentiments("bing"))
# Total no. of data set available
count(sentiment_review)
# Types of emotions and there freq.
sentiment_review %>% count(sentiment)
# Types of emotions, related words and there freq.
sentiment_review %>% count(word,sentiment) %>% arrange(desc(n))

# Word count 
word_sa1 <- sentiment_review %>% 
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=10) %>%
  ungroup() %>% 
  mutate(word2 = fct_reorder(word,n))

# Plot Sentiment Analysis
ggplot(word_sa1, aes(x = word2, y = n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + facet_wrap(~ sentiment, scales = "free") + 
  coord_flip() + labs(title = "Sentiment Words Counts using BING", x = "Words", y = "Frequency")

# Advanced
tidy_sample3 %>% inner_join(get_sentiments("bing")) %>% 
  count(Score, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n)
# Overall Sentiment
SR <- tidy_sample3 %>% inner_join(get_sentiments("bing")) %>% 
  count(Score, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  mutate(overall_sentiment = positive - negative,
         Score = fct_reorder(as.factor(Score), overall_sentiment))
SR
# Plot for overall sentiment
ggplot(SR, aes(x = Score, 
               y = overall_sentiment, 
               fill = as.factor(Score))) + 
  geom_col(show.legend = FALSE) + 
  coord_flip() + 
  labs(title = "Overall Sentiments by Review", 
       subtitle = "Review for hotels", 
       x = "Review Star", 
       y = "Overall Sentiment")

##### --------------------- [ TOPIC MODELLING ] --------------------- ######
# Install Necessary Packages
install.packages("ldatuning")
library(ldatuning)
install.packages("LDAvis")
library(LDAvis)
install.packages('servr') 
library(servr)

# Separate Samples based on scores
positive_data <- Fsample[Fsample$Score %in% c(4, 5), ]
negative_data <- Fsample[Fsample$Score %in% c(1, 2), ]
# - [ Used for Validation Later ] - #
neutral_data <- Fsample[Fsample$Score == 3, ]
summary(positive_data)

# Create sentiment labels
Fsample$Sentiment <- ifelse(Fsample$Score >= 4, "positive",
                            ifelse(Fsample$Score <= 2, "negative", "neutral"))

# Separate corpora
positive_corpus <- Fsample[Fsample$Sentiment == "positive", "Review"]
negative_corpus <- Fsample[Fsample$Sentiment == "negative", "Review"]
# - [ Used for Validation Later ] - #
neutral_corpus <- Fsample[Fsample$Sentiment == "neutral", "Review"]

# Document Term Matrix - Positive Corpus
p_dtm <- DocumentTermMatrix(positive_corpus,
                            control = list(lemma=TRUE,removePunctuation = TRUE,
                                           removeNumbers = TRUE, stopwords = TRUE,
                                           tolower = TRUE))

# Removing the documents with zero tokens
raw.sum = apply(p_dtm,1,FUN=sum)
p_dtm = p_dtm[raw.sum!=0,]

# Converting data matrix into matrix format
p_dtm <- as.matrix(p_dtm)

# Frequency Table
p_frequency <- colSums(p_dtm)
p_frequency <- sort(p_frequency, decreasing=TRUE)
p_doc_length <- rowSums(p_dtm)

# Top 30 Words
p_frequency[1:20]

# Get the Words
p_words <- names(p_frequency)

# Word-Cloud
wordcloud(p_words[1:100], p_frequency[1:50], rot.per=0.15, 
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))

# Inverse Document Frequency for positive corpus
p_idf <- DocumentTermMatrix(positive_corpus,
                            control = list( weighting =  weightTfIdf,
                                            removePunctuation = TRUE, 
                                            removeNumbers = TRUE, stopwords =  TRUE, 
                                            tolower = TRUE, wordLengths=c(1,Inf)))

p_freq = data.frame(sort(colSums(as.matrix(p_idf)), decreasing=TRUE))
# Wordcloud for IDF
wordcloud(rownames(p_freq), p_freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

head(p_freq)
tail(p_freq)

# Positive topic Modelling

p_result <- FindTopicsNumber(
  p_dtm,
  topics = seq(from = 5, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
# Find Number of Topics
FindTopicsNumber_plot(p_result)

# LDA using GIBBS
p_ldaOut <-LDA(p_dtm,19, method="Gibbs", 
               control=list(iter=1000,seed=1000))

# matrix -  distribution over terms for a topic
p_phi <- posterior(p_ldaOut)$terms %>% as.matrix 
# matrix -  the probability distribution over topics
p_theta <- posterior(p_ldaOut)$topics %>% as.matrix 

# Topic, Terms and Probabilities
pldaOut.terms <- as.matrix(terms(p_ldaOut, 10))
pldaOut.topics <- data.frame(topics(p_ldaOut))
pldaOut.topics$index <- as.numeric(row.names(pldaOut.topics))

# As index for our Data sets are different we will copy index from pldaout.topics data set
positive_data$index <- as.numeric(row.names(pldaOut.topics))
p_datawithtopic <- merge(positive_data, pldaOut.topics, by='index',all.x=TRUE)
p_datawithtopic <- p_datawithtopic[order(p_datawithtopic$index), ]
p_datawithtopic[0:10,]

# Calculating Probabilty
ptopicProbabilities <- as.data.frame(p_ldaOut@gamma)
ptopicProbabilities[0:10,1:5]

# create the JSON object to feed the visualization in LDAvis:

pvocab <- colnames(p_phi) #vocab list in DTM
json_lda <- createJSON(phi = p_phi, theta = p_theta, 
                       vocab = pvocab, doc.length = p_doc_length, 
                       term.frequency = p_frequency)

serVis(json_lda, out.dir = 'vis', open.browser = TRUE)

# POSITIVE TOP 3 TOPICS
# Sum Of probabilities of all topics in Positive Matrix
top_positive <- colSums(as.matrix(posterior(p_ldaOut)$topics))/sum(posterior(p_ldaOut)$topics)
names(top_positive) <- colnames(ptopicProbabilities)
top3_positive <- sort(top_positive, decreasing = TRUE)[1:3]
top3_positive

###############################################################################
# Document Term Matrix - Negative Corpus
ne_dtm <- DocumentTermMatrix(negative_corpus,
                             control = list(lemma=TRUE,removePunctuation = TRUE,
                                            removeNumbers = TRUE, stopwords = TRUE,
                                            tolower = TRUE))

# Removing the documents with zero tokens
raw.sum = apply(ne_dtm,1,FUN=sum)
ne_dtm = ne_dtm[raw.sum!=0,]
#Converting data matrix into matrix format
ne_dtm <- as.matrix(ne_dtm)

# Frequency Table
ne_frequency <- colSums(ne_dtm)
ne_frequency <- sort(ne_frequency, decreasing=TRUE)
ne_doc_length <- rowSums(ne_dtm)

ne_frequency[1:20]

library(wordcloud)
ne_words <- names(ne_frequency)# get back the word

wordcloud(ne_words[1:100], ne_frequency[1:40], rot.per=0.15, 
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))

# Inverse Document Frequency for negative corpus
ne_idf <- DocumentTermMatrix(negative_corpus,
                             control = list( weighting =  weightTfIdf, removePunctuation = TRUE,
                                             removeNumbers = TRUE, stopwords =  TRUE, 
                                             tolower = TRUE, wordLengths=c(1,Inf)))

ne_freq = data.frame(sort(colSums(as.matrix(ne_idf)), decreasing=TRUE))

wordcloud(rownames(ne_freq), ne_freq[,1], max.words=30, colors=brewer.pal(1, "Dark2"))
head(ne_freq)
tail(ne_freq)


# Negative topic Modelling
ne_result <- FindTopicsNumber(
  ne_dtm,
  topics = seq(from = 5, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
# Topic Numbers
FindTopicsNumber_plot(ne_result)

# LDA 
ne_ldaOut <- LDA(ne_dtm,13, method="Gibbs", 
                 control=list(iter=1000,seed=1000))

# matrix -  distribution over terms for a topic
ne_phi <- posterior(ne_ldaOut)$terms %>% as.matrix 
# matrix -  the probability distribution over topics
ne_theta <- posterior(ne_ldaOut)$topics %>% as.matrix 

# Topic Terms & Probability
ne_ldaOut.terms <- as.matrix(terms(ne_ldaOut, 10))
ne_ldaOut.topics <- data.frame(topics(ne_ldaOut))
ne_ldaOut.topics$index <- as.numeric(row.names(ne_ldaOut.topics))

# As index for our Data sets are different we will copy index from ne_ldaout.topics data set
negative_data$index <- as.numeric(row.names(ne_ldaOut.topics))
ne_datawithtopic <- merge(negative_data, ne_ldaOut.topics, by='index',all.x=TRUE)
ne_datawithtopic <- nedatawithtopic[order(nedatawithtopic$index), ]

# Probabilities
netopicProbabilities <- as.data.frame(ne_ldaOut@gamma)
netopicProbabilities[0:10,1:5]

# create the JSON object to feed the visualization in LDAvis:
install.packages("LDAvis")
library(LDAvis)
install.packages('servr') 
library(servr)
nevocab <- colnames(ne_phi)
json_lda <- createJSON(phi = ne_phi, theta = ne_theta, 
                       vocab = nevocab, doc.length = ne_doc_length, 
                       term.frequency = ne_frequency)
serVis(json_lda, out.dir = 'vis', open.browser = TRUE)

# NEGATIVE TOP 3 TOPICS
top_negative <- colSums(as.matrix(posterior(ne_ldaOut)$topics)) / sum(posterior(ne_ldaOut)$topics)
names(top_negative) <- colnames(netopicProbabilities)
top_negative
top3_negative <- sort(top_negative, decreasing = TRUE)[1:3]
top3_negative

################################################################################
#### --------- [ Validation For Results Using Neutral Corpus ] ------------ ####

# Neutral topic Modelling
n_result <- FindTopicsNumber(
  n_dtm,
  topics = seq(from = 5, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(n_result)

n_ldaOut <-LDA(n_dtm,15, method="Gibbs", 
               control=list(iter=1000,seed=1000))
n_phi <- posterior(n_ldaOut)$terms %>% as.matrix 
#matrix, with each row containing the distribution over terms for a topic,
n_theta <- posterior(n_ldaOut)$topics %>% as.matrix 
#matrix, with each row containing the probability distribution over topics for a document,

n_ldaOut.terms <- as.matrix(terms(n_ldaOut, 10))
n_ldaOut.topics <- data.frame(topics(n_ldaOut))
n_ldaOut.topics$index <- as.numeric(row.names(n_ldaOut.topics))
# As index for our Data sets are different we will copy index from pldaout.topics data set
neutral_data$index <- as.numeric(row.names(n_ldaOut.topics))
ndatawithtopic <- merge(neutral_data, n_ldaOut.topics, by='index',all.x=TRUE)
ndatawithtopic <- ndatawithtopic[order(ndatawithtopic$index), ]
ndatawithtopic[0:10,]
ntopicProbabilities <- as.data.frame(n_ldaOut@gamma)
ntopicProbabilities[0:10,1:5]


# Document Term Matrix - Neutral Corpus
n_dtm <- DocumentTermMatrix(neutral_corpus,
                            control = list(lemma=TRUE,removePunctuation = TRUE,
                                           removeNumbers = TRUE, stopwords = TRUE,
                                           tolower = TRUE))

# Removing the documents with zero tokens
raw.sum = apply(n_dtm,1,FUN=sum)
n_dtm = n_dtm[raw.sum!=0,]
#Converting data matrix into matrix format
n_dtm <- as.matrix(n_dtm)
# Frequency Table
n_frequency <- colSums(n_dtm)
n_frequency <- sort(n_frequency, decreasing=TRUE)
n_doc_length <- rowSums(n_dtm)

n_frequency[1:20]

library(wordcloud)
n_words <- names(n_frequency)# get back the word

wordcloud(n_words[1:100], n_frequency[1:40], rot.per=0.15, 
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))

# Inverse Document Frequency for negative corpus
n_idf <- DocumentTermMatrix(neutral_corpus,
                            control = list( weighting =  weightTfIdf, removePunctuation = TRUE, removeNumbers = TRUE, stopwords =  TRUE, 
                                            tolower = TRUE, wordLengths=c(1,Inf)))

n_freq = data.frame(sort(colSums(as.matrix(n_idf)), decreasing=TRUE))

wordcloud(rownames(n_freq), n_freq[,1], max.words=30, colors=brewer.pal(1, "Dark2"))
head(n_freq)
tail(ne_freq)


# create the JSON object to feed the visualization in LDAvis:
install.packages("LDAvis")
library(LDAvis)
install.packages('servr') 
library(servr)
# Vocab List in Dtm
nvocab <- colnames(n_phi)
n_json_lda <- createJSON(phi = n_phi, theta = n_theta, 
                         vocab = nvocab, doc.length = n_doc_length, 
                         term.frequency = n_frequency)


serVis(n_json_lda, out.dir = 'vis', open.browser = TRUE)


