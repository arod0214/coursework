library(tidyverse)
library(tm)
library(Matrix)
library(glmnet)
library(ROCR)
library(broom)

########################################
# LOAD AND PARSE ARTICLES
########################################

# read in the business and world articles from files
# combine them both into one data frame called articles
library(readr)
articles <- read.csv("../week4/nyt_busi_world_articles.csv") 

# create a corpus from the article snippets
# using the Corpus and VectorSource functions
corpus <- Corpus(VectorSource(articles$snippet))

# create a DocumentTermMatrix from the snippet Corpus
# remove stopwords, punctuation, and numbers
dtm <- DocumentTermMatrix(corpus, list(weighting=weightBin,
                                       stopwords=T,
                                       removePunctuation=T,
                                       removeNumbers=T))

# convert the DocumentTermMatrix to a sparseMatrix
X <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, dims=c(dtm$nrow, dtm$ncol), dimnames=dtm$dimnames)

# set a seed for the random number generator so we all agree
set.seed(42)

########################################
# YOUR SOLUTION BELOW
########################################

# create a train / test split
sample <- sample.int(n = nrow(X), size = floor(.8*nrow(X)), replace = F)
train <- X[sample, ]
test  <- X[-sample, ]

# cross-validate logistic regression with cv.glmnet (family="binomial"), measuring auc
model <- cv.glmnet(train, articles[sample,]$section_name,family = "binomial", type.measure = "auc")
summary(model)

# plot the cross-validation curve
plot(model)

# evaluate performance for the best-fit model
# note: it's useful to explicitly cast glmnet's predictions
# use as.numeric for probabilities and as.character for labels for this
articles_test <- data.frame(section_name = articles[-sample, ]$section_name)
articles_test$pred <- as.factor(predict(model,test, type = "class"))
articles_test$prob <- as.numeric(predict(model,test, type = "response"))

# look at the confusion matrix
table(articles_test$section_name, articles_test$pred)

# compute accuracy
articles_test %>% summarize(acc = mean(pred == section_name))

# plot an ROC curve and calculate the AUC
# (see last week's notebook for this)
pred <- prediction(articles_test$prob, articles_test$section_name)
roc <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(roc)
performance(pred, 'auc')

# show weights on words with top 10 weights for business
# use the coef() function to get the coefficients
# and tidy() to convert them into a tidy data frame
word_weights <- tidy(coef(model))
word_weights %>% filter(value < 0) %>% arrange(value) %>% head(10)

# show weights on words with top 10 weights for world
word_weights %>% filter(value > 0) %>% arrange(value) %>% head(10)

