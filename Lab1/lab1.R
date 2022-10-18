# Libraries
library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)


# Constant paths
test_path <- "data/0-authors/test.csv"
train_path <- "data/0-authors/train.csv"


# Stop words list
stop_words <- read_file("stop_words.txt")
splitted_stop_words <- strsplit(stop_words, split = '\n')[[1]]


naiveBayes <-
  setRefClass("naiveBayes",

              fields = list(data = "data.frame"),

              # Teaching model on train dataset
              methods = list(
                fit = function(X)
                {
                  tidy_words <- unnest_tokens(train, 'splitted', 'text', token = "words") %>% filter(!splitted %in% splitted_stop_words)
                  for (cur_author in unique(X$author)) {
                    df <- tidy_words %>%
                      filter(author == cur_author) %>%
                      count(splitted, name = cur_author, sort = TRUE)
                    df[cur_author] <- df[cur_author] + 1
                    df[nrow(df) + 1,] <- c("all", sum(df[, 2]))

                    data <<- merge(df, data, all.x = TRUE, all.y = TRUE, by = "splitted")
                  }
                  data_copy <- data[, -1]
                  rownames(data_copy) <- data[, 1]
                  data <<- data_copy
                  data[is.na(data)] <<- 1
                  data <<- data %>% mutate_at(colnames(data), as.numeric)
                  data["all"] <<- rowSums(data)
                },

                # Predict the author based on train set
                predict = function(message)
                {
                  col_amount <- ncol(data)
                  message_df <- data.frame(text = message)
                  words <- unnest_tokens(message_df, "word", "text")
                  probs <- data.frame()
                  probs["prob", seq_len(col_amount - 1)] <- (data["all", -col_amount] / data["all", "all"])["all",]
                  for (next_word in words[, 1]) {
                    if (!is.na(data[next_word, 1])) {
                      cur_word_num <- (data[next_word, -col_amount] / data["all", -col_amount])
                      probs["prob", seq_len(col_amount - 1)] <- probs["prob",] * cur_word_num
                    }
                  }
                  return(colnames(probs)[max.col(probs)])
                },

                # Calculate
                score = function(X_test)
                {
                  return(sum(lapply(X_test$text, function(x) return(predict(x))) == X_test$author) / nrow(X_test))
                },

                visualize_metrics = function(X_test) {
                  authors <- colnames(data[-ncol(data)])
                  ans <- data.frame()
                  ans[authors, authors] <- 0
                  for (i in seq_len(nrow(X_test))) {
                    pred <- predict(X_test[i, "text"])
                    actual <- X_test[i, "author"]
                    ans[pred, actual] <- ans[pred, actual] + 1
                  }
                  print(ans)
                  a <- data.frame()
                  for (author in authors) {
                    a[author, "precision"] <- ans[author, author] / sum(ans[, author])
                    a[author, "recall"] <- ans[author, author] / sum(ans[author,])
                    a[author, "f1"] <- 2 * a[author, "precision"] * a[author, "recall"] / (a[author, "recall"] + a[author, "precision"])
                  }
                  print(a)
                }
              ))

model <- naiveBayes$new(data = data.frame(splitted = "all"))


train <- read.csv(file = train_path, stringsAsFactors = FALSE)
model$fit(train)
test <- read.csv(file = test_path, stringsAsFactors = FALSE)
model$visualize_metrics(test)
print(model$score(test))
