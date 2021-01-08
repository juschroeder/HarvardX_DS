library(tidyverse)
library(caret)
library(data.table)
library(lubridate)


# 1. Movielens dataset ----------------------------------------------------
# presets
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)




# Quiz:
head(edx)
head(validation)

# Q1
nrow(edx)
ncol(edx)

# Q2
edx %>% count(rating)

# Q3
length(unique(edx$movieId))
n_distinct(edx$movieId)

# Q4
n_distinct(edx$userId)

# Q5
as.data.frame(str_split(edx$genres, fixed("|"), simplify = TRUE)) %>%
  gather(col, genres) %>% 
  filter(genres != "") %>%
  count(genres, sort = TRUE)

#edx %>% separate_rows(genres, sep = "\\|") %>% head

# Q6
edx %>%
  count(movieId, sort = TRUE) %>% 
  head
edx %>% 
  filter(movieId == 296) %>%
  head

# Q7
edx %>%
  count(rating, sort = TRUE) %>%
  head(5)


# Q8
edx %>%
  select(rating) %>%
  mutate(half = case_when(
    str_detect(.$rating, ".5") ~ "half",
    TRUE ~ "full")) %>%
  count(half)
