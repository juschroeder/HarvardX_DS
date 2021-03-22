library(tidyverse)
library(data.table)
library(caret)


# Presets
# code provided in task:

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>%
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")



# Data cleaning
# the title string was separated into title and year by string matching
# the title string was separated into title and year by string matching
title_str <- as.data.frame(str_match(movielens$title, pattern =  "(.*)\\s\\((\\d{4})\\)"))

# the nested genre information was split into multiple rows per movie with one genre each
genres <- str_split(movielens$genres, fixed("|"), simplify = TRUE) %>% 
  as.data.frame() %>%
  bind_cols(select(movielens, movieId)) %>% 
  distinct(.keep_all = TRUE) %>% 
  gather(col, genre, -movieId) %>% 
  filter(genre != "") %>% 
  select(-col)

# the movie title, year and genre information was added back to the original dataset and only relevant columns were kept
movielens <- movielens %>%
  mutate(movie_year = as.character(title_str[,3]),
         movie_title = as.character(title_str[,2])) %>% 
  left_join(genres, by = "movieId") %>%
  rename(movie_genre = genre) %>%
  select(userId, rating, movieId, movie_title, movie_genre)



# Data split
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
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



# Final model as obtained from model training
# lambda from final optimized model tuning
lambda <- 4.5

# calculate mu of the train dataset
mu <- mean(edx$rating)

# calculate movie effect on the train dataset
b_movie <- edx %>%
  group_by(movieId) %>%
  summarize(b_movie = sum(rating - mu)/(n()+lambda), .groups="keep")

# calculate user effect on the train dataset
b_user <- edx %>%
  left_join(b_movie, by = "movieId") %>% 
  group_by(userId) %>%
  summarize(b_user = sum(rating - mu - b_movie)/(n()+lambda), .groups="keep")

# calculate genre effect on the train dataset
b_genre <- edx %>%
  left_join(b_movie, by = "movieId") %>% 
  left_join(b_user, by = "userId") %>% 
  group_by(movie_genre) %>%
  summarize(b_genre = sum(rating - mu - b_movie - b_user)/(n()+lambda), .groups="keep")

# calculate predictions using mu, movie, user and genre effects in validation dataset
preds <- validation %>%
  left_join(b_movie, by = "movieId") %>%
  left_join(b_user, by = "userId") %>% 
  left_join(b_genre, by = "movie_genre") %>% 
  group_by(movieId, userId) %>%
  mutate(pred = mu + b_movie + b_user + sum(b_genre)) %>%
  .$pred

# calculate final RMSE with true ratings from validation dataset and obtained predictions 
rmse_final <- rmse(validation$rating, preds)

# the final RMSE is below 0.86490
isTRUE(rmse_final < 0.86490)

# print final RMSE
print(rmse_final)