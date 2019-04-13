## add libraries
library(tidyverse)
library(caret)
library(ggplot2)

## download data from movielens
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

## read ratings and movies from downloaded files
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

## joining movies and ratings into movielens dataset
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

## exploring dataset
nrow(movielens)

ncol(movielens)

summary(movielens)

movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId),
            n_ratings = n_distinct(rating))

## distribution of ratings
hist(movielens$rating, col = "grey", main = "Distribution of Ratings",
     xlab = "ratings", ylab = "count", xlim = range(0.5,5), breaks = 10)

## distribution of movies
movielens %>% count(movieId) %>% ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "grey") + scale_x_log10() + 
  ggtitle("Distribution of Movies")

## distribution of users
movielens %>% count(userId) %>% ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "grey") + scale_x_log10() + 
  ggtitle("Distribution of Users")

## top movies
movielens %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

## preparing data for model
set.seed(1)

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

train_set <- movielens[-test_index,]

temp <- movielens[test_index,]

test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

removed <- anti_join(temp, test_set)

train_set <- rbind(train_set, removed)

## just the average model
mu <- mean(train_set$rating)

mu

rmse_1 <- RMSE(test_set$rating, mu)

rmse_table <- data_frame(method = "Just the average", RMSE = rmse_1)

rmse_table

## movie effect model
movie_avgs <- train_set %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% left_join(movie_avgs, by= 'movieId') %>% .$b_i

rmse_2 <- RMSE(predicted_ratings, test_set$rating)

rmse_table <- bind_rows(rmse_table, data_frame(method = "Movie effect model", RMSE = rmse_2))

rmse_table

## movie and user effect model
user_avgs <- test_set %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% mutate(pred = mu + b_i + b_u) %>% .$pred

rmse_3 <- RMSE(predicted_ratings, test_set$rating)

rmse_table <- bind_rows(rmse_table, 
                        data_frame(method = "Movie + User Effects Model", RMSE = rmse_3))

rmse_table

## tuning predicted ratings
predicted_ratings[which(predicted_ratings<1)] <- min(train_set$rating) 

predicted_ratings[which(predicted_ratings>5)] <- max(train_set$rating)
