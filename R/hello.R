library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
data <- read.csv('music_genre.csv')

glimpse(Music)
summary(Music)
head(Music)
Music <- data.frame(data)


names(Music)

Music <- subset(Music, select = -c(instance_id, obtained_date, popularity, track_name, artist_name))

sapply(Music, function(x) sum(is.na(x)))
Music <- na.omit(Music)
Music <- Music[Music$tempo != "?", ]
Music <- Music[Music$duration_ms != "-1", ]
Music <- Music[Music$music_genre != "Rap", ]
Music$tempo <- as.numeric(Music$tempo)
unique(Music$music_genre)

#UnderSampling
desired_rows <- 30000
set.seed(123)
sampled_indices <- sample(nrow(Music), desired_rows, replace = TRUE)
sampled_data <- Music[sampled_indices, ]
table(sampled_data$music_genre)
Music<- sampled_data
View(Music)

sapply(Music, class)


hist(Music$valence)

ggplot(Music, aes(x = music_genre, fill = music_genre)) +
  geom_bar() +
  labs(x = "Music Genre", y = "Count", title = "Multi-Class Target Variable Distribution")


ggplot(Music, aes(x = mode, fill = mode)) +
  geom_bar()

ggplot(Music, aes(x = key, fill = key)) +
  geom_bar()

#ggplot(Music, aes(x = key, y = energy, fill = key)) +
  geom_boxplot() +
  labs(x = "Key", y = "Energy") +
  theme_minimal()

ggplot(Music, aes(x = music_genre, y = tempo, fill = music_genre)) +
  geom_boxplot() +
  labs(x = "Music Genre", y = "Tempo") +
  theme_minimal()



ggplot(Music, aes(x = music_genre, y = danceability, fill = music_genre)) +
  geom_bar() +
  labs(x = "Music Genre", y = "Danceability", title = "Genre Danceability") +
  theme_minimal()

ggplot(Music, aes(x = energy, y = liveness)) +
  geom_point() +
  labs(x = "Energy", y = "Liveness") +
  theme_minimal()

subset_data <- Music[Music$music_genre %in% c("Electronic", "Classical", "Country", "Anime"),]

library(tidyr)
subset_data_long <- gather(subset_data, key = "feature", value = "value", tempo, loudness, speechiness, danceability)


#Create a grouped bar chart
ggplot(subset_data_long, aes(x = feature, y = value, fill = music_genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of Various Numerical Features Across Music Genres",
       x = "Feature",
       y = "Value",
       fill = "Music Genre") +
  scale_fill_manual(values = c("Electronic" = "blue", "Classical" = "red", "Country" = "pink", "Anime" = "cyan"))


p

library(corrplot)
Music_numeric <- subset (Music, select = -c(key, mode))
Music_numeric$music_genre <- as.factor(Music_numeric$music_genre)
Music_numeric$music_genre <- as.numeric(Music_numeric$music_genre)
Music_numeric$tempo <- as.numeric(Music_numeric$tempo)
sapply(Music_numeric, class)
correlation_matrix <- cor(Music_numeric)
correlation_df <- as.data.frame(correlation_matrix)
corrplot(correlation_matrix, method = "color")

#Normalizing numeric columns
numeric_cols <- sapply(Music, is.numeric)
#Using Min-Max Scaling
Music_normalized <- Music
Music_normalized[, numeric_cols] <- lapply(Music_normalized[, numeric_cols], function(x) (x-min(x)) / (max(x) - min(x)))
head(Music_normalized)
Music <- Music_normalized


Music$key <- factor(Music$key)
Music$mode <- factor(Music$mode)
Music$music_genre <- factor(Music$music_genre)

set.seed((100))
#Split Dataset into training and testing
TrainingIndex <- createDataPartition(Music$music_genre, p=0.8, list = FALSE)
TrainingSet <- Music[TrainingIndex, ]
TestingSet <- Music[-TrainingIndex, ]


#Converting Categorical Features to Numerical Using Label Encoding
TrainingSet$key <- as.factor(TrainingSet$key)
TrainingSet$mode <- as.factor(TrainingSet$mode)
TrainingSet$music_genre <- as.factor(TrainingSet$music_genre)

TestingSet$key <- as.factor(TestingSet$key)
TestingSet$mode <- as.factor(TestingSet$mode)
TestingSet$music_genre <- as.factor(TestingSet$music_genre)

X_Train <- TrainingSet[, -13]
Y_Train <- TrainingSet[, 13]

X_Test <- TestingSet[, -13]
Y_Test <- TestingSet[, 13]

sapply(TrainingSet, function(x) sum(is.na(x)))
str(TrainingSet)




library(randomForest)
Model <- randomForest(x = X_Train,
                      y = Y_Train,
                      mtry = 4,
                      ntree = 501,
                      max_depth = 15,
                      min_samples_leaf = 200,
                      importance = TRUE)
print(Model)


predictions <- predict(Model, newdata = X_Test)
accuracy <- mean(predictions == Y_Test)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
view_results <- cbind(predictions, Y_Test)





Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")


