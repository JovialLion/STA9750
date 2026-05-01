library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)

get_imdb_file <- function(fname){
  BASE_URL <- "https://datasets.imdbws.com/"
  fname_ext <- paste0(fname, ".tsv.gz")
  if(!file.exists(fname_ext)){
    FILE_URL <- paste0(BASE_URL, fname_ext)
    download.file(FILE_URL, 
                  destfile = fname_ext)
  }
  as.data.frame(readr::read_tsv(fname_ext, lazy=FALSE))
}

NAME_BASICS      <- get_imdb_file("name.basics")
TITLE_BASICS     <- get_imdb_file("title.basics")
TITLE_EPISODES   <- get_imdb_file("title.episode")
TITLE_RATINGS    <- get_imdb_file("title.ratings")
TITLE_CREW       <- get_imdb_file("title.crew")
TITLE_PRINCIPALS <- get_imdb_file("title.principals")



NAME_BASICS <- NAME_BASICS |> 
  filter(str_count(knownForTitles, ",") > 1)



TITLE_RATINGS |>
  ggplot(aes(x=numVotes)) + 
  geom_histogram(bins=30) +
  xlab("Number of IMDB Ratings") + 
  ylab("Number of Titles") + 
  ggtitle("Majority of IMDB Titles Have Less than 100 Ratings") + 
  theme_bw() + 
  scale_x_log10(label=scales::comma) + 
  scale_y_continuous(label=scales::comma)



TITLE_RATINGS |>
  pull(numVotes) |>
  quantile()



TITLE_RATINGS <- TITLE_RATINGS |>
  filter(numVotes >= 100)



TITLE_BASICS <- TITLE_BASICS |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))

TITLE_CREW <- TITLE_CREW |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))

TITLE_EPISODES_1 <- TITLE_EPISODES |>
  semi_join(TITLE_RATINGS, 
            join_by(tconst == tconst))
TITLE_EPISODES_2 <- TITLE_EPISODES |>
  semi_join(TITLE_RATINGS, 
            join_by(parentTconst == tconst))

TITLE_EPISODES <- bind_rows(TITLE_EPISODES_1,
                            TITLE_EPISODES_2) |>
  distinct()

TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
  semi_join(TITLE_RATINGS, join_by(tconst == tconst))


rm(TITLE_EPISODES_1)
rm(TITLE_EPISODES_2)



NAME_BASICS <- NAME_BASICS |>
  mutate(birthYear = as.numeric(birthYear),
         deathYear = as.numeric(deathYear))



#Task 1: Column Type Correction
TITLE_BASICS <- TITLE_BASICS |>
  mutate(startYear = as.numeric(startYear),
         endYear = as.numeric(endYear),
         runtimeMinutes = as.numeric(runtimeMinutes),
         isAdult = as.logical(isAdult))
TITLE_EPISODES <- TITLE_EPISODES |>
  mutate(seasonNumber = as.numeric(seasonNumber),
         episodeNumber = as.numeric(episodeNumber))



#Task 2: Instructor-Provided Questions
movies <- TITLE_BASICS |> count(titleType) |>
  filter(titleType == "movie")
print(movies)
tvSeries <- TITLE_BASICS |> count(titleType) |>
  filter(titleType == "tvSeries")
print(tvSeries)
tvEpisodes <- TITLE_BASICS |> count(titleType) |>
  filter(titleType == "tvEpisode")
print(tvEpisodes)

oldestLiving <- NAME_BASICS |> filter(birthYear > 1917, is.na(deathYear)) |>
  arrange(birthYear) |>
  slice(1)
print(oldestLiving)

perfectRating <- merge(TITLE_BASICS, TITLE_RATINGS, by = "tconst") |> 
  filter(titleType == "tvEpisode", averageRating == 10, numVotes >= 200000)
print(perfectRating)

markHamill <- NAME_BASICS |> filter(primaryName == "Mark Hamill") |>
  separate_longer_delim(knownForTitles, ",") |> slice_head(n=4) |>
  rename("tconst" = "knownForTitles")
markHamilltop4 <- merge(markHamill, TITLE_BASICS, by = "tconst") |>
  slice_head(n=4)
print(markHamilltop4)

highestAvgRate <- full_join(TITLE_EPISODES, TITLE_RATINGS, by = "tconst") |>
  group_by(parentTconst) |>
  summarise(num_episodes = n(), avg_rating = mean(averageRating, na.rm = TRUE)) |>
  filter(num_episodes > 12) |>
  arrange(desc(avg_rating)) |>
  rename("tconst" = "parentTconst")
highestAvgRateName <- full_join(highestAvgRate, TITLE_BASICS, by = "tconst") |>
  slice_head(n=1)
print(highestAvgRateName)



#Task 3: Custom Success Metric
TITLE_RATINGS <- TITLE_RATINGS |> 
  mutate(successRating = numVotes/averageRating)

top5SuccessfulMovies <- TITLE_RATINGS |>
  inner_join(TITLE_BASICS, by = "tconst") |>
  filter(titleType == "movie") |>
  arrange(desc(successRating)) |>
  slice_head(n = 5) |>
  print()

top5UnsuccessfulMovies <- TITLE_RATINGS |>
  inner_join(TITLE_BASICS, by = "tconst") |> 
  filter(titleType == "movie", successRating < 1000, numVotes > 8000) |> 
  arrange(desc(successRating)) |>
  slice_tail(n = 5) |>
  print()



#Task 4: Trends in Success Over Time
success_threshold <- 1000
TITLE_BASICS <- TITLE_BASICS |>
  separate_longer_delim(genres, ",")
title_decades <- full_join(TITLE_BASICS, TITLE_RATINGS, by = "tconst") |>
  mutate(decade = floor(startYear / 10) * 10)
success_counts <- title_decades |>
  filter(successRating > success_threshold) |>
  group_by(decade, genres) |>
  summarise(success_count = n(), .groups = 'drop')
most_successful_genres <- success_counts |>
  group_by(decade) |>
  slice_max(success_count, n = 1, with_ties = FALSE) |>
  ungroup()
ggplot(success_counts, aes(x = decade, y = success_count, fill = genres)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Successful Movies by Decade and Genre", x = "Decade", 
       y = "Success Count", fill = "Genre") +
  theme_minimal()

success_threshold <- 1000
TITLE_MOVIES <- TITLE_BASICS |>
  full_join(TITLE_RATINGS) |>
  filter(titleType == "movie")
consistent_genres <- TITLE_MOVIES |>
  group_by(genres) |> 
  summarise(avg_score = mean(successRating), .groups = 'drop') |>
  filter(avg_score > success_threshold) |>
  arrange(desc(avg_score)) |>
  print()
ggplot(consistent_genres, aes(x = genres, y = avg_score)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Most Successful Movie Genres", x = "Genre", y = "Average Score")



#Task 5: Key Personnel
directors <- NAME_BASICS |>
  separate_longer_delim(primaryProfession, ",") |>
  separate_longer_delim(knownForTitles, ",") |>
  filter(primaryProfession == "director") |>
  filter(!is.na(birthYear), is.na(deathYear)) |>
  rename("tconst" = "knownForTitles") |>
  select(primaryName, birthYear, deathYear, tconst)
sci_fi <- TITLE_BASICS |>
  filter(titleType == "movie" & genres == "Sci-Fi") |>
  select(tconst, primaryTitle, startYear, endYear)
sci_fiDirectors <- inner_join(sci_fi, directors, by = "tconst")
sci_fiDirectorsScores <- sci_fiDirectors |>
  left_join(TITLE_RATINGS |> select(tconst, successRating), by = "tconst")
success_threshold <- 1000
director_counts <- sci_fiDirectorsScores |>
  filter(successRating > success_threshold) |>
  group_by(primaryName) |>
  summarise(movie_count = n(), .groups = 'drop') |>
  arrange(desc(movie_count))

actors <- NAME_BASICS |>
  separate_longer_delim(primaryProfession, ",") |>
  separate_longer_delim(knownForTitles, ",") |>
  filter(primaryProfession == "actor") |>
  filter(!is.na(birthYear), is.na(deathYear)) |>
  rename("tconst" = "knownForTitles") |>
  select(primaryName, birthYear, deathYear, tconst)
sci_fi <- TITLE_BASICS |>
  filter(titleType == "movie" & genres == "Sci-Fi") |>
  select(tconst, primaryTitle, startYear, endYear)
sci_fiActors <- inner_join(sci_fi, actors, by = "tconst")
sci_fiActorsScores <- sci_fiActors |>
  left_join(TITLE_RATINGS |> select(tconst, successRating), by = "tconst")
success_threshold <- 1000
actor_counts <- sci_fiActorsScores |>
  filter(successRating > success_threshold) |>
  group_by(primaryName) |>
  summarise(movie_count = n(), .groups = 'drop') |>
  arrange(desc(movie_count)) 

directors_to_plot <- c("Christopher Nolan", "Anthony Russo", "James Cameron", 
                       "George Lucas", "Denis Villeneuve")
director_plot <- director_counts |>
  filter(primaryName %in% directors_to_plot)
ggplot(director_plot, aes(x = primaryName, y = movie_count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Successful Movie Count by Director",
       x = "Director",
       y = "Number of Successful Movies")

library(knitr)
actor_table <- kable(actor_counts, 
                     caption = "Successful Movie Count by Actor",
                     col.names = c("Actor", "Number of Successful Movies"),
                     format = "markdown")



#Task 6: Finding a Classic Movie to Remake
sci_fi <- TITLE_BASICS |>
  filter(titleType == "movie" & genres == "Sci-Fi") |> 
  select(tconst, primaryTitle, startYear, endYear)
sci_fiHighestRatings <- sci_fi |>
  left_join(TITLE_RATINGS |> select(tconst, successRating), by = "tconst") |>
  arrange(desc(successRating)) |> 
  filter(startYear <= 1999) |>
  slice_head(n = 1) |>
  print()



#Task 7: Write and Deliver Your Pitch
#Pitch goes here.