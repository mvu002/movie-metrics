# Libraries

source("code/helper_functions.R")

# chunk takes ~ 2 min to run
title_basics <- convert_to_csv("title.basics.tsv")
name_basics <- convert_to_csv("name.basics.tsv")
title_crew <- convert_to_csv("title.crew.tsv")
title_episode <- convert_to_csv("title.episode.tsv")
title_principals <- convert_to_csv("title.principals.tsv")
title_ratings <- convert_to_csv("title.ratings.tsv")

# Exploratory Data Analysis 

# Movie data
head(title_basics)
unique(title_basics$titleType)

movies <- filter(title_basics, titleType == "movie")
nrow(movies)
head(movies)

# Movie star data (actor, actress, director)
head(title_principals)
unique(title_principals$category)

roles <- list("actor", "actress", "director")

principal_roles <- filter(title_principals, category %in% roles)
head(principal_roles)
unique(principal_roles$job)
principal_roles <- select(principal_roles, -job, -characters, -ordering)

head(principal_roles)
head(movies)

# Movie data & movie star data
movies_and_principals <- inner_join(movies,
                                    principal_roles,
                                    by = c("tconst"))

head(movies_and_principals)

# Movie star name data
head(name_basics)

# Movie data & movie star data & movie star name data
data <- inner_join(movies_and_principals,
                   name_basics,
                   by = c("nconst"))

head(data)


# My ratings
my_ratings <- fread("C:/Users/mvu02/Desktop/Projects/movie-metrics/data/my_ratings.csv")
head(my_ratings)
my_movie_ratings <- filter(my_ratings, `Title Type` == "movie")

# My ratings + data
ratings_and_data <- inner_join(data,
                               my_movie_ratings,
                               by = c("tconst" = "Const"))
head(ratings_and_data)
ratings_and_data <- ratings_and_data %>%
  select(-genres, -titleType, -`Title Type`, -runtimeMinutes, -`Title`) %>%
  clean_names()

head(ratings_and_data)
length(unique(ratings_and_data$primary_title))

######### compare titles on my_ratings and data (final join is missing 10 titles)
# not_in <- my_ratings %>%
#  filter(!Title %in% unique(ratings_and_data$primary_title) & !Title %in% unique(ratings_and_data$original_title))
# View(not_in)
#not_in <- my_ratings %>%
#  filter(`Title Type` != "movie")
#length(my_ratings$Title)
#View(filter(data, startsWith(primaryTitle, "Star Wars")))
#View(filter(movies, startsWith(primaryTitle, "Population")))

#### REASON -- found that the excluded titles had a different TitleType (tvMovie, video),
####           which is why the `data` variable did not have matching rows
####          `data` variable was filtered to only include TitleType "movie"
#### SOLUTION -- filter `my_ratings` for only rows with TitleType == "movie"

# Filter movies by genre
View(my_movie_ratings)
View(filter(my_movie_ratings, str_detect(my_movie_ratings$Genres, 'Horror') == TRUE))

# Get list of genres defined by IMDB
imdb_genres <- c('Action','Adult','Adventure','Animation','Biography','Comedy','Crime','Documentary','Drama','Family','Fantasy','Film Noir','Game Show','History','Horror','Musical','Music','Mystery','News','Reality-TV','Romance','Sci-Fi','Short','Sport','Talk-Show','Thriller','War','Western')

# Get counts of each movie genre
for (genre in imdb_genres) {
  print(paste(genre, ":", sum(str_detect(my_movie_ratings$Genres, genre) == TRUE)))
}


genre_counts = c()
for (genre in imdb_genres) {
  genre_counts = c(genre_counts, sum(str_detect(my_movie_ratings$Genres, genre) == TRUE))
}  

# Create df of movie genre counts
my_movie_genres_df <- data.frame(imdb_genres, genre_counts)
my_movie_genres_df <- as_tibble(my_movie_genres_df)
View(my_movie_genres_df)

# Sort table by highest genre count
my_movie_genres_df <- arrange(my_movie_genres_df, desc(genre_counts))
my_movie_genres_df

# Select Top 5 genres
my_top_five_movie_genres <- my_movie_genres_df[1:5, 1:2]
my_top_five_movie_genres

# Identify director whose movies I watched the most
my_movie_directors <- filter(ratings_and_data, category == "director")
my_movie_directors 

my_movie_directors <- table(my_movie_directors$primary_name)
my_movie_directors <- data.frame(my_movie_directors)
colnames(my_movie_directors) <- c('Director', 'Count')
my_movie_directors <- as_tibble(my_movie_directors)
View(my_movie_directors)

my_movie_directors <- arrange(my_movie_directors, desc(Count))
my_movie_directors

# Select Top 5 directors
my_top_five_movie_directors <- my_movie_directors[1:5, 1:2]
my_top_five_movie_directors

# Identify the actor whose movies I watched the most
