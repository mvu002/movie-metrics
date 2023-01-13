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

