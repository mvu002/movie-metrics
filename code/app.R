library(shiny)
library(data.table)
library(dplyr)
library(janitor)
library(stringr)
library(reticulate)
use_python("C:\\Users\\mvu02\\AppData\\Local\\Programs\\Python\\Python311\\python.exe")

data <- fread("../data/imdb_movie_data.csv")


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(
    h1("Movie Metrics", align = "center")
  ),
  
  headerPanel(""),
  
  # Main panel for displaying outputs ----
  mainPanel(
      
    fluidRow(
      align = "center",
      # Input: Select a file ----
      fileInput("file1", "Please upload your IMDb CSV file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      ), width = 12,
      
    # Output: Most watched movie genre ----
    fluidRow(
      textOutput("mostWatchedMovieGenre"),
      tags$head(tags$style("#mostWatchedMovieGenre{color : red;
                                                   font-size: 20px;
                                                   font-style: italic;
                           }")
                )
      ),
#    fluidRow(
#      tableOutput("contents")
#      )
    fluidRow(
      textOutput("test_name"),
      tags$head(tags$style("#test_name{color : black;
                                      font-size: 20px;
                                      font-style: italic;
                           }")
                )
    ),
    fluidRow(
      imageOutput("director_img"),
      #imageOutput("actor_img"),
      #imageOutput("actress_img")
    )
    )
)



# Define server logic to read selected file ----
server <- function(input, output) {
  
  input_file <- reactive({input$file1$datapath})
  
  
  
  output$mostWatchedMovieGenre <- renderText({
    
    req(input$file1)
    ratings <- fread(input_file())

    movie_ratings <- filter(ratings, `Title Type` == "movie")
    ratings_and_data <- inner_join(data,
                                   movie_ratings,
                                   by = c("tconst" = "Const"))
    ratings_and_data <- ratings_and_data %>%
      select(-genres, -`Title Type`, -runtimeMinutes, -`Title`) %>%
      clean_names()

    imdb_genres <- c('Action','Adult','Adventure','Animation','Biography','Comedy','Crime','Documentary','Drama','Family','Fantasy','Film Noir','Game Show','History','Horror','Musical','Music','Mystery','News','Reality-TV','Romance','Sci-Fi','Short','Sport','Talk-Show','Thriller','War','Western')

    genre_counts = c()
    for (genre in imdb_genres) {
      genre_counts = c(genre_counts, sum(str_detect(movie_ratings$Genres, genre) == TRUE))
    }

    # Create df of movie genre counts
    movie_genres_df <- data.frame(imdb_genres, genre_counts)
    movie_genres_df <- as_tibble(movie_genres_df)

    # Sort table by highest genre count
    movie_genres_df <- arrange(movie_genres_df, desc(genre_counts))
    movie_genres_df

    # Select Top 5 genres
    top_five_movie_genres <- movie_genres_df[1:5, 1:2]
    top_five_movie_genres
    
    return(paste("Most watched genre: ", toString(top_five_movie_genres[1, 1])))
    
  })
  
  output$contents <- renderTable({
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- fread(input_file())
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(head(df))
  })
  
  output$test_name <- renderText({
    req(input$file1)
    ratings <- fread(input_file())
    
    movie_ratings <- filter(ratings, `Title Type` == "movie")
    ratings_and_data <- inner_join(data,
                                   movie_ratings,
                                   by = c("tconst" = "Const"))
    ratings_and_data <- ratings_and_data %>%
      select(-genres, -`Title Type`, -runtimeMinutes, -`Title`) %>%
      clean_names()
    
    # Identify director whose movies I watched the most
    movie_directors <- filter(ratings_and_data, category == "director")
    
    movie_directors <- table(movie_directors$primary_name)
    movie_directors <- data.frame(movie_directors)
    colnames(movie_directors) <- c('Director', 'Count')
    movie_directors <- as_tibble(movie_directors)
    
    movie_directors <- arrange(movie_directors, desc(Count))
    
    # Select Top 5 directors
    top_five_movie_directors <- movie_directors[1:5, 1:2]
    top_five_movie_directors[] <- lapply(top_five_movie_directors, as.character)
    top_movie_director <- toString(top_five_movie_directors[1, 1])
    print(top_movie_director)
    
    
    # Identify the actor whose movies I watched the most
    movie_actors <- filter(ratings_and_data, category == "actor")
    movie_actors 
    
    movie_actors <- table(movie_actors$primary_name)
    movie_actors <- data.frame(movie_actors)
    colnames(movie_actors) <- c('Actor', 'Count')
    movie_actors <- as_tibble(movie_actors)
    
    movie_actors <- arrange(movie_actors, desc(Count))
    
    # Select Top 5 actors
    top_five_movie_actors <- movie_actors[1:5, 1:2]
    top_five_movie_actors[] <- lapply(top_five_movie_actors, as.character)
    top_movie_actor <- toString(top_five_movie_actors[1, 1])
    print(top_movie_actor)
    
    
    # Identify actress whose movies I watched the most
    movie_actresses <- filter(ratings_and_data, category == "actress")
    
    movie_actresses <- table(movie_actresses$primary_name)
    movie_actresses <- data.frame(movie_actresses)
    colnames(movie_actresses) <- c('Director', 'Count')
    movie_actresses <- as_tibble(movie_actresses)
    
    movie_actresses <- arrange(movie_actresses, desc(Count))
    
    # Select Top 5 actresses
    top_five_movie_actresses <- movie_actresses[1:5, 1:2]
    top_five_movie_actresses[] <- lapply(top_five_movie_actresses, as.character)
    top_movie_actress <- toString(top_five_movie_actresses[1, 1])
    print(top_movie_actress)
    
    celebrity_list <- c(top_movie_director, top_movie_actor, top_movie_actress)
    fileConn<-file("../data/celebrity_names.txt")
    writeLines(celebrity_list, fileConn)
    close(fileConn)
    
    py_run_file("tmdb_test.py")
    
    return(paste(top_movie_director, "(most watched director)"))
  })
  
  output$director_img <- renderImage({
    
    req(input$file1)
    ratings <- fread(input_file())
    
    movie_ratings <- filter(ratings, `Title Type` == "movie")
    ratings_and_data <- inner_join(data,
                                   movie_ratings,
                                   by = c("tconst" = "Const"))
    ratings_and_data <- ratings_and_data %>%
      select(-genres, -`Title Type`, -runtimeMinutes, -`Title`) %>%
      clean_names()
    
    # Identify director whose movies I watched the most
    movie_directors <- filter(ratings_and_data, category == "director")
    
    movie_directors <- table(movie_directors$primary_name)
    movie_directors <- data.frame(movie_directors)
    colnames(movie_directors) <- c('Director', 'Count')
    movie_directors <- as_tibble(movie_directors)
    
    movie_directors <- arrange(movie_directors, desc(Count))
    
    # Select Top 5 directors
    top_five_movie_directors <- movie_directors[1:5, 1:2]
    top_five_movie_directors[] <- lapply(top_five_movie_directors, as.character)
    top_movie_director <- toString(top_five_movie_directors[1, 1])
    print(top_movie_director)
    
    list(src = paste0("../data/", top_movie_director, ".jpg"))
  }, deleteFile = F)
  
}

# Create Shiny app ----
shinyApp(ui, server)