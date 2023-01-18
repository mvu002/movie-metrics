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
    # fluidRow(
    #   textOutput("mostWatchedMovieGenre"),
    #   tags$head(tags$style("#mostWatchedMovieGenre{color : red;
    #                                                font-size: 20px;
    #                                                font-style: italic;
    #                        }")
    #             )
    #   ),
    
    
#    fluidRow(
#      tableOutput("contents")
#      )
    # fluidRow(
    #   textOutput("director_name"),
    #   tags$head(tags$style("#director_name{color : black;
    #                                   font-size: 20px;
    #                                   font-style: italic;
    #                        }")
    #             )
    # ),
    fluidRow(
      align = "center",
      textOutput("director_name"),
         tags$head(tags$style("#director_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
      imageOutput("director_img")
      ),

headerPanel(""),
headerPanel(""),
headerPanel(""),
headerPanel(""),

    fluidRow(
      align = "center", 
      textOutput("actor_name"),
      tags$head(tags$style("#actor_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
      imageOutput("actor_img")
    ),

headerPanel(""),
headerPanel(""),
headerPanel(""),
headerPanel(""),

    fluidRow(
      align = "center",
      textOutput("actress_name"),
      tags$head(tags$style("#actress_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
      imageOutput("actress_img")
      )
  )
)



# Define server logic to read selected file ----
server <- function(input, output) {
  
  ratings <- reactive({
    fread(input$file1$datapath)
    })
  
  movie_ratings <- reactive({
    filter(ratings(), `Title Type` == "movie")
    })
  
  ratings_and_data <- reactive({
    inner_join(data, 
               movie_ratings(),
               by = c("tconst" = "Const")) %>%
      select(-genres, -`Title Type`, -runtimeMinutes, -`Title`) %>%
      clean_names()
  })
  
  # Identify top 5 movie directors
  top_five_movie_directors <- reactive({
    movie_directors <- filter(ratings_and_data(), category == "director")
    movie_directors <- table(movie_directors$primary_name)
    movie_directors <- data.frame(movie_directors)
    colnames(movie_directors) <- c('Director', 'Count')
    movie_directors <- as_tibble(movie_directors)
    movie_directors <- arrange(movie_directors, desc(Count))
    return(movie_directors[1:5, 1:2])
  })
  
  # Select top movie director
  top_movie_director <- reactive({
    result <- top_five_movie_directors()
    result[] <- lapply(result, as.character)
    result <- toString(result[1, 1])
    return(result)
  })
  
  # Identify top 5 movie actors
  top_five_movie_actors <- reactive({
    movie_actors <- filter(ratings_and_data(), category == "actor")
    movie_actors <- table(movie_actors$primary_name)
    movie_actors <- data.frame(movie_actors)
    colnames(movie_actors) <- c('Actor', 'Count')
    movie_actors <- as_tibble(movie_actors)
    movie_actors <- arrange(movie_actors, desc(Count))
    return(movie_actors[1:5, 1:2])
  })

  # Select top movie actor
  top_movie_actor <- reactive({
    result <- top_five_movie_actors()
    result[] <- lapply(result, as.character)
    result <- toString(result[1, 1])
    return(result)
  })
  
  # Identify top 5 movie actresses
  top_five_movie_actresses <- reactive({
    movie_actresses <- filter(ratings_and_data(), category == "actress")
    movie_actresses <- table(movie_actresses$primary_name)
    movie_actresses <- data.frame(movie_actresses)
    colnames(movie_actresses) <- c('Director', 'Count')
    movie_actresses <- as_tibble(movie_actresses)
    movie_actresses <- arrange(movie_actresses, desc(Count))
    return(movie_actresses[1:5, 1:2])
  })
  
  # Select top movie actress
  top_movie_actress <- reactive({
    result <- top_five_movie_actresses()
    result[] <- lapply(result, as.character)
    result <- toString(result[1, 1])
    return(result)
  })
  
  # Write top director/actor/actress to txt file
  celebrity_list <- reactive({
    names <- c(top_movie_director(), top_movie_actor(), top_movie_actress())
    # fileConn<-file("../data/celebrity_names.txt")
    # writeLines(names, fileConn)
    # close(fileConn)
    
    return(names)
  })

  output$mostWatchedMovieGenre <- renderText({
    
    req(input$file1)

    imdb_genres <- c('Action','Adult','Adventure','Animation','Biography','Comedy','Crime','Documentary','Drama','Family','Fantasy','Film Noir','Game Show','History','Horror','Musical','Music','Mystery','News','Reality-TV','Romance','Sci-Fi','Short','Sport','Talk-Show','Thriller','War','Western')

    genre_counts = c()
    for (genre in imdb_genres) {
      genre_counts = c(genre_counts, sum(str_detect(movie_ratings()$Genres, genre) == TRUE))
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
  
  output$director_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    return(paste(top_movie_director(), "(most watched director)"))
  })
  
  output$actor_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    return(paste(top_movie_actor(), "(most watched actor)"))
  })
  
  output$actress_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    return(paste(top_movie_actress(), "(most watched actress)"))
  })
  
  
  output$director_img <- renderImage({
    
    req(input$file1)
    
    # Use TMDb Python wrapper to get images
    py_run_file("tmdb_test.py")
    
    print("posting director image")
    
    list(src = paste0("../data/", top_movie_director(), ".jpg"))
  }, deleteFile = F)
  
  output$actor_img <- renderImage({
    req(input$file1)
    
    print("posting actor image")
    
    list(src = paste0("../data/", top_movie_actor(), ".jpg"))
  }, deleteFile = F)
  
  output$actress_img <- renderImage({
    req(input$file1)
    
    # Use TMDb Python wrapper to get images
    py_run_file("tmdb_test.py")
    
    print("posting actress image")
    
    list(src = paste0("../data/", top_movie_actress(), ".jpg"))
  }, deleteFile = F)
  
  # output$contents <- renderTable({
  #   
  #   req(input$file1)
  #   
  #   # when reading semicolon separated files,
  #   # having a comma separator causes `read.csv` to error
  #   tryCatch(
  #     {
  #       df <- fread(input_file())
  #     },
  #     error = function(e) {
  #       # return a safeError if a parsing error occurs
  #       stop(safeError(e))
  #     }
  #   )
  #   
  #   return(head(df))
  # })
  
}

# Create Shiny app ----
shinyApp(ui, server)