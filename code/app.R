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

    headerPanel(""),

    fluidRow(
      align = "center",
      textOutput("directors_header"),
      tags$head(tags$style("#directors_header{color : blue;
                                         font-size: 25px;
                                         text-decoration-line: underline;}")),
    ),

    headerPanel(""),

    fluidRow(
      align = "center",
      column(
        textOutput("director_name"),
        tags$head(tags$style("#director_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
        imageOutput("director1_img"),
        width = 4
      ),
      column(
        textOutput("director2_name"),
        tags$head(tags$style("#director2_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
        imageOutput("director2_img"),
        width = 4
      ),
      column(
        textOutput("director3_name"),
        tags$head(tags$style("#director3_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
        imageOutput("director3_img"),
        width = 4
      ),
    ),

headerPanel(""),
headerPanel(""),
headerPanel(""),
headerPanel(""),

    fluidRow(
      align = "center",
      textOutput("actors_header"),
      tags$head(tags$style("#actors_header{color : blue;
                                         font-size: 25px;
                                         text-decoration-line: underline;}")),
      ),

    headerPanel(""),

    fluidRow(
      align = "center",
      column(
        textOutput("actor_name"),
        tags$head(tags$style("#actor_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
        imageOutput("actor_img"),
        width = 4
      ),
      column(
        textOutput("actor2_name"),
        tags$head(tags$style("#actor2_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
        imageOutput("actor2_img"),
        width = 4
      ),
      column(
        textOutput("actor3_name"),
        tags$head(tags$style("#actor3_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
        imageOutput("actor3_img"),
        width = 4
      ),
    ),

headerPanel(""),
headerPanel(""),
headerPanel(""),
headerPanel(""),


    fluidRow(
      align = "center",
      textOutput("actresses_header"),
      tags$head(tags$style("#actresses_header{color : blue;
                                         font-size: 25px;
                                         text-decoration-line: underline;}")),
    ),

    headerPanel(""),

    fluidRow(
      align = "center",
      column(
        textOutput("actress_name"),
        tags$head(tags$style("#actress_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
        imageOutput("actress_img"),
        width = 4
      ),
      column(
        textOutput("actress2_name"),
        tags$head(tags$style("#actress2_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
        imageOutput("actress2_img"),
        width = 4
      ),
      column(
        textOutput("actress3_name"),
        tags$head(tags$style("#actress3_name{color : black;
                                         font-size: 20px;
                                         font-style: bold;}")),
        imageOutput("actress3_img"),
        width = 4
      ),
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
  top_three_movie_directors <- reactive({
    movie_directors <- filter(ratings_and_data(), category == "director")
    movie_directors <- table(movie_directors$primary_name)
    movie_directors <- data.frame(movie_directors)
    colnames(movie_directors) <- c('Director', 'Count')
    movie_directors <- as_tibble(movie_directors)
    movie_directors <- arrange(movie_directors, desc(Count))
    movie_directors <- movie_directors[1:3, 1]
    movie_directors[] <- lapply(movie_directors, as.character)
    #movie_directors <- toString(movie_directors[1:3, 1])
    return(movie_directors$Director)
  })
  
  # # Select top movie director
  # top_movie_director <- reactive({
  #   result <- top_three_movie_directors()
  #   result[] <- lapply(result, as.character)
  #   result <- toString(result[1, 1])
  #   return(result)
  # })
  
  # Identify top 5 movie actors
  top_three_movie_actors <- reactive({
    movie_actors <- filter(ratings_and_data(), category == "actor")
    movie_actors <- table(movie_actors$primary_name)
    movie_actors <- data.frame(movie_actors)
    colnames(movie_actors) <- c('Actor', 'Count')
    movie_actors <- as_tibble(movie_actors)
    movie_actors <- arrange(movie_actors, desc(Count))
    movie_actors <- movie_actors[1:3, 1]
    movie_actors[] <- lapply(movie_actors, as.character)
    #movie_actors <- toString(movie_actors[1:3, 1])
    return(movie_actors$Actor)
  })

  # # Select top movie actor
  # top_movie_actor <- reactive({
  #   result <- top_three_movie_actors()
  #   result[] <- lapply(result, as.character)
  #   result <- toString(result[1, 1])
  #   return(result)
  # })
  
  # Identify top 5 movie actresses
  top_three_movie_actresses <- reactive({
    movie_actresses <- filter(ratings_and_data(), category == "actress")
    movie_actresses <- table(movie_actresses$primary_name)
    movie_actresses <- data.frame(movie_actresses)
    colnames(movie_actresses) <- c('Actress', 'Count')
    movie_actresses <- as_tibble(movie_actresses)
    movie_actresses <- arrange(movie_actresses, desc(Count))
    movie_actresses <- movie_actresses[1:3, 1]
    movie_actresses[] <- lapply(movie_actresses, as.character)
    #movie_actresses <- toString(movie_actresses[1:3, 1])
    return(movie_actresses$Actress)
  })
  
  # # Select top movie actress
  # top_movie_actress <- reactive({
  #   result <- top_three_movie_actresses()
  #   result[] <- lapply(result, as.character)
  #   result <- toString(result[1, 1])
  #   return(result)
  # })
  
  # Write top director/actor/actress to txt file
  celebrity_list <- reactive({
    #names <- c(top_movie_director(), top_movie_actor(), top_movie_actress())
    names <- c(top_three_movie_directors(), top_three_movie_actors(), top_three_movie_actresses())
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
  
  output$directors_header <- renderText({
    req(input$file1)
    return("Most Watched Directors")
  })
  
  output$actors_header <- renderText({
    req(input$file1)
    return("Most Watched Actors")
  })
  
  output$actresses_header <- renderText({
    req(input$file1)
    return("Most Watched Actresses")
  })
  
  output$director_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    #return(paste(top_movie_director(), "(most watched director)"))
    return(paste("1.", top_three_movie_directors()[1]))
  })
  
  output$director2_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    #return(paste(top_movie_director(), "(most watched director)"))
    return(paste("2.", top_three_movie_directors()[2]))
  })
  
  output$director3_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    #return(paste(top_movie_director(), "(most watched director)"))
    return(paste("3.", top_three_movie_directors()[3]))
  })
  
  output$actor_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    #return(paste(top_movie_actor(), "(most watched actor)"))
    return(paste("1.", top_three_movie_actors()[1]))
  })
  
  output$actor2_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    #return(paste(top_movie_director(), "(most watched director)"))
    return(paste("2.", top_three_movie_actors()[2]))
  })
  
  output$actor3_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    #return(paste(top_movie_director(), "(most watched director)"))
    return(paste("3.", top_three_movie_actors()[3]))
  })
  
  output$actress_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    #return(paste(top_movie_actress(), "(most watched actress)"))
    return(paste("1.", top_three_movie_actresses()[1]))
  })
    
  output$actress2_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    #return(paste(top_movie_director(), "(most watched director)"))
    return(paste("2.", top_three_movie_actresses()[2]))
  })
    
  output$actress3_name <- renderText({
    req(input$file1)
    writeLines(celebrity_list(), "../data/celebrity_names.txt")
    #return(paste(top_movie_director(), "(most watched director)"))
    return(paste("3.", top_three_movie_actresses()[3]))
  })
  
  
  output$director1_img <- renderImage({
    
    req(input$file1)
    
    # Use TMDb Python wrapper to get images
    py_run_file("tmdb_test.py")
    
    print("posting director image")
    
    #list(src = paste0("../data/", top_movie_director(), ".jpg"))
    list(src = paste0("../data/", top_three_movie_directors()[1], ".jpg"))
  }, deleteFile = F)
  
  output$director2_img <- renderImage({
    
    req(input$file1)
    
    print("posting director 2 image")
    
    list(src = paste0("../data/", top_three_movie_directors()[2], ".jpg"))
  }, deleteFile = F)
  
  output$director3_img <- renderImage({
    
    req(input$file1)
    
    print("posting director 3 image")
    
    list(src = paste0("../data/", top_three_movie_directors()[3], ".jpg"))
  }, deleteFile = F)  
  
  output$actor_img <- renderImage({
    req(input$file1)
    
    print("posting actor image")
    
    #list(src = paste0("../data/", top_movie_actor(), ".jpg"))
    list(src = paste0("../data/", top_three_movie_actors()[1], ".jpg"))
  }, deleteFile = F)
  
  output$actor2_img <- renderImage({
    req(input$file1)
    
    print("posting actor 2 image")
    
    #list(src = paste0("../data/", top_movie_actor(), ".jpg"))
    list(src = paste0("../data/", top_three_movie_actors()[2], ".jpg"))
  }, deleteFile = F)
  
  
  output$actor3_img <- renderImage({
    req(input$file1)
    
    print("posting actor 3 image")
    
    #list(src = paste0("../data/", top_movie_actor(), ".jpg"))
    list(src = paste0("../data/", top_three_movie_actors()[3], ".jpg"))
  }, deleteFile = F)
  
  output$actress_img <- renderImage({
    req(input$file1)
    
    # Use TMDb Python wrapper to get images
    #py_run_file("tmdb_test.py")
    
    print("posting actress image")
    
    #list(src = paste0("../data/", top_movie_actress(), ".jpg"))
    list(src = paste0("../data/", top_three_movie_actresses()[1], ".jpg"))
  }, deleteFile = F)
  
  output$actress2_img <- renderImage({
    req(input$file1)
    
    # Use TMDb Python wrapper to get images
    #py_run_file("tmdb_test.py")
    
    print("posting actress 2 image")
    
    #list(src = paste0("../data/", top_movie_actress(), ".jpg"))
    list(src = paste0("../data/", top_three_movie_actresses()[2], ".jpg"))
  }, deleteFile = F)
  
  output$actress3_img <- renderImage({
    req(input$file1)
    
    # Use TMDb Python wrapper to get images
    #py_run_file("tmdb_test.py")
    
    print("posting actress 3 image")
    
    #list(src = paste0("../data/", top_movie_actress(), ".jpg"))
    list(src = paste0("../data/", top_three_movie_actresses()[3], ".jpg"))
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