library(shiny)
library(data.table)

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
      genre_counts = c(genre_counts, sum(str_detect(my_movie_ratings$Genres, genre) == TRUE))
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
  
}

# Create Shiny app ----
shinyApp(ui, server)