library(shiny)

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
      
    # Output: Data file ----
    tableOutput("contents")
      
    )
)



# Define server logic to read selected file ----
server <- function(input, output) {
  
  input_file <- reactive({input$file1$datapath})
  
  output$contents <- renderTable({
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input_file())
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