library(shiny)
ui <- fluidPage(
  titlePanel("Reactivity"),
  
  tags$head(
    tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
  ),
  
  fluidRow(
    column(4,
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary")),
      column(4,
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:", selectize = FALSE,
                  choices = c("rock", "pressure", "cars"))),
      column(4,
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10))
      
    ),
    hr(),
  fluidRow(
    column(7,
      h3(textOutput("caption", container = span)),
      
      verbatimTextOutput("summary")),
    
    column(5, 
      
      tableOutput("view"))
      
    )

)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$dataset
  })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
