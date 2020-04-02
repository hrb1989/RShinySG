library(shiny)
ui <- pageWithSidebar(
  headerPanel("CSV File Upload Demo"),
  
  sidebarPanel(
    #Selector for file upload
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    #These column selectors are dynamically created when the file is loaded
    uiOutput("codeCol"),
    uiOutput("rentCol"),
    uiOutput("renCol")
    
  ),
  mainPanel(
    textOutput("tex")
  )
)


server <- function(input, output) {
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  
  #The following set of functions populate the column selectors
  output$rentCol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("rent", "rent:",items)
    
  })
  
  output$codeCol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("code", "Code:",items)
    
  })
  
  
  output$renCol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("ren", "Ren:",items)
    
  })

  output$tex <- renderText ({
    paste(input$code, input$rent, input$ren)
  })
  
}


shinyApp(ui, server)
