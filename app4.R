## app.R ##
library(shinythemes)
library(shinydashboard)

shinyApp(
  ui = fluidPage(shinythemes::themeSelector(),
                # sidebarPanel(),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Tab 1",
                              textInput("txt", "Text input:", "text here"),
                              sliderInput("slider", "Slider input:", 1, 100, 30),
                              actionButton("action", "Button"),
                              actionButton("action2", "Button2", class = "btn-primary")
                     ),
                     tabPanel("Tab 2",
                              tabItem(tabName = "dashboard",
                                      fluidRow(
                                        box(plotOutput("plot1", height = 400)),
                                        
                                        box(
                                          title = "Controls",
                                          sliderInput("slider", "Number of observations:", 1, 100, 50), height = 300, background = "yellow",
                                          textInput("text", "Text input:")
                                          
                                        )
                                      )
                              ))
                   
                 ))
  ),
  server = function(input, output)  {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
      data <- histdata[seq_len(input$slider)]
      hist(data)
    })
  }
)
