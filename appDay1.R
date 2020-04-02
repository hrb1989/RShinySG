library(shiny)
ui <-
  fluidPage(
  #navbarPage(
    titlePanel("Soc Gen"),
    sidebarLayout(
      sidebarPanel(
    
   # tabPanel("Home"),
    #tabPanel("Slider",
      sliderInput("test", label = "test", min = 1, max = 10, value = 5, step = 0.5),
      tags$hr(),
      checkboxInput("Yes", "Yes", value = FALSE),
      tags$hr(),
      radioButtons("radio",label = "Radio Test",choices = list("Monday", "Tuesday", "Wednesday", "Thursday"))
    #)
      ),
   
   mainPanel(
     navbarPage(
       "",
      tabPanel("Hello", 
               h1("This is the main panel page"),
               img(src = "Rlogo.png", height = 300, width = 300))
     )
   )
  )
  )
  
server <- function(input, output){}

shinyApp(ui, server)
