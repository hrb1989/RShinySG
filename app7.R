## app.R ##
library(shinydashboard)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inputs", tabName = "Inputs", icon = icon("dashboard")),
      #menuItem("Test", tabName = "Test", icon = icon("dashboard")),
      menuItem("Analysis", tabName = "Analysis", icon = icon("th"), # startExpanded = TRUE,
               menuSubItem("Plot", tabName = "Plot", icon = icon("th")),
               menuSubItem("TestA", tabName = "TestA", icon = icon("th"))
      ),
      menuItem("Reports", tabName = "Reports", icon = icon("circle"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Inputs",
              fileInput('datafile', 'Choose CSV file',
                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
             
              #These column selectors are dynamically created when the file is loaded
              uiOutput("Porfolio.name"),
              uiOutput("loanid"),
              uiOutput("segment"),
              uiOutput("defaultdate"),
              uiOutput("ead"),
              uiOutput("rwa"),
              uiOutput("realisedLGD"),
              uiOutput("estimatedLGD")

      ),
      # firstTest content
      # tabItem(tabName = "Test",
      #         fluidPage(
      #           title = "Hello",
      #           h1("Hello Everyone")
      #           
      #         ),
      #         h2("Test tab content"),
      #         fluidRow(
      #           box(plotOutput("plot1", height = 400)))
      # ),
      
      # first sub tab content
      tabItem(tabName = "Plot",
              fluidRow(column(8, h2("Sub Menu 1 tab content"), align = "center"),column(4,
              uiOutput("Porfolio.name2", inline = FALSE, align = "center"))),
              verbatimTextOutput("sumtab"),
              plotOutput("aesp"),
              plotOutput("distPlot")
              
      ),
      # Second sub tab content
      tabItem(tabName = "TestA",
              fluidRow(column(8, h2("Sub Menu 2 tab content"), align = "center"),column(4, h2(textOutput("texar")))),
              hr(),
              tableOutput("ttest1"),
              span(
                textAreaInput(
                  "Comment", label = "Please enter your comments: "), align = "center", rows = "6"
              )
      ),
      
      tabItem(tabName = "Reports",
              h1("Reports content"),
              radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                           inline = TRUE),
              downloadButton('downloadReport')
              # Button
              
      )
      
      
    )
  )
)

server <- function(input, output) {

  
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  observe(print(filedata()))
  
  #The following set of functions populate the column selectors
  output$Porfolio.name <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("PorfolioName1", "Porfolio Name:",items, selected = 'Portfolio.name')
    
  })
  
  output$Porfolio.name2 <- renderUI({
    df1 <-filedata()
    if (is.null(df)) return(NULL)
    
    items=df1$Porfolio.name
    names(items)=items
    selectInput("PorfolioName2", "Porfolio Name:",items, selected = 'Portfolio.name')
    
  })
  
  output$loanid <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("loanid1", "Loan ID:",items, selected = 'loanid')
    
  })
  
  
  output$segment <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("segment1", "Segment:",items, selected = 'segment')
    
  })
  
  output$defaultdate <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("defaultdate1", "Default Date:",items, selected = 'defaultdate')
    
  })
  
  output$ead <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("ead1", "EAD:",items,selected = 'ead')
    
  })
  
  output$rwa <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("rwa1", "RWA:",items, selected = 'rwa')
    
  })
  
  
  output$realisedLGD <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("realisedLGD1", "Realised LGD:",items, selected = 'realisedLGD')
    
  })
  
  
  output$estimatedLGD <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("estimatedLGD1", "Estimated LGD:",items, selected = 'estimatedLGD')
    
  })
 
  output$sumtab <- renderPrint({
    df1 <- filedata()
    summary(df1 %>% filter(Porfolio.name == input$PorfolioName2))
    
    #summary(df1)
    #summry()
    #returnValue(df1)
    
  })
  
  
  filedata1temp <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  filedata1 <- reactive(
    data.frame(filedata1temp() %>% filter(Porfolio.name == input$PorfolioName2))
    
    )
  observe(print("Hello"))
  #observe(print(filedata1))
  
  
  ttest<-function(x){
    count_x = length(x)
    
    if (count_x <= 1){
      var_x=0
      t_stat = 0
      p_value = 0
    }
    else{
      var_x = sum((x-mean(x))^2)/(count_x-1)
      t_stat = mean(x)*sqrt(count_x/var_x)
      p_value =  1 - pt(t_stat,count_x-1)
    }
    
    c(var_x,t_stat, p_value)
    #c(var_x,t_stat)#, p_value)
    #c(var_x, p_value)
    #c(t_stat, p_value)
  } 
  
  
  output$ttest1 <- renderTable({
    #reactive({
    print("Hello inside TestA")
    df3 <- filedata1()
    #print(filedata1())
    print(df3)
    #})
    #df3 <- df %>% filter(Porfolio.name == input$PorfolioName2)
    #print("df3")
    #print(df3)
    hell <- aggregate(df3$realisedLGD - df3$estimatedLGD, by=list(df3$segment),FUN= function(z) ttest(z))
    print(hell)
    cbind(Group.1=as.character(hell$Group.1), data.frame(hell$x))
   # })
  })
  

  # output$ttest1 <- renderDataTable({
  #   
  #   tet <- setNames(aggregate(df$realisedLGD - df$estimatedLGD, by=list(df$segment),FUN= function(z) ttest(z)), c("Segment", "TStat"))
  #   #switch( tet, "Segment" = te$Group.1, "T Stat" = tet$x[,1], )
  #   #cbind(Segm=as.character(tet$Segment), SD=as.character(data.frame(tet$TStat[,1])))#, check.names = FALSE))
  #   data.frame(cbind(Segm=as.character(tet$Segment), SD=as.character(tet$TStat[,1]), TStat=as.character(tet$TStat[,2]), PValue=as.character(tet$TStat[,3])),fix.empty.names = TRUE, stringsAsFactors = TRUE)
  # })
  

  
  output$distPlot <- renderPlot({
    df3 <- filedata1()
    hist(df3$ead)
    #x    <- df
   # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #hist(x, col = "#75AADB", border = "white",
    #     xlab = "Waiting time to next eruption (in mins)",
    #     main = "Histogram of waiting times")
    
  })
  output$texar <- renderText ({
    paste(input$PorfolioName2)
  })
  
  
  output$aesp <- renderPlot({
    df3 <- filedata1()
    ggplot(df3) +geom_point(aes(realisedLGD, estimatedLGD))
  })
  
  # output$downloadReport <- downloadHandler(
  #   filename = function() {
  #     paste('my-report', sep = '.', switch(
  #       input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
  #     ))
  #   },
  #   
  #   content = function(file) {
  #     src <- normalizePath('C:/Users/shrb/Documents/R/win-library/3.6/shiny/examples/16_SGApp1/report.Rmd')
  #     
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, 'C:/Users/shrb/Documents/R/win-library/3.6/shiny/examples/16_SGApp1/report.Rmd', overwrite = TRUE)
  #     
  #     library(rmarkdown)
  #     out <- render('C:/Users/shrb/Documents/R/win-library/3.6/shiny/examples/16_SGApp1/report.Rmd', switch(
  #       input$format,
  #       PDF = pdf_document(), HTML = html_document(), Word = word_document()
  #     ))
  #     file.rename(out, file)
  #   }
  # )
  
}

shinyApp(ui, server)
