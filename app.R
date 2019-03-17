library(shiny)
library(tm)
library(wordcloud)
library(memoise)




# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(
        img(src = "scbd.png", height = 90, style="display: block; margin-left: auto; margin-right: auto;"),
    h1("Wordcloud Generator")),
  
  sidebarLayout(
    sidebarPanel(
      h1('WordCloud Generator'),
      helpText("Created by Muhammad Apriandito"),
      hr(),
      textInput("filein", label = h3("Text input"), value = "Word Cloud"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100),
      actionButton("update", "Generate"),
      hr(),
      p(" Ujicoba Pengembangan Shiny Word-Cloud Generator.", align ='left')
      ),
    
    mainPanel(
      h4("Data Properties:"),
      p(""),
      fluidRow(column(12, verbatimTextOutput("value"))),
      hr(),
      h4("Wordcloud Visualization:"),
      plotOutput("plot")
      ),

  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    terms <- reactive({ input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        text <- input$filein
        myCorpus = Corpus(VectorSource(text))
        myCorpus = tm_map(myCorpus, content_transformer(tolower))
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        myCorpus = tm_map(myCorpus, removeWords,
                          c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
        
        myDTM = TermDocumentMatrix(myCorpus,
                                   control = list(minWordLength = 1))
        
        m = as.matrix(myDTM)
        
        output$value <- renderPrint({ input$filein })
        
        sort(rowSums(m), decreasing = TRUE)
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

