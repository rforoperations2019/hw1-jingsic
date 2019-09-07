#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(wordcloud)
library(memoise)



fataldrug.load <- read.csv("fataldrug.csv", header = TRUE, sep=',')

getTermMatrix <- memoise(function(no) {

  wc<-fataldrug.load[[no]]
  tableWork<-table(wc)
  tableWork<-sort(tableWork,decreasing = TRUE)
  tableWork
  
})

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Allegheny county fatal accidental overdoses"),
  
  sidebarLayout(
    sidebarPanel (
      helpText("Data source: WRPDC "),
      helpText("Fatal accidental overdose incidents in Allegheny County, denoting age, gender, race, drugs present, zip code of incident and zip code of residence"),
      selectInput(inputId = "z1",
                  label = "coloring:",
                  choices = c("Sex","Race"),
                  selected = "Sex"),
      sliderInput(
        inputId = "size1",
        label = "Size1:",
        min = 0,max = 3,
        value= 2),
      
      checkboxGroupInput(
        inputId = "selected_sex",
        label="selected Male or Female",
        choices = c("Male","Female"),
        selected = "Male"
      ),
      checkboxGroupInput(
        inputId = "selected_race",
        label="selected race",
        choices = c("White","Black or African American","Asian","Hispanic"),
        selected = "White"
      ),
      
      hr(),
      
      selectInput("selection","choose the nth drug:",choices = c("CombinedOD1","CombinedOD2","CombinedOD3","CombinedOD4","CombinedOD5","CombinedOD6","CombinedOD7","CombinedOD8","CombinedOD9")),
      actionButton("update", "Change"),
      radioButtons(inputId = "image_type",label ="select the image type",choices = list("png","jpg"))
      
      
    ),
    mainPanel (
      plotOutput(outputId = "scatterplot"),
      br(),br(),
      plotOutput(outputId = "scatterplot2"),
      br(),br(),
      plotOutput("word_cloud"),
      downloadButton(outputId = "down",label = "download plot"),
      br(),br(),
      # uiOutput(outputId = "n"),
      DT::dataTableOutput(outputId = "overdosetable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  fataldrug_subset <- reactive({
    fataldrug <- fataldrug.load
    if (length(input$selected_sex) > 0) {
      fataldrug <- filter(fataldrug, Sex %in% input$selected_sex) 
    }
    if (length(input$selected_race) > 0) {
      fataldrug <- filter(fataldrug, Race %in% input$selected_race )
    }
    fataldrug
  })
  
    
  output$scatterplot <- renderPlot({
     ggplot(data = fataldrug_subset(), aes_string(x="Age", y= "drug_num",color = input$z1))+
     geom_point(size = input$size1) 
})
  
  output$scatterplot2 <- renderPlot({
    ggplot(data =fataldrug_subset(), aes_string(x="IncidentZip", y=  "DecedentZip")) +
      geom_point(size = input$size1) +ylim(15000,15400)+xlim(15000,15400)+geom_abline(intercept = 0,slope = 1,color = "dark blue")
  
  })
  
  observe({update_geom_defaults("point",list(color = "red"))})

  terms <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$word_cloud <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = 1, max.words=Inf,
                  colors=brewer.pal(8, "Dark2"))
  })

  output$down <- downloadHandler(
    filename = function(){
               paste("overdoses",input$image_type,sep = ".")
           },
           content = function(cloud){
               if(input$image_type =="png")
                   png(cloud)
               else
                   jpg(cloud)
             file.copy(word_cloud(),cloud)
    })


  output$overdosetable <- DT::renderDataTable({
    DT::datatable(data = fataldrug_subset(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
}

shinyApp(ui, server)