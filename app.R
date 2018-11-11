

library(shiny)
library(shinyjs) 
library(tesseract)
library(stringr)

ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Image File",
                accept = c(
                  ".jpg",
                  ".png",
                  ".jpeg")
      ),
      div(htmlOutput("Error"), style = "text-align:left;color:red"),
      div(htmlOutput("Completed"), style = "text-align:left;color:green"),
      actionButton("scrapeText", "Scrape Text"),
      actionButton('reset', 'Reset'),
      hr(),
      downloadButton('download',"Download"),
      actionButton("exit", "Exit") 
    ),
    
    mainPanel(
      h3("Image File",align="center"),
      imageOutput("myImage",height = "auto"),
      h3("Scrape Text from Image",align="center"),
      div(htmlOutput("contents"), style = "text-align:left")
    )
  ) 
  
  
)

server <- function(input, output,session){
  
  
  shinyjs::disable(id="download")
  
  v <- reactiveValues(
    data = NULL,
    clear = FALSE
  )
  
  
  
  logicOCR <- function(inFile){
    language <- tesseract("eng")
    text <- tesseract::ocr(inFile$datapath, engine = language)
    v$data=str_replace_all(text,"\n","<br>")
    shinyjs::enable(id="download")
  }
  
  observeEvent(input$scrapeText, {
    inFile <- input$file1
    path <- inFile$datapath
    path <- str_split(path,"\\.")
    
    if (is.null(inFile))
    {
      output$Error <- renderUI(
        return("* Please Choose a File")
      )
    }
    else if((path[[1]][-1] != "png") && (path[[1]][-1] != "jpg") && (path[[1]][-1] != "jpeg"))
    {
      output$Error <- renderUI(
        return("* Please Choose a .jpg/.jpeg/.png File")
      )
    }
    else
    {
      output$Error <- renderUI(
        return("")
      )
      output$myImage <- renderImage({
        inFile <- input$file1
        path <- inFile$datapath
        list(src = path,
             #contentType = c( 'jpg'),
             width = 400,
             height = 300)
      }, deleteFile = FALSE)
      
      output$Completed <- renderUI(
        return("Scrapping Completed")
      )
      logicOCR(inFile)
      
      
    }
  })
  
  observeEvent(input$exit, {
    stopApp()
  })
  
  observeEvent(input$reset, {
    v$data <- NULL
    reset('file1')
    inFile <- input$file1
    infFile <- NULL
    output$Error <- renderUI(
      return("")
    )
    output$Completed <- renderUI(
      return("")
    )
    output$myImage <- renderUI(
      return("")
    )
    shinyjs::disable(id="download") 
  })
  
  output$contents <- renderUI({
    
    HTML(v$data)
      
  }
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      print(v$data)
      writeLines(v$data, file)
    }
  )
}

shinyApp(ui, server)

