library(shiny)
library(quarto)
library(bslib)
library(lubridate)

ui <- page_fluid(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "cerulean"
  ),
  
  titlePanel("PDF Generator"),
  
  card(
    
    card_header("Enter Information"),
    #Title
    textInput("title", "Title"),
    #Authors
    textInput("authors", "Authors"),
    #date
    dateInput(inputId = "date", label = ("Date of Preregistration"), value = Sys.Date()),
    #Versioninput
    
    #CODE CODE CODE
    
    #License
     selectInput( 
       inputId = "license", 
       "Select options below:", 
       list("TEST1" = "TEST1", "TEST2" = "TEST2", "TEST3" = "TEST3") 
     ), 
    
    #textAreaInput("authors", "Authors", rows = 6),
    
    br(),
    
    downloadButton("download_pdf", "Download PDF")
  )
)

server <- function(input, output, session) {
  
  output$download_pdf <- downloadHandler(
    
    filename = function() {
      "Preregistration.pdf"
    },
    
    content = function(file) {
      
      temp_dir <- tempdir()
      temp_qmd <- file.path(temp_dir, "Preregistration.qmd")
      
      file.copy("Preregistration.qmd", temp_qmd, overwrite = TRUE)
      
      quarto::quarto_render(
        input = temp_qmd,
        output_file = "Preregistration.pdf",
        execute_params = list(
          title = input$title,
          authors = input$authors,
          date = input$date,
          license = input$license
          
        )
      )
      
      pdf_path <- file.path(temp_dir, "Preregistration.pdf")
      
      file.copy(pdf_path, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)

