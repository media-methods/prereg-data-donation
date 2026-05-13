#install.packages('rsconnect')

library(rsconnect)
library(shiny)
library(quarto)
library(bslib)
library(lubridate)
library(tinytex)

# for deployment, just push changes. I will deploy it later.
#But you can also do it, just remove the comment after all of the code changes
# and run the command
# I will do auto deployment later
# rsconnect::deployApp() 
#in the console

 

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
       list(
         "No License" = "No License",
         "Academic Free License 3.0" = "Academic Free License 3.0",
         "Apache License 2.0" = "Apache License 2.0",
         "Artistic License 2.0" = "Artistic License 2.0",
         "BSD 2-Clause \"Simplified\" License" = "BSD 2-Clause \"Simplified\" License",
         "BSD 3-Clause \"New/Revised\" License" = "BSD 3-Clause \"New/Revised\" License",
         "CC-By Attribution 4.0 International" = "CC-By Attribution 4.0 International",
         "CC0 1.0 Universal" = "CC0 1.0 Universal",
         "CC-By Attribution-NonCommercial-NoDerivatives 4.0 International" = "CC-By Attribution-NonCommercial-NoDerivatives 4.0 International",
         "CC-By Attribution-ShareAlike 4.0 International" = "CC-By Attribution-ShareAlike 4.0 International",
         "Eclipse Public License 1.0" = "Eclipse Public License 1.0",
         "GNU General Public License (GPL) 2.0" = "GNU General Public License (GPL) 2.0",
         "GNU General Public License (GPL) 3.0" = "GNU General Public License (GPL) 3.0",
         "GNU Lesser General Public License (LGPL) 2.1" = "GNU Lesser General Public License (LGPL) 2.1",
         "GNU Lesser General Public License (LGPL) 3.0" = "GNU Lesser General Public License (LGPL) 3.0",
         "MIT License" = "MIT License",
         "Mozilla Public License 2.0" = "Mozilla Public License 2.0"
       ) 
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

