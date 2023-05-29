if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,httr, jsonlite,shinythemes)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Code Translator"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("api_key","API Key",""),
      selectInput("input_language", "Input Language:",
                  choices = c("","R", "SAS", "SQL", "SPSS", "Stata", "Python", "Other"),
                  selected=""),
      conditionalPanel(
        condition = "input.input_language == 'Other'",
        textInput("other_input_language", "Specify Language")
      ),
      
      selectInput("output_language", "Output Language:",
                  choices = c("","R", "SAS", "SQL", "SPSS", "Stata", "Python", "Other"),
                  selected=""),
      conditionalPanel(
        condition = "input.output_language == 'Other'",
        textInput("other_output_language", "Specify Language")
      ),
      
      textAreaInput("comments", "Additional Comments:",  placeholder = "Example: make use of the tidyverse family of functions."),
      
      textAreaInput("original_code", "Original Code:", "", width = '100%', height = '400px'),
      
      actionButton("translate", "Translate"),
      
      downloadButton("download", "Download Output")
    ),
    
    mainPanel(
      textAreaInput("translation", "Translated Code:", "", width = '100%', height = '400px')
    )
  )
)


server <- function(input, output,session) {
  
  
  gpt_code_translate<-function( api_key, model, temperature, 
                                input_language,output_language,additional_comments, original_code){
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)), content_type_json(), encode = "json",
      body = list(
        model = model,
        messages = list(list(role = "system",
                             content = "Vous êtes un traducteur de différent languages de logiciel statistiques. Je fournis une syntaxe et vous ressortez la traduction. Ajouter des commentaires à l'intérieur de la syntaxe pour s'orienter. "),
                        list(role = "user",
                             content =  paste("Voici une syntaxe dans le language ",
                                              input_language,
                                              "\n Traduit le en ",
                                              output_language,
                                              "\n",
                                              additional_comments,
                                              ":\n",
                                              original_code))
        ),
        temperature = temperature))
    
    unlist(content(response)$choices[[1]]$message$content)
  }
  
  observeEvent(input$translate, {
    input_language <- ifelse(input$input_language == "Other", input$other_input_language, input$input_language)
    output_language <- ifelse(input$output_language == "Other", input$other_output_language, input$output_language)
    
    translation <- gpt_code_translate(api_key = input$api_key, 
                                      model = "gpt-4", 
                                      temperature = 0, 
                                      input_language = input_language,
                                      output_language = output_language,
                                      additional_comments = input$comments,
                                      original_code =  gsub("\"", "\\\"", input$original_code, fixed = TRUE))
    
    updateTextAreaInput(session, "translation", value = translation)
  })
  
  
  output$download <- downloadHandler(
    filename = function() {
      paste(input$output_language, "_translation",Sys.time(),".txt", sep = "")
    },
    
    content = function(file) {
      writeLines(input$translation, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
