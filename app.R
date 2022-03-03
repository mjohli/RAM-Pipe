library(shiny)
library(shinysurveys)
library(readxl)

q1 <- data.frame(question = "What is your favorite food?",
                 option = "Your Answer",
                 input_type = "text",
                 input_id = "favorite_food",
                 dependence = NA,
                 dependence_value = NA,
                 required = F)
q2 <- data.frame(question = "What is your  food?",
                 option = "Your Answer",
                 input_type = "text",
                 input_id = "favorite",
                 dependence = NA,
                 dependence_value = NA,
                 required = F)
df <- rbind(q1, q2)

ui <- fluidPage(
    surveyOutput(df = df,
                 survey_title = "Hello, World!",
                 survey_description = "Welcome! This is a demo survey showing off the {shinysurveys} package."),
    htmlOutput("text")
)

server <- function(input, output, session) {
    renderSurvey()
    output$text <- renderText({ 
      input$favorite 
    })
    observeEvent(input$submit, {
        showModal(modalDialog(
            title = "Congrats, you completed your first shinysurvey!",
            "You can customize what actions happen when a user finishes a survey using input$submit."
        ))
    })
}

shinyApp(ui, server)