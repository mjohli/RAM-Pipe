#1     Preparations####

#1-1   Import libraries####

#1-1-1 Define library function####

#First define a function that loads a package into the library and installs it
#beforehand if the package is not yet installed.

#pck: The name of the desired package.
libr <- function(pck){
  if(!require(pck, character.only = T)){
    install.packages(pck)
    library(pck, character.only = T)
  }
}

#1-1-2 Import libraries####

#Load and install, if necessary, the packages needed.
libr("shiny")        #Necessary for shiny itself.
libr("shinysurveys") #Necessary for the survey part.
libr("readxl")       #Necessary for reading the excel-File we want to append the results
                     #of the survey to.
libr("writexl")      #Will probably be necessary for writing the excel-File in the end.

#1-2   Import data####

#1-2-1 Survey data####

#This right now is not very neat and ugly. the survey takes its input (questions etc.) 
#from a data.frame. Here two data.frames are created manually, but I think it would be
#easier and more beautiful to just read a .csv or similar containing the parameters as
#variables, as proposed in Questions.xlsx.
#question:         The question that appears in the survey.
#option:           Not exactly sure what this does, in text-inputs it appears to be the 
#                  text that is in the input field in the beginning.
#input_type:       This seems to define if the question is numeric, text, or another type
#                  (I guess that multiple-choice is possible). Not sure what is possible
#                  here.
#input_id          This is the name of the variable the answer to this question will be
#                  saved. It can be obtained in the server-part using input$input_id.
#dependence:       This is kind of a connection to other questions like "show this question if
#                  another question was answered yes" but not sure how it works
#dependence_value: This is also for the connection to other questions. Maybe dependence
#                  defines which question and dependence_value which value is needed.
#required:         This specifies if the question must be answered, not sure what happens
#                  if the question is not answered. Maybe a warning.

#Create a data.frame q1 with one question in it.
q1 <- data.frame(question = "Vorname",
                 option = "First Name",
                 input_type = "text",
                 input_id = "first_name",
                 dependence = NA,
                 dependence_value = NA,
                 required = T)
#Create a data.frame q2 with another question in it.
q2 <- data.frame(question = "Nachname",
                 option = "Last Name",
                 input_type = "text",
                 input_id = "last_name",
                 dependence = NA,
                 dependence_value = NA,
                 required = T)
q3 <- data.frame(question = "Studium/Typ der Arbeit",
                 option = "Study/Job Type",
                 input_type = "text",
                 input_id = "study_job",
                 dependence = NA,
                 dependence_value = NA,
                 required = T)
q4 <- data.frame(question = "Fachbereich",
                 option = "Subject",
                 input_type = "text",
                 input_id = "subject",
                 dependence = NA,
                 dependence_value = NA,
                 required = T)
q5 <- data.frame(question = "Erst Betreuer/in",
                 option = "Main Supervisor",
                 input_type = "text",
                 input_id = "main_supervisor",
                 dependence = NA,
                 dependence_value = NA,
                 required = T)
q6 <- data.frame(question = "Sind Sie RAM-Mitglieder?",
                 option = c("Ja","Nein"),
                 input_type = "y/n",
                 input_id = "membeship",
                 dependence = NA,
                 dependence_value = NA,
                 required = T)
q7 <- data.frame(question = "Was ist der Preis bzw. FÃ¶rdungersart, den Sie beantragen mÃ¶chten?",
                 option = c("TeilfÃ¶rderung","Otto-Selz Preis","FÃ¶rderung","RAT-Preis","RAM-Preis"),
                 input_type = "mc",
                 input_id = "prize_subsidary_type",
                 dependence = NA,
                 dependence_value = NA,
                 required = T)
q8 <- data.frame(question = "Bitte beschreiben Sie den Zweck der FÃ¶rderung:",
                 option = "Please describe the purpose of this prize or subsidary",
                 input_type = "text",
                 input_id = "purpose",
                 dependence = NA,
                 dependence_value = NA,
                 required = T)
q9 <- data.frame(question = "Preisgeld bzw. FÃ¶rderungsbetrag (â¬):",
                 option = "Amount of prize or subsidary (â¬)",
                 input_type = "text",
                 input_id = "amount",
                 dependence = NA,
                 dependence_value = NA,
                 required = T)

#Put both data.frames together into one data.frame df
df <- rbind(q1, q2, q3, q4, q5, q6, q7, q8, q9)

#1-2-2 Table####

#Here, I think, we should load the data from the server to append the answers to later.

#2     Shiny-App####

#Now the fun begins. The shiny-app consists out of two parts: the user-interface, where we
#define how the things the user sees look like and behave

#2-1   Page####

#fluidPage generates the user-interface
ui <- fluidPage(
  
    #surveyOutput creates a questionnaire.
    #df:                 The data.frame containing the questions.
    #survey_title:       Should be shown at the top of the page
    #survey_description: Should be shown underneath the title but not sure.
    surveyOutput(df = df,
                 survey_title = "Hello, World!",
                 survey_description = "Welcome! This is a demo survey showing off the {shinysurveys} package."),
    
    #This generates an output which is specified as output$text in the server-part.
    #In general that's how you create reactive output. Define it as output$xy in the
    #server part and put an appropriate output function into the ui-part (there's e.g.,
    #tableOutput, plotOutput, ...). Also note that in the ui-part the outputs are
    #separated with commas.
    htmlOutput("text")
)

#2-2   Server####

#I don't really know why server needs input, output, and session as parameters but it
#seems to work like this.
server <- function(input, output, session) {
    
    #I think this is necessary for the survey but I have no clue (yet) what it does.
    renderSurvey()
  
    #This takes the text written in the question with question_id favorite and puts it
    #into output$text to display it in the user interface. This is just a proof of concept
    #and to have something where one can see how this works and what is necessary a) to
    #use input inside the ui and b) to access the input from the questionnaire to append
    #to the data later.
    output$text <- renderText({ 
      input$favorite 
    })
    
    #This reacts to a press on the (I guess automatically with SurveyOutput generated)
    #submit-button by displaying a dialogue window containing text. In general one can
    #react to things happening with user input using observeEvent()
    observeEvent(input$submit, {
        showModal(modalDialog(
            title = "Congrats, you completed your first shinysurvey!",
            "You can customize what actions happen when a user finishes a survey using input$submit."
        ))
    })
}

#3     Start the app####

#This starts the app using the previously defined ui and server.
shinyApp(ui, server)
