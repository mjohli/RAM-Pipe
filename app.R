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

#Read Questions.CSV containing the questions
#Variables currently in Questions.CSV:
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
#Variables possible (that I know of):
#page:             If we want more than one page, we can specify on which page the question
#                  should be.
dat <- read.csv2("Questions.CSV")

#Setting the current year as a proposition. Right now this is grey and not 'written' into the
#input field. In my opinion there are two possibilities how to deal with this: Finding out how
#to put an actual value into an input field or finding out how to color the year black and setting
#the year variable in the output-file to the current year if it's left empty. Also for year it would
#be of interest if we can define minimum and maximum input values. This should at least be possible
#using if-statements inside the ui and/or server.
dat$option[dat$question == "Jahr"] <- format(Sys.Date(), "%Y")

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
    surveyOutput(df = dat,
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
#I guess that the translation with deepl will take place in the server part. Titles should
#be translated from german to english if they are not titles of a thesis (Bachelor, Master
#, ...)

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

# Ende
