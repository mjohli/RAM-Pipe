#0 Clear R's memory
rm(list=ls())

getwd() 			           # shows you the present working directory
dir()                    # shows you what in the wd is

#1     Preparations####

#1-1   Import libraries####

#1-1-1 Import libraries####

# libr: a function to automatically install and open libraries
#pck: The name of the desired package.
libr <- function(pck){
  if(!require(pck, character.only = T)){
    install.packages(pck)
    library(pck, character.only = T)
  }
}

#Load and install, if necessary, the packages needed.
libr("shiny")        #Necessary for shiny itself.
libr("shinysurveys") #Necessary for the survey part.
libr("readxl")       #Necessary for reading the excel-File we want to append the results
                     #of the survey to.
libr("writexl")      #Will probably be necessary for writing the excel-File in the end.
libr("tidyverse")    #necessary for data wrangling
libr("deepl")       # necessacry for title translation; authentification-key needed
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
#define how the things the user sees look like and behave.

#2-1   Page####

# We are using the shiny package to generate the user-interface. "fluidPage" generates the user-interface.

#Hex codes (color codes) from RAM:
#  #ffffff - white
#  #759dbd - light blue
#  #043a6f - dark blue
#  #d7e0e7 - also white lol?
#  #0a477a - dark, 'neon' blue

ui <- fluidPage(
  ## Here we define the header of the survey.
  tags$div(class="h1", checked=NA, align ="center",
           tags$p("Verwaltungshilfe-Tool", style = "color:#ffffff;
                                                    font-family: Calibri;
                                                    font-size:40px")),
  
  ## Defining the description/subheading.
  tags$div(class="h2", checked=NA, align ="center",
           tags$p("RAM-Förderungen / - Preisvergabe", style = "color:#ffffff;
                                                               font-family: Calibri;
                                                               font-size:25px")),
  
  ## Changing the survey area sub-background & questions.
  ## style = background-color: color of sub-background
  ##                    color: color of the question-text
  ##                       ff: Question-text font
  ##                       fs: Question font size, Error!
  tags$div(dataTableOutput("dat"), style = "background-color:#ffffff;      
                                            color:#0a477a;                 
                                            font-family: Arial;            
                                            font-size: 30px",              
           
           # Create a questionnaire with "surveyOutput".
           #df:                 The data.frame containing the questions.
           #survey_title:       Should be shown at the top of the page.
           #survey_description: Should be shown underneath the title.
           surveyOutput(df = dat, # survey_title = "Hallo RAM!",
                        # survey_description = "Hier könnt Ihr die Infos für RAM-Förderungen und RAM-Preise eintragen",
                        theme = "#759dbd",                                                                               
                        style = "color: #0a477a;                                                                         
                                  font-family: Calibri;
                                  font-size:20px,"
           )
  )
)

 ## style = color: Defining page background color
 ##         ff: Defining submit-button, color white does not appear?


#2-2   Server####

#I don't really know why server needs input, output, and session as parameters but it
#seems to work like this.
#I guess that the translation with deepl will take place in the server part. Titles should
#be translated from german to english if they are not titles of a thesis (Bachelor, Master
#, ...)

server <- function(input, output, session) {
    
    #I think this is necessary for the survey but I have no clue (yet) what it does.
    renderSurvey()
  
    #This reacts to a press on the (I guess automatically with SurveyOutput generated)
    #submit-button by displaying a dialogue window containing text. In general one can
    #react to things happening with user input using observeEvent()
    observeEvent(input$submit, {
      #load survey data in
      response_data <- getSurveyData() %>% 
        #select relevant variables
        select(question_id, response) %>% 
        #to wide format
        pivot_wider(names_from = "question_id" ,
                    values_from = "response") 
      #shape data so it fits the excel:
      #we often have two columns where only one column exists in the excel 
      #Since one of those two columns (e.g.: level of education, level of education alternative)
      #is always empty, the alternative column gets deleted. If the alternative column (level of education alternative: Dr. med)
      #includes data, the data gets transferred to the first column (level of education: HIDDEN_QUESTION) as it must be empty.
      if (response_data$levelAlt == 'HIDDEN-QUESTION') {
        response_data$levelAlt <- NULL
      } else {
        response_data$level <- response_data$levelAlt
        response_data$levelAlt <- NULL
      }
      if (response_data$majorAlt == 'HIDDEN-QUESTION') {
        response_data$majorAlt <- NULL
      } else {
        response_data$major <- response_data$majorAlt
        response_data$majorAlt <- NULL
      }
      if (response_data$prizeAlt == 'HIDDEN-QUESTION') {
        response_data$prizeAlt <- NULL
      } else {
        response_data$prize <- response_data$prizeAlt
        response_data$prizeAlt <- NULL
      }
      if (response_data$prize == 'HIDDEN-QUESTION') {
        response_data$prize <- ''
      }
      if (response_data$funding_type == 'HIDDEN-QUESTION') {
        response_data$funding_type <- ''
      }
      
    ### Deepl Implementation
      
      if (responsedata$language == "de"){
        # duplicate response vector from responsedata-dataframe
        eng_response <- responsedata
        
        # translate title of last row to english
        eng_response$title <- toEnglish(eng_response$title)
        
        # change langugage type to 'en'
        eng_response$language <- "en"
        
        # merge english response to responsedata-dataframe
        responsedata <- rbind(responsedata, eng_response)
        
      }else if( responsedata$language == "en"){
        # do nothing
      }else{
        # error
        print("Error: You must indicate language type")
      }
      
    ### Deepl Implementation end  
      
      
      #order data so it fits the excel
      col_order <- c("year", "surname", "name",
                     "level", "major", "supervisor", 
                     'prize', 'title', 'language', 'type',
                     'funding_type', 'location', 'country')
      #apply order
      response_data <- response_data[, col_order]
      
      #read latest excel in 
      #part below only works when the first excel file was written - delete ### after first use
        # old_data <-   list.files( pattern = '20[0-9][0-9]-[0-9][0-9]-[0-9][0-9].xlsx') %>%
        #   map_df(~read_xlsx(.))
        
      # following line as comment after first use
      old_data <- read_xlsx("funding_overview_all.xlsx")
        
      #(over)write archive version
      write_xlsx(old_data, 'funding_overview_all_alt.xlsx')
      #merge data
      merged <- rbind(old_data, response_data)
      #write new excel with today's date
      write_xlsx(merged, paste0('funding_overview_all_',Sys.Date() ,'.xlsx'))
      showModal(modalDialog(
            title = "Thanks for using this tool! You can now find your updated excelfile in
            your directory. You can close this window now"
        ))
    })
}

#3     Start the app####

#This starts the app using the previously defined ui and server.
shinyApp(ui, server)

# Ende
