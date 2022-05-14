#0 Clear R's memory
rm(list=ls())
options(encoding = 'UTF-8')
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
libr("deeplr")       # necessacry for title translation; authentification-key needed
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
key <- read.csv("auth-key", header = F)$V1

#Setting the current year as a proposition. Right now this is grey and not 'written' into the
#input field. In my opinion there are two possibilities how to deal with this: Finding out how
#to put an actual value into an input field or finding out how to color the year black and setting
#the year variable in the output-file to the current year if it's left empty. Also for year it would
#be of interest if we can define minimum and maximum input values. This should at least be possible
#using if-statements inside the ui and/or server.
dat$option[dat$question == ""] <- format(Sys.Date(), "%Y")

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
      
### Deepl Implementation and hard-code translations ###
      
      # duplicate response vector from responsedata-dataframe. Translations will be stored in each of
      # these vectors depending on the origin language indicated. If the origin language is English, the 
      # German responses will be translated and stored in the response vector. You can see that when there
      # are two if-statements depending on origin language.
      
      eng_response <- response_data
      germ_response <- response_data
      
#title, location, country (Note: location translation does not really work)      
      if (response_data$language == "de"){
        
        # translate title,location and country to English 
        eng_response$title <- toEnglish2(response_data$title, auth_key = key)
        eng_response$location <- toEnglish2(response_data$location, auth_key = key)
        eng_response$country <- toEnglish2(response_data$country, auth_key = key)
        
        # change langugage type to 'en'
        eng_response$language <- "en"
        
      }else if( response_data$language == "en"){
        # translate title, location and country to German
        germ_response$title <- toGerman2(response_data$title, auth_key = key)
        germ_response$location <- toGerman2(response_data$location, auth_key = key)
        germ_response$country <- toGerman2(response_data$country, auth_key = key)
        
        # change langugage type to 'de'
        germ_response$language <- "de"
        
      }else{
        # error
        print("Error: You must indicate language type")
      }
 
##NOTE: For each variable there will be two if-statements depending on the origin language.
      # That is necessary to translate German responses, although language type is English due to 
      # the fact, that the survey is in German and some responses have to be translated to English
      # They will be then stored in the baseline-vector, the response_data vector.
      
      #IMPORTANT: Translation of type must be after translation of prize, or you have to change 'Preis'
      # 'award' in the prize section, otherwise it won't translate it.
      
#level_de ACHTUNG: Sonstige Angabe wird hier aktuell noch nicht übersetzt!
      if(response_data$language == "de" & response_data$level == "Diplom"){
        eng_response$level <- "Diploma"
      }else{}
      
#level_en ACHTUNG: Sonstige Angabe wird hier aktuell noch nicht übersetzt!
      if(response_data$language == "en" & response_data$level == "Diplom"){
        response_data$level <- "Diploma"
      }else{}
      
      
#major_de ACHTUNG: Sonstige Angabe wird hier aktuell noch nicht übersetzt!
     if (response_data$language == "de" && response_data$major == "Soziologie"){
       eng_response$major <- "Sociology"
     }else if(response_data$language == "de" && response_data$major == "Politikwissenschaften"){
       eng_response$major <- "Political Science"
     }else if(response_data$language == "de" && response_data$major == "Psychologie"){
       eng_response$major <- "Psycology"
     }else if(response_data$language == "de" && response_data$major == "Sozialwissenschaften"){
       eng_response$major <- "Social Science"
     }else if(response_data$language == "de" && response_data$major == "Erziehungswissenschaften"){
       eng_response$major <- "Educational Studies"
     }else if(response_data$language == "de" && response_data$major == "CDSS"){
       eng_response$major <- "CDSS"
     }else{}
      
#major_en ACHTUNG: Sonstige Angabe wird hier aktuell noch nicht übersetzt!      
      if (response_data$language == "en" && response_data$major == "Soziologie"){
        response_data$major <- "Sociology"
      }else if(response_data$language == "en" && response_data$major == "Politikwissenschaften"){
        response_data$major <- "Political Science"
      }else if(response_data$language == "en" && response_data$major == "Psychologie"){
        response_data$major <- "Psychology"
      }else if(response_data$language == "en" && response_data$major == "Sozialwissenschaften"){
        response_data$major <- "Social Science"
      }else if(response_data$language == "en" && response_data$major == "Erziehungswissenschaften"){
        response_data$major <- "Educational Studies"
      }else if(response_data$language == "en" && response_data$major == "CDSS"){
        response_data$major <- "CDSS"
      }else{}
      

#prize_de ACHTUNG: Sonstige Angabe wird hier aktuell noch nicht übersetzt!
      if (response_data$language == "de" & response_data$type == "Preis" & response_data$prize == "Franz-Urban-Pappi-Preis"){
        eng_response$prize <- "Franz-Urban-Pappi-Award"
        
      }else if(response_data$language == "de" & response_data$type == "Preis" & response_data$prize == "Otto-Selz-Preis"){
        eng_response$prize <- "Otto-Selz-Award"
        
      }else if(response_data$language == "de" & response_data$type == "Preis" & response_data$prize == "Hans-Albert-Preis"){
        eng_response$prize <- "Hans-Albert-Award"
        
      }else if(response_data$language == "de" & response_data$type == "Preis" & response_data$prize == "RAM-Preis"){
        eng_response$prize <- "RAM-Award"
        
      }else if(response_data$language == "de" & response_data$type == "Preis" & response_data$prize == "RAT-Preis"){
        eng_response$prize <- "RAT-Award"
        
      }else{}
      
#prize_en ACHTUNG: Sonstige Angabe wird hier aktuell noch nicht übersetzt!
      if (response_data$language == "en" & response_data$type == "Preis" & response_data$prize == "Franz-Urban-Pappi-Preis"){
        response_data$prize <- "Franz-Urban-Pappi-Award"
        
      }else if(response_data$language == "en" & response_data$type == "Preis" & response_data$prize == "Otto-Selz-Preis"){
        response_data$prize <- "Otto-Selz-Award"
        
      }else if(response_data$language == "en" & response_data$type == "Preis" & response_data$prize == "Hans-Albert-Preis"){
        response_data$prize <- "Hans-Albert-Award"
        
      }else if(response_data$language == "en" & response_data$type == "Preis" & response_data$prize == "RAM-Preis"){
        response_data$prize <- "RAM-Award"
        
      }else if(response_data$language == "en" & response_data$type == "Preis" & response_data$prize == "RAT-Preis"){
        response_data$prize <- "RAT-Award"
        
      }else{}
      
      
#type_de
      if (response_data$language == "de" && response_data$type == "Förderung"){
        eng_response$type <- "funding"
      }else if(response_data$language == "de" && response_data$type == "Preis"){
        eng_response$type <- "award"
      }else{} 
      
#type_en
      if (response_data$language == "en" && response_data$type == "Förderung"){
        response_data$type <- "funding"
      }else if(response_data$language == "en" && response_data$type == "Preis"){
        response_data$type <- "award"
      }else{}
      
      
#funding_type_de
      if (response_data$language == "de" & response_data$funding_type == "Teilförderung"){
        eng_response$funding_type <- "Partial funding"
      }else if(response_data$language == "de" & response_data$funding_type == "Förderung"){
        eng_response$funding_type <- "funding"
      }else{}
      
#funding_type_en
      if (response_data$language == "en" & response_data$funding_type == "Teilförderung"){
        response_data$funding_type <- "Partial funding"
      }else if(response_data$language == "en" & response_data$funding_type == "Förderung"){
        response_data$funding_type <- "funding"
      }else{}  
      
## merge translated responses to responsedata-dataframe
      
      if(response_data$language == "de"){
        response_data <- rbind(response_data, eng_response)
      }else if(response_data$language == "en"){
        response_data <- rbind(response_data, germ_response)
      }else{print("ERROR: You must indicate language!")}
      
### Deepl Implementation end ###  
      
      
      #order data so it fits the excel
      col_order <- c("year", "surname", "name",
                     "level", "major", "supervisor", 
                     'prize', 'title', 'language', 'type',
                     'funding_type', 'location', 'country')
      #apply order
      response_data <- response_data[, col_order]
      
      #read latest excel in 
      #part below only works when the first excel file was written - delete ### after first use
        old_data <-   list.files( pattern = '20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9]{2}-[0-9]{2}.xlsx') %>%
           map_df(~read_xlsx(.))
        
      # following line as comment after first use
    #  old_data <- read_xlsx("funding_overview_all.xlsx")
        
      #(over)write archive version
      write_xlsx(old_data, 'funding_overview_all_alt.xlsx')
      #merge data
      merged <- rbind(old_data, response_data)
      #write new excel with today's date
      write_xlsx(merged, paste0('funding_overview_all_', format(Sys.time(), "%Y-%m-%d_%H-%M"),'.xlsx'))
      showModal(modalDialog(
            title = "Daten gespeichert.
            WICHTIG: Vor der nächsten Ausführung Excel-File schließen, falls offen! 
            Du kannst dieses Fenster jetzt schließen. Vielen Dank für die Benutzung! "
        ))
    })
}

#3     Start the app####

#This starts the app using the previously defined ui and server.
shinyApp(ui, server)

# Ende
